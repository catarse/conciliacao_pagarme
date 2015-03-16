{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pagarme
import Network.Wreq
import Data.HashMap.Strict ((!), member)
import qualified Data.Map as M

import Database.HDBC 
import Database.HDBC.PostgreSQL

import Data.Scientific
import Data.Aeson

type DbResult = M.Map String SqlValue

usingKey :: Options -> Options
usingKey = withApiKey ""

jsonToSql :: Value -> SqlValue
jsonToSql = toSql . jsonToString

jsonToString :: Value -> String
jsonToString value = (show . coefficient) number
  where Number number = value

getConfirmedTransactionsPage :: Int -> IO [Object]
getConfirmedTransactionsPage page = do
  putStrLn $ "Fetching page " ++ show page
  getTransactions $ (byPage page . usingKey) defaults

verifyTransactionsWith :: (Object -> IO ()) -> Int -> IO ()
verifyTransactionsWith verifyFunction page = do
  toVerify <- getConfirmedTransactionsPage page
  _ <- mapM_ verifyFunction toVerify
  case toVerify of
       []   -> putStrLn "Finish verification"
       _ -> verifyTransactionsWith verifyFunction (page + 1)

shouldUpdateDb transaction Nothing = pagarmeState /= "waiting_payment"
  where String pagarmeState = transaction ! "status"
shouldUpdateDb transaction (Just dbRecord) = (dbState /= "confirmed" && pagarmeState == "paid") 
                                             || (dbState /= "refunded" && dbState /= "requested_refund" && pagarmeState == "refunded")
  where SqlByteString dbState = dbRecord M.! "state"
        String pagarmeState = transaction ! "status"

main :: IO ()
main = do
  con <- connectPostgreSQL ""
  select <- prepare con "SELECT * FROM contributions WHERE payment_id = ?;"
  selectByKey <- prepare con "SELECT * FROM contributions WHERE key = ?;"
  selectByCustomer <- prepare con "SELECT * FROM contributions WHERE value::int = ? AND payer_email = ?;"

  let 
    returnHint transaction id = return (show id, paymentId, show pagarmeState)
      where String pagarmeState = transaction ! "status"
            paymentId = jsonToString $ transaction ! "id"

    findUpdateHintWithMetadata key transaction = do
      _ <- execute selectByKey [toSql key]
      result <- fetchRowMap selectByKey
      case result of
           Just record -> putStrLn "Found by key!!!" >> returnHint transaction (record M.! "id")
           Nothing -> putStrLn ("Missing key " ++ show key) >> findUpdateHintWithCustomer transaction
      

    findUpdateHintWithCustomer transaction = do 
      let Number value = transaction ! "amount"
          v = coefficient $ value / 100
          Object customer = transaction ! "customer"
          String email = customer ! "email"
      _ <- execute selectByCustomer [nToSql v, toSql email]
      result <- fetchRowMap selectByCustomer
      case result of
           Just record -> putStrLn "Found by customer!!!" >> returnHint transaction (record M.! "id")
           Nothing -> putStrLn ("Missing key " ++ show email ++ " value " ++ show v) >> returnHint transaction "Missgin key"

    findUpdateHint :: Object -> Maybe DbResult -> IO (String, String, String)
    findUpdateHint transaction Nothing = if member "metadata" transaction
                                            then let Object metadata = transaction ! "metadata" :: Value
                                                  in if member "key" metadata
                                                        then let String key =  (metadata ! "key")
                                                              in findUpdateHintWithMetadata key transaction
                                                        else findUpdateHintWithCustomer transaction
                                            else findUpdateHintWithCustomer transaction
    findUpdateHint transaction (Just dbRecord) = return (show id, paymentId, show pagarmeState)
      where String pagarmeState = transaction ! "status"
            paymentId = jsonToString $ transaction ! "id"
            SqlInteger id = dbRecord M.! "id"

    insertUpdateHint :: Object -> Maybe DbResult -> IO ()
    insertUpdateHint transaction dbRecord = do
      (contributionId, paymentId, state) <- findUpdateHint transaction dbRecord
      run con "INSERT INTO temp.contributions_to_fix (contribution_id, payment_id, pagarme_state) VALUES (?, ?, ?)" (map toSql [contributionId, paymentId, state])
      commit con

    resolveConflicts :: Object -> Maybe DbResult -> IO ()
    resolveConflicts transaction dbRecord =
      if shouldUpdateDb transaction dbRecord
         then insertUpdateHint transaction dbRecord
         else putStrLn "LGTM"

    verifyInDB transaction = do
                 _ <- execute select [jsonToSql (transaction ! "id")]
                 result <- fetchRowMap select
                 resolveConflicts transaction result
                 putStrLn $ "Transaction " ++ show (transaction ! "id") ++ " verified"

    verifyTransactions = verifyTransactionsWith verifyInDB

  verifyTransactions 1
  disconnect con
