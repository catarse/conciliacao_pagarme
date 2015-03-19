{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pagarme
import Network.Wreq
import Data.HashMap.Strict ((!), member)
import qualified Data.Map as M
import Data.Maybe (isJust)

import Database.HDBC 
import Database.HDBC.SqlValue
import Database.HDBC.PostgreSQL

import Data.Scientific
import Data.Aeson
import Data.Text (unpack)

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

equivalentPagarmeState :: String -> String
equivalentPagarmeState dbState 
  | dbState == "confirmed" || dbState == "requested_refund" = "paid" 
  | dbState == "refunded" || dbState == "refunded_and_canceled" = "refunded" 
  | dbState == "canceled" || dbState == "deleted" = "refused" 
  | otherwise = "waiting_payment"

shouldUpdateDb transaction Nothing = pagarmeState /= "waiting_payment"
  where String pagarmeState = transaction ! "status"
shouldUpdateDb transaction (Just dbRecord) = unpack pagarmeState /= equivalentPagarmeState dbState
  where dbState = fromSql $ dbRecord M.! "state" :: String
        String pagarmeState = transaction ! "status"

main :: IO ()
main = do
  con <- connectPostgreSQL ""
  select <- prepare con "SELECT * FROM contributions WHERE payment_id = ?;"
  selectByKey <- prepare con "SELECT * FROM contributions WHERE key = ?;"
  selectByCustomer <- prepare con "SELECT * FROM contributions WHERE value::int = ? AND payer_email = ?;"
  selectExisting <- prepare con "SELECT * FROM temp.contributions_to_fix WHERE payment_id = ?;"

  let 
    transactionAlreadyExists transaction = do
      _ <- execute select [jsonToSql (transaction ! "id")]
      result <- fetchRowMap select
      return $ isJust result


    valueToSql transaction = nToSql v
      where
        Number value = transaction ! "amount"
        v = coefficient $ value / 100

    emailToSql transaction = toSql email
      where
        Object customer = transaction ! "customer"
        String email = customer ! "email"

    keyToSql transaction = if member "metadata" transaction
                              then let Object metadata = transaction ! "metadata" :: Value
                                    in if member "key" metadata
                                          then let String key =  (metadata ! "key")
                                                in toSql key
                                          else SqlNull
                              else SqlNull

    returnHint transaction id = return (id, toSql paymentId, toSql (unpack pagarmeState), keyToSql transaction, valueToSql transaction, emailToSql transaction)
      where String pagarmeState = transaction ! "status"
            paymentId = jsonToString $ transaction ! "id"

    findUpdateHintWithMetadata key transaction = do
      _ <- execute selectByKey [key]
      result <- fetchRowMap selectByKey
      case result of
           Just record -> putStrLn "Found!!!" >> returnHint transaction (record M.! "id")
           Nothing -> findUpdateHintWithCustomer transaction
      

    findUpdateHintWithCustomer transaction = do 
      _ <- execute selectByCustomer [valueToSql transaction, emailToSql transaction]
      result <- fetchRowMap selectByCustomer
      case result of
           Just record -> putStrLn "Found!!!" >> returnHint transaction (record M.! "id")
           Nothing -> returnHint transaction SqlNull

    findUpdateHint :: Object -> Maybe DbResult -> IO (SqlValue, SqlValue, SqlValue, SqlValue, SqlValue, SqlValue)
    findUpdateHint transaction Nothing = if key == SqlNull
                                            then findUpdateHintWithCustomer transaction
                                            else findUpdateHintWithMetadata key transaction
                                              where key = keyToSql transaction
    findUpdateHint transaction (Just dbRecord) = returnHint transaction (dbRecord M.! "id")

    insertUpdateHint :: Object -> Maybe DbResult -> IO ()
    insertUpdateHint transaction dbRecord = do
      (contributionId, paymentId, state, key, value, payer_email) <- findUpdateHint transaction dbRecord
      print [contributionId, paymentId, state, key, value, payer_email]
      run con "INSERT INTO temp.contributions_to_fix (contribution_id, payment_id, pagarme_state, key, value, payer_email) VALUES (?, ?, ?, ?, ?, ?)" [contributionId, paymentId, state, key, value, payer_email]
      commit con

    resolveConflicts :: Object -> Maybe DbResult -> IO ()
    resolveConflicts transaction dbRecord = do
      alreadyExists <- transactionAlreadyExists transaction
      if shouldUpdateDb transaction dbRecord && not alreadyExists
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
