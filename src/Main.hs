{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pagarme
import Network.Wreq
import Data.Map

import Database.HDBC 
import Database.HDBC.PostgreSQL

import Data.Scientific
import Data.Aeson

usingKey :: Options -> Options
usingKey = withApiKey ""

jsonToSql :: Value -> SqlValue
jsonToSql value = (toSql . show . coefficient) number
  where Number number = value

getConfirmedTransactionsPage :: Int -> IO [JSONMap]
getConfirmedTransactionsPage page = do
  putStrLn $ "Fetching page " ++ show page
  getTransactions $ (byPage page . byConfirmedStatus . usingKey) defaults

verifyTransactionsWith :: (JSONMap -> IO ()) -> Int -> IO ()
verifyTransactionsWith verifyFunction page = do
  toVerify <- getConfirmedTransactionsPage page
  _ <- mapM_ verifyFunction toVerify
  case toVerify of
       []   -> putStrLn "Finish verification"
       _ -> verifyTransactionsWith verifyFunction (page + 1)

shouldUpdateDb _ Nothing = True
shouldUpdateDb transaction (Just dbRecord) = (dbState == "confirmed" && pagarmeState /= "paid") 
                                             || ((dbState == "refunded" || dbState == "requested_refund") && pagarmeState /= "refunded")
  where SqlByteString dbState = dbRecord ! "state"
        String pagarmeState = transaction ! "status"

main :: IO ()
main = do
  con <- connectPostgreSQL ""
  select <- prepare con "SELECT * FROM contributions WHERE payment_id = ?;"
  let 
    resolveConflicts transaction dbRecord =
      if shouldUpdateDb transaction dbRecord
         then putStrLn "Should update"
         else putStrLn "LGTM"

    verifyInDB transaction = do
                 _ <- execute select [jsonToSql (transaction ! "id")]
                 result <- fetchRowMap select
                 resolveConflicts transaction result
                 putStrLn $ "Transaction " ++ show (transaction ! "id") ++ " verified"
    verifyTransactions = verifyTransactionsWith verifyInDB
  verifyTransactions 1
  disconnect con
