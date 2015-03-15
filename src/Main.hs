{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pagarme
import Network.Wreq
import qualified Data.Map as M

import Database.HDBC 
import Database.HDBC.PostgreSQL

import Data.Scientific
import Data.Aeson

usingKey :: Options -> Options
usingKey = withApiKey "API KEY"

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

main :: IO ()
main = do
  con <- connectPostgreSQL "host=localhost dbname=catarse_development port=5432 user=postgres password=pass"
  select <- prepare con "SELECT * FROM contributions WHERE payment_id = ?;"
  let 
    verifyInDB transaction = do
                 _ <- execute select [jsonToSql (transaction M.! "id")]
                 result <- fetchRowMap select
                 print result
                 putStrLn $ "Transaction " ++ show (transaction M.! "id") ++ " verified"
    verifyTransactions = verifyTransactionsWith verifyInDB
  verifyTransactions 1
  disconnect con
