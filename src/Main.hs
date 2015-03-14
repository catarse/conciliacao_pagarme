{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pagarme
import Network.Wreq
import Control.Monad
import qualified Data.Map as M

import Control.Exception
import Database.HDBC 
import Database.HDBC.PostgreSQL

usingKey :: Options -> Options
usingKey = withApiKey "API KEY"

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
  c <- connectPostgreSQL "host=localhost dbname=postgres user=postgres password=pass"
  select <- prepare c "SELECT * FROM version();"
  let 
    verifyInDB transaction = do
                 execute select []
                 result <- fetchAllRows select
                 print result
                 putStrLn $ "Transaction " ++ show (transaction M.! "id") ++ " verified"
    verifyTransactions = verifyTransactionsWith verifyInDB
  verifyTransactions 1
