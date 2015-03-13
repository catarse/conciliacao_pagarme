{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Pagarme
import Network.Wreq
import Control.Monad
import qualified Hasql as H
import qualified Data.Map as M

usingKey :: Options -> Options
usingKey = withApiKey "API_KEY"

getConfirmedTransactionsPage :: Int -> IO [JSONMap]
getConfirmedTransactionsPage page = do
  putStrLn $ "Fetching page " ++ show page
  getTransactions $ (byPage page . byConfirmedStatus . usingKey) defaults

verifyTransaction :: JSONMap -> IO ()
verifyTransaction transaction = putStrLn $ "Transaction " ++ show (transaction M.! "id") ++ " verified"

verifyPage :: [JSONMap] -> IO ()
verifyPage = mapM_ verifyTransaction

verifyTransactions :: Int -> IO ()
verifyTransactions page = do
  toVerify <- getConfirmedTransactionsPage page
  _ <- verifyPage toVerify
  case toVerify of
       []   -> putStrLn "Finish verification"
       _ -> verifyTransactions (page + 1)

main :: IO ()
main = verifyTransactions 1

