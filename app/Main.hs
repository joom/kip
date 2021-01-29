{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.IO.Class
import System.Exit
import System.Environment
import Data.List

import System.Console.Haskeline
import Language.Foma

import Kip.Parser

main :: IO ()
main = 
  lookupEnv "TRMORPH" >>= \case
    Nothing -> die "The TRMORPH environment variable does not exist"
    Just path -> do
      fsm <- fsmReadBinaryFile path
      runInputT defaultSettings (loop fsm)
   where
    loop :: FSM -> InputT IO ()
    loop fsm = do
      minput <- getInputLine "kip> "
      case minput of
          Nothing -> return ()
          Just ":quit" -> return ()
          Just input 
            | Just word <- stripPrefix ":up " input -> do 
                liftIO (ups fsm word) >>= \xs -> mapM_ outputStrLn xs
                loop fsm
            | otherwise -> do 
                outputStrLn $ "Unrecognized command: " ++ input
                           ++ "\n\nexample commands:\n:up okudum\n:quit"
                loop fsm
