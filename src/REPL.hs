{-# LANGUAGE OverloadedStrings #-}

module REPL (repl) where

import Control.Exception (catchJust)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import Evaluator
import KittyTypes
import Parser
import System.IO.Error (ioeGetErrorType, isDoesNotExistErrorType)
import TypeChecker

repl :: IO ()
repl = do
  putStrLn "Wecome to Kitty!"
  putStrLn " /\\_/\\"
  putStrLn "( o.o )"
  putStrLn " > ^ <"

  putStrLn "please enter quit to quit"
  putStrLn "or help for more info"
  putStrLn "or simply enter your kitty code >:X)"
  repl' initialEnv

repl' :: Env -> IO ()
repl' env = do
  putStr "kitty>:X)"
  input <- getLine
  case input of
    "quit" -> putStrLn "goodbye!"
    "help" -> do
      help <- readFileIfExists "resources/help.txt"
      putStrLn help >> repl' env



    _ ->
      -- if input starts with the type keyword,
      -- only type-checks the expression and prints
      -- it's type, but doesn't evaluate it
      if "type " `isPrefixOf` input
        then putStrLn (typeCheckOutput env (T.pack (drop 5 input))) >> repl' env
        -- type checking first, but printing result of type checking
        -- only if type error
        -- otherwise, continue with evaluation
        else case typeCheck env (T.pack input) of
          Left err -> print err >> repl' env
          Right _ -> case parseRepl env (T.pack input) of
            Left err -> print err >> repl' env
            Right newenv -> putStrLn ((toOutput . _tmpResult) newenv) >> repl' newenv

-- | reads a file from FilePath if path exists and returns conent, otherwise returns an error message string
readFileIfExists :: FilePath -> IO String
readFileIfExists file = catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing) (readFile file) (\_ -> return (file ++ " not found"))
