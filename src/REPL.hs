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
import System.IO (hFlush, stdout)

repl :: IO ()
repl = do
  putStrLn "Wecome to Kitty!"
  putStrLn " /\\_/\\"
  putStrLn "( o.o )"
  putStrLn " > ^ <"

  putStrLn "please enter quit to quit"
  putStrLn "or help for more info"
  putStrLn "or simply enter your kitty code >:X)"
  repl' initialEnv initialTypeEnv

repl' :: Env -> TypeEnv -> IO ()
repl' env tenv = do
  putStr "kitty>:X)" 
  hFlush stdout
  input <- getLine

  case input of
    "quit" -> putStrLn "goodbye!"
    "help" -> do
      help <- readFileIfExists "resources/help.txt"
      putStrLn help >> repl' env tenv
    _ ->
      -- if input starts with the type keyword,
      -- only type-checks the expression and prints
      -- it's type, but doesn't evaluate it
      if "type " `isPrefixOf` input
        then putStrLn (typeCheckOutput tenv (T.pack (drop 5 input))) >> repl' env tenv
        else -- type checking first, but printing result of type checking
        -- only if type error
        -- otherwise, continue with evaluation
        case updateTypeEnv tenv (T.pack input) of
          Left err -> print err >> repl' env tenv
          Right (newtenv,t) -> case parseRepl env (T.pack input) of
            Left err -> print err >> repl' env tenv
            Right newenv -> putStrLn ((toOutput . _tmpResult) newenv) >> repl' newenv newtenv

-- | reads a file from FilePath if path exists and returns content, otherwise returns an error message string
readFileIfExists :: FilePath -> IO String
readFileIfExists file = catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing) (readFile file) (\_ -> return (file ++ " not found"))
