{-# LANGUAGE DeriveFunctor #-}

module KittyTypes (Operator (..), KittyAST (..), Env (..), KittyError (..), ArithOperations, FunctionDefinition (..), Definition (..), KType (..), FunctionCall (..), add, sub, divide, mult, toOutput, opSymb, ) where

import Data.Char (toLower)
import qualified Data.Map as M

data KType = KBool | KInt | KFloat | KString | KVoid deriving (Read, Eq)

instance Show KType where
  show KBool = "truth"
  show KInt = "wholeNumber"
  show KFloat = "decimalNumber"
  show KString = "text"
  show KVoid = "empty"

data Operator
  = Add
  | Mult
  | Div
  | Sub
  deriving (Show, Read, Eq)

data Definition
  = AssignDef String KittyAST
  | FunctionDef FunctionDefinition
  deriving (Show,Eq)

data FunctionCall = FunctionCall String [String] deriving (Show, Eq)

data KittyAST
  = None
  | IntLit Int
  | FloatLit Float
  | Variable String
  | BoolLit Bool
  | StrLit String
  | DefType Definition
  | Call FunctionCall
  | Expr Operator KittyAST KittyAST
  | Parens KittyAST
  | And KittyAST KittyAST
  | Or KittyAST KittyAST
  | Not KittyAST
  | Xor KittyAST KittyAST
  | Equal KittyAST KittyAST
  | Greater KittyAST KittyAST
  | Less KittyAST KittyAST
  | NotEqual KittyAST KittyAST
  | LessEq KittyAST KittyAST
  | GreaterEq KittyAST KittyAST
  deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition
  { _funcName :: String,
    _funcParams :: [String],
    _funcBody :: [KittyAST]
  }
  deriving (Show,Eq)

type ErrorMsg = String

type FileName = String

type LineNumber = Int

data KittyError
  = TypeError ErrorMsg
  | ParseError ErrorMsg
  | DoesNotExistError ErrorMsg
  | UndefinedError ErrorMsg
  deriving (Eq)

instance Show KittyError where
  show (TypeError msg) = "Type Error: " ++ msg
  show (ParseError msg) = "Parse Error: Not a valid Kitty program" ++ msg
  show (DoesNotExistError msg) = "DoesNotExist Error: " ++ msg
  show (UndefinedError msg) = "Undefined Error: " ++ msg

type Defs = M.Map String KittyAST

data Env = Env {_defnitions :: Defs, _variables :: Defs, _tmpResult :: KittyAST} deriving (Show)

class ArithOperations a where
  add :: a -> a -> a
  sub :: a -> a -> a
  mult :: a -> a -> a
  divide :: a -> a -> a

instance ArithOperations Float where
  add = (+)
  sub = (-)
  mult = (*)
  divide = (/)

instance ArithOperations Int where
  add = (+)
  sub = (-)
  mult = (*)
  divide = div

class ProgramOutput a where
  toOutput :: a -> String

instance ProgramOutput KittyAST where
  toOutput None = ""
  toOutput (StrLit s) = s
  toOutput (DefType d) = show d
  toOutput (Equal x y) = toOutput x ++ "==" ++ toOutput y
  toOutput (NotEqual x y) = toOutput x ++ "=/=" ++ toOutput y
  toOutput (Less x y) = toOutput x ++ "<" ++ toOutput y
  toOutput (Greater x y) = toOutput x ++ ">" ++ toOutput y
  toOutput (GreaterEq x y) = toOutput x ++ ">=" ++ toOutput y
  toOutput (LessEq x y) = toOutput x ++ "<=" ++ toOutput y
  toOutput (IntLit x) = show x
  toOutput (FloatLit x) = show x
  toOutput (Expr op e1 e2) = toOutput e1 ++ (opSymb op : toOutput e2)
  toOutput (Parens e) = '(' : toOutput e ++ ")"
  toOutput (Call x) = show x
  toOutput (Variable v) = v
  toOutput (BoolLit x) = map toLower $ show x
  toOutput (And e1 e2) = toOutput e1 ++ " and " ++ toOutput e2
  toOutput (Or e1 e2) = toOutput e1 ++ " or " ++ toOutput e2
  toOutput (Xor e1 e2) = toOutput e1 ++ " xor " ++ toOutput e2
  toOutput (Not e) = "not " ++ toOutput e

opSymb :: Operator -> Char
opSymb Add = '+'
opSymb Mult = '*'
opSymb Sub = '-'
opSymb Div = '/'
