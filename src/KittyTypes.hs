{-# LANGUAGE DeriveFunctor #-}

module KittyTypes (Operator (..), KittyAST (..), Env (..), TypeEnv(..),KittyError (..), ArithOperations, FunctionDefinition (..), Definition (..), KType (..), FunctionCall (..), add, sub, divide, mult, toOutput, opSymb) where

import Data.Char (toLower)
import qualified Data.Map as M
import Data.List(foldl1')
import Text.ParserCombinators.ReadP
import Data.Char (isSpace)
import Control.Applicative ((<|>))

{- This file defines the Types and Values of a kitty programm-}

-- | Kitty Types
data KType = KBool | KInt | KFloat | KString | KChar | KVoid | OneOf KType KType | KFun ([KType],[KType]) deriving ( Eq)

instance Show KType where
  show KBool = "truth"
  show KInt = "wholeNumber"
  show KFloat = "decimalNumber"
  show KString = "text"
  show KVoid = "empty"
  show KChar = "letter"
  show (KFun (args, returns))= foldl1' (++ ) (map show args) ++ "->"++ foldl1' ( ++) (map show returns)
  show (OneOf i e )= "one of " ++ show i ++ " or "++ show e
instance Read KType where
    readsPrec _ = readP_to_S parseKType

parseKType :: ReadP KType
parseKType = parseKBool
         <|> parseKInt
         <|> parseKFloat
         <|> parseKString
         <|> parseKChar
         <|> parseKVoid
         -- <|> parseKFun
         <|> parseOneOf

parseKBool :: ReadP KType
parseKBool = do
    skipSpaces
    _ <- string "truth"
    return KBool

parseKInt :: ReadP KType
parseKInt = do
    skipSpaces
    _ <- string "wholeNumber"
    return KInt

parseKFloat :: ReadP KType
parseKFloat = do
    skipSpaces
    _ <- string "decimalNumber"
    return KFloat

parseKString :: ReadP KType
parseKString = do
    skipSpaces
    _ <- string "text"
    return KString

parseKChar :: ReadP KType
parseKChar = do
    skipSpaces
    _ <- string "letter"
    return KChar

parseKVoid :: ReadP KType
parseKVoid = do
    skipSpaces
    _ <- string "empty"
    return KVoid

-- parseKFun :: ReadP KType
-- parseKFun = do
--     args <- sepBy1 parseKType (skipSpaces >> return ())
--     _ <- skipSpaces >> string "->" >> skipSpaces
--     returns <- sepBy1 parseKType (skipSpaces >> return ())
--     return $ KFun (args, returns)

parseOneOf :: ReadP KType
parseOneOf = do
    _ <- string "one of"
    skipSpaces
    i <- parseKType
    skipSpaces
    _ <- string "or"
    skipSpaces
    e <- parseKType
    return $ OneOf i e

-- | Arithmetic operators
data Operator
  = Add
  | Mult
  | Div
  | Sub
  deriving (Show, Read, Eq)

-- | function and variable definition and assignment
data Definition
  = AssignDef String KittyAST
  | FunctionDef FunctionDefinition
  deriving (Show, Eq)

data FunctionCall = FunctionCall String [String] deriving (Show, Eq)

-- | AST Variants
data KittyAST
  = None
  | IntLit Int
  | FloatLit Float
  | Variable String
  | BoolLit Bool
  | StrLit String
  | Letter Char
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
  | If KittyAST [KittyAST]
  | IfElse KittyAST [KittyAST][KittyAST]
  | UnwrapAs KittyAST KType String [KittyAST] -- for unwrapping OneOfs 
  | While KittyAST [KittyAST]
  | Print KittyAST
  deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition
  { _funcName :: String,
    _funcParams :: [String],
    _funcBody :: [KittyAST]
  }
  deriving (Show, Eq)

type ErrorMsg = String

type FileName = String

type LineNumber = Int

-- | Error Types
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
type FType = M.Map String KType -- should always be KFun
type VType = M.Map String KType
data Env = Env {_definitions :: Defs, _variables :: Defs, _tmpResult :: KittyAST} deriving (Show)
data TypeEnv = TypeEnv {_functionTypes :: FType, _varTypes :: VType}  deriving (Show, Read, Eq)
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
  toOutput (Letter c) = show c
  toOutput (If condition ifblock) = "if " ++ toOutput condition ++ "\n" ++ foldl1 (++) (map ((++) "\n" .toOutput) ifblock) 
  toOutput (IfElse condition ifblock elseblock) = "if " ++ toOutput condition ++ "\n" ++ foldl1 (++) (map ((++) "\n" .toOutput) ifblock) ++ "\nelse\n" ++ foldl1 (++) (map ((++) "\n" .toOutput) elseblock) 
  toOutput (While condition loopBody) = "if " ++ toOutput condition ++ "\n" ++ foldl1 (++) (map ((++) "\n" .toOutput) loopBody) 
-- | convert operator to the char representing it
-- | used for printing an operator
opSymb :: Operator -> Char
opSymb Add = '+'
opSymb Mult = '*'
opSymb Sub = '-'
opSymb Div = '/'
