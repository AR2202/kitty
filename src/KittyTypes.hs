{-# LANGUAGE DeriveFunctor #-}

module KittyTypes (Operator (..), ArithExpr (..), KittyAST (..), Env (..), KittyError (..), ArithOperations, FunctionDefinition (..), Definition (..), KType (..), FunctionCall (..), BoolExpr (..), add, sub, divide, mult, toOutput, opSymb, CompOp (..)) where

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

-- not sure this duplication of variable and function call is sensible
data ArithExpr = IntLit Int | FloatLit Float | Variable String | Call FunctionCall | Exp Operator ArithExpr ArithExpr | Parens ArithExpr deriving (Show)

data BoolExpr = BoolLit Bool | And BoolExpr BoolExpr | Or BoolExpr BoolExpr | Not BoolExpr | Var String | Xor BoolExpr BoolExpr | FCall FunctionCall deriving (Show)

data CompOp = Equal KittyAST KittyAST | Greater KittyAST KittyAST | Less KittyAST KittyAST | NotEqual KittyAST KittyAST | LessEq KittyAST KittyAST | GreaterEq KittyAST KittyAST deriving (Show)

data Definition
  = AssignDef String KittyAST
  | FunctionDef FunctionDefinition
  deriving (Show)

data FunctionCall = FunctionCall String [String] deriving (Show)

data KittyAST
  = None
  | Expr ArithExpr
  | Boolean BoolExpr
  | Comp CompOp
  | StrLit String
  | DefType Definition
  deriving (Show)

data FunctionDefinition = FunctionDefinition
  { _funcName :: String,
    _funcParams :: [String],
    _funcBody :: [KittyAST]
  }
  deriving (Show)

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

{- class TryArithOperations a where
  tryAdd :: a -> a -> Either KittyError a
  trySub :: a -> a -> Either KittyError a
  tryMult :: a -> a -> Either KittyError a
  tryDivide :: a -> a -> Either KittyError a

instance TryArithOperations KittyAST where
  tryAdd None x = Right x
  tryAdd x None = Right x
  tryAdd (NumI i) (NumI j) = Right $ NumI (add i j)
  tryAdd (NumF i) (NumF j) = Right $ NumF (add i j)
  tryAdd (StrLit s) (StrLit t) = Right $ StrLit (s ++ t)
  tryAdd (Variable s) _ = Left (UndefinedError "variable " ++ s) -- variables have to be resolved first
  tryAdd _ (Variable s) = Left (UndefinedError "variable " ++ s)
  tryAdd (Arith s) _ = Left (UndefinedError "Expressions can't be added") -- expressions have to be evaluated
  tryAdd _ (Arith s) = Left (UndefinedError "Expressions can't be added")
  tryAdd x y = Left $ TypeError "Types " ++ typeOf x ++ " and " ++ typeOf y " can't be added" -}

class ProgramOutput a where
  toOutput :: a -> String

instance ProgramOutput KittyAST where
  toOutput None = ""
  toOutput (Expr e) = toOutput e
  toOutput (Boolean e) = toOutput e
  toOutput (StrLit s) = s
  toOutput (DefType d) = show d
  toOutput (Comp c) = toOutput c

instance ProgramOutput CompOp where
  toOutput (Equal x y) = toOutput x ++ "==" ++ toOutput y
  toOutput (NotEqual x y) = toOutput x ++ "=/=" ++ toOutput y
  toOutput (Less x y) = toOutput x ++ "<" ++ toOutput y
  toOutput (Greater x y) = toOutput x ++ ">" ++ toOutput y
  toOutput (GreaterEq x y) = toOutput x ++ ">=" ++ toOutput y
  toOutput (LessEq x y) = toOutput x ++ "<=" ++ toOutput y

instance ProgramOutput ArithExpr where
  toOutput (IntLit x) = show x
  toOutput (FloatLit x) = show x
  toOutput (Exp op e1 e2) = toOutput e1 ++ (opSymb op : toOutput e2)
  toOutput (Parens e) = '(' : toOutput e ++ ")"
  toOutput (Call x) = show x
  toOutput (Variable v) = v

instance ProgramOutput BoolExpr where
  toOutput (BoolLit x) = show x
  toOutput (And e1 e2) = toOutput e1 ++ " and " ++ toOutput e2
  toOutput (Or e1 e2) = toOutput e1 ++ " or " ++ toOutput e2
  toOutput (Xor e1 e2) = toOutput e1 ++ " xor " ++ toOutput e2
  toOutput (Not e) = "not " ++ toOutput e
  toOutput (Var e) = e
  toOutput (FCall e) = show e

opSymb :: Operator -> Char
opSymb Add = '+'
opSymb Mult = '*'
opSymb Sub = '-'
opSymb Div = '/'

{- typeOf :: KittyAST -> String
typeOf None = "none"
typeOf (NumI _) = "integer"
typeOf (NumF _) = "number"
typeOf (StrLit _) = "text" -}
