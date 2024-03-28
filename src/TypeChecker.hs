module TypeChecker (typeOf, typeCheck, typeCheckPrint, typeCheckOutput) where

import Data.Bits (Bits (xor))
import Data.Functor.Contravariant (Comparison)
import qualified Data.Map as M
import qualified Data.Text as T
import Evaluator (initialEnv)
import Foreign.C (eNODEV)
import KittyTypes
import KittyTypes (KittyAST, KittyError)
import Parser
import Control.Exception (TypeError)

class TypeCheckable t where
  typeOf :: t -> Env -> Either KittyError KType

instance TypeCheckable KittyAST where
  typeOf None _ = Right KVoid
  -- typeOf (Expr e) env = typeOf e env
  -- typeOf (Boolean e) env = typeOf e env
  typeOf (StrLit s) _ = Right KString
  typeOf (DefType d) env = typeOf d env
  -- typeOf (Comp c) env = typeOf c env

  -- instance TypeCheckable ArithExpr where
  typeOf (IntLit i) _ = Right KInt
  typeOf (FloatLit f) _ = Right KFloat
  typeOf (Variable s) env = lookupVar s env >>= (flip typeOf) env
    where
      lookupVar v en = case M.lookup v (_variables en) of
        Nothing -> Left $ DoesNotExistError ("no variable named " ++ s)
        Just val -> Right val
  typeOf (Call fc) env = typeOf fc env
  typeOf (Expr o e e2) env
    | typeOf e env == typeOf e2 env && typeOf e env /= Right KInt &&typeOf e env /= Right KFloat && typeOf e env /= Right KString = Left $ TypeError ("wrong type: expected number; value of type " ++ show (typeOf e env) ++ " can't be combined used with Operator " ++ (show . opSymb) o)
    | typeOf e env == typeOf e2 env = typeOf e env
    | otherwise = Left $ TypeError ("type mismatch: value of type " ++ show (typeOf e env) ++ " can't be combined with a value of type " ++ show (typeOf e2 env) ++ " with Operator " ++ (show . opSymb) o)
  typeOf (Parens e) env = typeOf e env
  -- instance TypeCheckable BoolExpr where
  typeOf (BoolLit _) _ = Right KBool
  typeOf (And e1 e2) env
    | typeOf e1 env == typeOf e2 env && typeOf e1 env /= Right KBool = Left $TypeError "wrong type: expected truth; and can only be used with values of type truth"
    | typeOf e1 env == typeOf e2 env = typeOf e1 env
    | otherwise = Left $ TypeError ("type mismatch: value of type " ++ show (typeOf e1 env) ++ " can't be combined with a value of type " ++ show (typeOf e2 env) ++ " with and ")
  typeOf (Or e1 e2) env
   | typeOf e1 env == typeOf e2 env && typeOf e1 env /= Right KBool = Left $TypeError "wrong type: expected truth; or can only be used with values of type truth"
    | typeOf e1 env == typeOf e2 env = typeOf e1 env
    | otherwise = Left $ TypeError ("type mismatch: value of type " ++ show (typeOf e1 env) ++ " can't be combined with a value of type " ++ show (typeOf e2 env) ++ " with or ")
  typeOf (Xor e1 e2) env
   | typeOf e1 env == typeOf e2 env && typeOf e1 env /= Right KBool = Left $TypeError "wrong type: expected truth; xor can only be used with values of type truth"
    | typeOf e1 env == typeOf e2 env = typeOf e1 env
    | otherwise = Left $ TypeError ("type mismatch: value of type " ++ show (typeOf e1 env) ++ " can't be combined with a value of type " ++ show (typeOf e2 env) ++ " with xor ")
  typeOf (Not b) env = case typeOf b env of
    Right KBool -> Right KBool
    _ -> Left $ TypeError ("not requires a value of type " ++ show KBool ++ " but has been given Type " ++ show (typeOf b env))


  -- instance TypeCheckable CompOp where
  typeOf (Equal e1 e2) env
    | typeOf e1 env == typeOf e2 env = Right KBool
    | otherwise = Left $ TypeError $ "type mismatch: value of type " ++ show (typeOf e1 env) ++ " can't be compared with value of type "++ show (typeOf e2 env)
  typeOf (NotEqual e1 e2) env
    | typeOf e1 env == typeOf e2 env = Right KBool
    | otherwise = Left $ TypeError $ "type mismatch: value of type " ++ show (typeOf e1 env) ++ " can't be compared with value of type "++ show (typeOf e2 env)
  typeOf (Less e1 e2) env
    | typeOf e1 env /= typeOf e2 env = Left $ TypeError $ "type mismatch: value of type " ++ show (typeOf e1 env) ++ " can't be compared with value of type "++ show (typeOf e2 env)
    | typeOf e1 env == Right KInt = Right KBool
    | typeOf e1 env == Right KFloat = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise = Left $ TypeError $ "type  " ++ show (typeOf e1 env) ++ " can't be ordered, and < can't be used on this type"
  typeOf (Greater e1 e2) env
    | typeOf e1 env /= typeOf e2 env = Left $ TypeError $ "type mismatch: value of type " ++ show (typeOf e1 env) ++ " can't be compared"
    | typeOf e1 env == Right KInt = Right KBool
    | typeOf e1 env == Right KFloat = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise = Left $ TypeError $ "type  " ++ show (typeOf e1 env) ++ " can't be ordered, and > can't be used on this type"
  typeOf (LessEq e1 e2) env
    | typeOf e1 env /= typeOf e2 env = Left $ TypeError $ "type mismatch: value of type " ++ show (typeOf e1 env) ++ " can't be compared"
    | typeOf e1 env == Right KInt = Right KBool
    | typeOf e1 env == Right KFloat = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise = Left $ TypeError $ "type  " ++ show (typeOf e1 env) ++ " can't be ordered, and <= can't be used on this type"
  typeOf (GreaterEq e1 e2) env
    | typeOf e1 env /= typeOf e2 env = Left $ TypeError $ "type mismatch: value of type " ++ show (typeOf e1 env) ++ " can't be compared"
    | typeOf e1 env == Right KInt = Right KBool
    | typeOf e1 env == Right KFloat = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise = Left $ TypeError $ "type  " ++ show (typeOf e1 env) ++ " can't be ordered, and >= can't be used on this type"

instance TypeCheckable FunctionCall where
  typeOf (FunctionCall fnName fnParams) env = case M.lookup fnName (_defnitions env) of
    Nothing -> Left $ UndefinedError ("no function named " ++ fnName)
    Just _ -> Left $ UndefinedError "not yet implemented"

instance TypeCheckable Definition where
  typeOf (AssignDef varname e) env = typeOf e env
  typeOf _ _ = Left $ UndefinedError "not yet implemented"

-- | parsers and type checks
typeCheck :: Env -> T.Text -> Either KittyError KType
typeCheck env text = case parseAsAST text of
  Left _ -> Left $ ParseError $ T.unpack text
  Right ast -> typeOf ast env

-- | parses, typechecks, then prints result
typeCheckPrint :: T.Text -> String
typeCheckPrint = show . typeCheck initialEnv

typeCheckOutput :: Env -> T.Text -> String
typeCheckOutput env text = case typeCheck env text of
  Right x -> show x
  Left x -> show x
