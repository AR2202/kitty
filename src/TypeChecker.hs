module TypeChecker (typeOf, typeCheck, typeCheckPrint, typeCheckOutput, initialTypeEnv, updateTypeEnv) where

import Control.Exception (TypeError)
import Data.Bits (Bits (xor))
import Data.Functor.Contravariant (Comparison)
import qualified Data.Map as M
import qualified Data.Text as T
import Evaluator (initialEnv)
import Foreign.C (eNODEV)
import KittyTypes
import KittyTypes (KittyAST, KittyError)
import Parser

{-This is the type checker of the kitty language-}
initialTypeEnv :: TypeEnv
initialTypeEnv = TypeEnv M.empty M.empty

class TypeCheckable t where
  typeOf :: t -> TypeEnv -> Either KittyError KType

instance TypeCheckable KittyAST where
  typeOf None _ = Right KVoid
  typeOf (StrLit s) _ = Right KString
  typeOf (Letter s) _ = Right KChar
  typeOf (DefType d) env = typeOf d env
  typeOf (IntLit i) _ = Right KInt
  typeOf (FloatLit f) _ = Right KFloat
  typeOf (Variable s) env = lookupVar s env
    where
      lookupVar v en = case M.lookup v (_varTypes en) of
        Nothing -> Left $ DoesNotExistError ("no variable named " ++ s)
        Just val -> Right val
  typeOf (Call fc) env = typeOf fc env
  typeOf (Expr o e e2) env
    | typeOf e env == typeOf e2 env && typeOf e env /= Right KInt && typeOf e env /= Right KFloat = Left $ TypeError ("wrong type: expected number; value of type " ++ showUnwrapped (typeOf e env) ++ " can't be combined used with Operator " ++ (show . opSymb) o)
    | typeOf e env == typeOf e2 env = typeOf e env
    | otherwise = Left $ TypeError ("type mismatch: value of type " ++ showUnwrapped (typeOf e env) ++ " can't be combined with a value of type " ++ showUnwrapped (typeOf e2 env) ++ " with Operator " ++ (show . opSymb) o)
  typeOf (Parens e) env = typeOf e env
  -- instance TypeCheckable BoolExpr where
  typeOf (BoolLit _) _ = Right KBool
  typeOf (And e1 e2) env
    | typeOf e1 env == typeOf e2 env && typeOf e1 env /= Right KBool = Left $ TypeError ("wrong type: expected truth, but got " ++ showUnwrapped (typeOf e1 env) ++ "; and can only be used with values of type truth")
    | typeOf e1 env == typeOf e2 env = typeOf e1 env
    | otherwise = Left $ TypeError ("type mismatch: value of type " ++ showUnwrapped (typeOf e1 env) ++ " can't be combined with a value of type " ++ showUnwrapped (typeOf e2 env) ++ " with and ")
  typeOf (Or e1 e2) env
    | typeOf e1 env == typeOf e2 env && typeOf e1 env /= Right KBool = Left $ TypeError ("wrong type: expected truth, but got " ++ showUnwrapped (typeOf e1 env) ++ "; or can only be used with values of type truth")
    | typeOf e1 env == typeOf e2 env = typeOf e1 env
    | otherwise = Left $ TypeError ("type mismatch: value of type " ++ showUnwrapped (typeOf e1 env) ++ " can't be combined with a value of type " ++ showUnwrapped (typeOf e2 env) ++ " with or ")
  typeOf (Xor e1 e2) env
    | typeOf e1 env == typeOf e2 env && typeOf e1 env /= Right KBool = Left $ TypeError "wrong type: expected truth; xor can only be used with values of type truth"
    | typeOf e1 env == typeOf e2 env = typeOf e1 env
    | otherwise = Left $ TypeError ("type mismatch: value of type " ++ showUnwrapped (typeOf e1 env) ++ " can't be combined with a value of type " ++ showUnwrapped (typeOf e2 env) ++ " with xor ")
  typeOf (Not b) env = case typeOf b env of
    Right KBool -> Right KBool
    _ -> Left $ TypeError ("wrong type: expected truth, but got " ++ showUnwrapped (typeOf b env) ++ "; not requires a value of type " ++ show KBool ++ " but has been given Type " ++ showUnwrapped (typeOf b env))
  -- instance TypeCheckable CompOp where
  typeOf (Equal e1 e2) env
    | typeOf e1 env == typeOf e2 env = Right KBool
    | otherwise = Left $ TypeError $ "type mismatch: value of type " ++ showUnwrapped (typeOf e1 env) ++ " can't be compared with value of type " ++ showUnwrapped (typeOf e2 env)
  typeOf (NotEqual e1 e2) env
    | typeOf e1 env == typeOf e2 env = Right KBool
    | otherwise = Left $ TypeError $ "type mismatch: value of type " ++ showUnwrapped (typeOf e1 env) ++ " can't be compared with value of type " ++ showUnwrapped (typeOf e2 env)
  typeOf (Less e1 e2) env
    | typeOf e1 env /= typeOf e2 env = Left $ TypeError $ "type mismatch: value of type " ++ showUnwrapped (typeOf e1 env) ++ " can't be compared with value of type " ++ showUnwrapped (typeOf e2 env)
    | typeOf e1 env == Right KInt = Right KBool
    | typeOf e1 env == Right KFloat = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise = Left $ TypeError $ "type  " ++ showUnwrapped (typeOf e1 env) ++ " can't be ordered, so < can't be used on this type"
  typeOf (Greater e1 e2) env
    | typeOf e1 env /= typeOf e2 env = Left $ TypeError $ "type mismatch: value of type " ++ showUnwrapped (typeOf e1 env) ++ " can't be compared"
    | typeOf e1 env == Right KInt = Right KBool
    | typeOf e1 env == Right KFloat = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise = Left $ TypeError $ "type  " ++ showUnwrapped (typeOf e1 env) ++ " can't be ordered, so > can't be used on this type"
  typeOf (LessEq e1 e2) env
    | typeOf e1 env /= typeOf e2 env = Left $ TypeError $ "type mismatch: value of type " ++ showUnwrapped (typeOf e1 env) ++ " can't be compared"
    | typeOf e1 env == Right KInt = Right KBool
    | typeOf e1 env == Right KFloat = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise = Left $ TypeError $ "type  " ++ showUnwrapped (typeOf e1 env) ++ " can't be ordered, so <= can't be used on this type"
  typeOf (GreaterEq e1 e2) env
    | typeOf e1 env /= typeOf e2 env = Left $ TypeError $ "type mismatch: value of type " ++ showUnwrapped (typeOf e1 env) ++ " can't be compared"
    | typeOf e1 env == Right KInt = Right KBool
    | typeOf e1 env == Right KFloat = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise = Left $ TypeError $ "type  " ++ showUnwrapped (typeOf e1 env) ++ " can't be ordered, so >= can't be used on this type"

instance TypeCheckable FunctionCall where
  typeOf (FunctionCall fnName fnParams) env = case M.lookup fnName (_functionTypes env) of
    Nothing -> Left $ UndefinedError ("no function named " ++ fnName)
    Just _ -> Left $ UndefinedError "not yet implemented"

instance TypeCheckable Definition where
  typeOf (AssignDef varname e) env = typeOf e env
  typeOf _ _ = Left $ UndefinedError "not yet implemented"

-- | parsers and type checks
typeCheck :: TypeEnv -> T.Text -> Either KittyError KType
typeCheck env text = case parseAsAST text of
  Left _ -> Left $ ParseError $ T.unpack text
  Right ast -> typeOf ast env

-- | parses, typechecks, then prints result
typeCheckPrint :: T.Text -> String
typeCheckPrint = show . typeCheck initialTypeEnv

typeCheckOutput :: TypeEnv -> T.Text -> String
typeCheckOutput env text = case typeCheck env text of
  Right x -> show x
  Left x -> show x

updateTypeEnv :: TypeEnv -> T.Text -> Either KittyError (TypeEnv, KType)
updateTypeEnv tenv text = case parseAsAST text of
  Left _ -> Left $ ParseError (T.unpack text)
  Right (DefType (AssignDef varname e)) -> case typeCheck tenv text of
    Right t -> Right (tenv {_varTypes = M.insert varname t (_varTypes tenv)}, t)
    Left err -> Left err
  Right ast -> (,) tenv <$> typeOf ast tenv

{-helper functions-}

showUnwrapped :: (Show a, Show b) => Either a b -> String
showUnwrapped (Left x) = show x
showUnwrapped (Right y) = show y
