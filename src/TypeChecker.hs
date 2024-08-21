module TypeChecker (typeOf, typeCheck, typeCheckPrint, typeCheckOutput, initialTypeEnv, updateTypeEnv) where

import Control.Exception (TypeError)
import Control.Monad (foldM, void)
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

-- | initial empty type environment
initialTypeEnv :: TypeEnv
initialTypeEnv = TypeEnv M.empty M.empty

class TypeCheckable t where
  typeOf :: t -> TypeEnv -> Either KittyError KType

-- | the main part of the Typechecker. Checks an AST and produces
--  | either a type or an error
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
    | typeOf e1 env == typeOf e2 env && typeOf e1 env /= Right KBool = Left $ TypeError ("wrong type: expected truth, but got " ++ showUnwrapped (typeOf e1 env) ++ " xor can only be used with values of type truth")
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
  typeOf (If condition ifblock) env
    | typeOf condition env /= Right KBool = Left $ TypeError $ "condition for if must be a value of type truth, but a value of type " ++ showUnwrapped (typeOf condition env) ++ " was provided"
    | null ifblock = Right KVoid
    | otherwise = case foldM updateTypeEnv' env ifblock of
        Left err -> Left err
        Right env -> typeOf (last ifblock) env -- type of the last expression
        -- still undecided if this is the desired behaviour
  typeOf (IfElse condition ifblock elseblock) env
    | typeOf condition env /= Right KBool = Left $ TypeError $ "condition for if must be a value of type truth, but a value of type " ++ showUnwrapped (typeOf condition env) ++ " was provided"
    | otherwise = case ifTypeOrErr of
        Left err -> Left err
        Right typeIf -> case elseTypeOrErr of
          Left err -> Left err
          Right typeElse -> Right $ OneOf typeIf typeElse
    where
      ifTypeOrErr = checkIfType ifblock
      elseTypeOrErr = checkIfType elseblock
      checkIfType [] = Right KVoid
      checkIfType statements = case foldM updateTypeEnv' env statements of
        Left err -> Left err
        Right env' -> typeOf (last statements) env'

instance TypeCheckable FunctionCall where
  typeOf (FunctionCall fnName fnParams) env = case M.lookup fnName (_functionTypes env) of
    Nothing -> Left $ UndefinedError ("no function named " ++ fnName)
    Just _ -> Left $ UndefinedError "not yet implemented"

instance TypeCheckable Definition where
  typeOf (AssignDef varname e) env = typeOf e env
  typeOf _ _ = Left $ UndefinedError "not yet implemented"

{-type checking functions functions-}

-- | parsers and type checks
typeCheck :: TypeEnv -> T.Text -> Either KittyError KType
typeCheck env text = case parseAsAST text of
  Left _ -> Left $ ParseError $ T.unpack text
  Right ast -> typeOf ast env

-- | parses, typechecks, then prints result
typeCheckPrint :: T.Text -> String
typeCheckPrint = show . typeCheck initialTypeEnv

-- | Type checks the parsed text and converts the error or the type to a string
typeCheckOutput :: TypeEnv -> T.Text -> String
typeCheckOutput env text = case typeCheck env text of
  Right x -> show x
  Left x -> show x

-- | handles different cases of the AST types to update type environment, and return type 
-- | updates Type environment only if a definition or if/else block is encountered
-- | otherwise, keeps the type enviornment unchanged
-- | also returns the type of the expression for type checking
updateTypeEnv :: TypeEnv -> T.Text -> Either KittyError (TypeEnv, KType)
updateTypeEnv tenv text = case parseAsAST text of
  Left _ -> Left $ ParseError (T.unpack text)
  Right (DefType (AssignDef varname e)) -> case typeCheck tenv text of
    Right t -> Right (tenv {_varTypes = M.insert varname t (_varTypes tenv)}, t)
    Left err -> Left err
  Right (If c e) -> case typeCheck tenv text of
    Right t -> makeEitherTuple  (unifyMaybeTypeEnvs (updateManyTypeEnv tenv e) ( Right tenv)) t
    Left err -> Left err
  Right (IfElse c i e) -> case typeCheck tenv text of
    Right t -> makeEitherTuple (unifyMaybeTypeEnvs (updateManyTypeEnv tenv i) (updateManyTypeEnv tenv e)) t
    Left err -> Left err
  Right ast -> (,) tenv <$> typeOf ast tenv

-- | combines two typeEnvs by combining their variable type lookup tables
-- | if variable names collide, creates a OneOf of both types if types are different
unifyTypeEnvs :: TypeEnv -> TypeEnv -> TypeEnv
unifyTypeEnvs tenv1 tenv2 = tenv1 {_varTypes = M.map (unwrapOneOfIfEqual . oneOfVoidIfMissing) $ M.unionWith OneOf (_varTypes tenv1) (_varTypes tenv2)}

-- | wraps two types in a OneOf if and only if they are different
oneOfIfDifferent :: KType -> KType -> KType
oneOfIfDifferent type1 type2 = if type1 == type2 then type1 else OneOf type1 type2

-- | unwraps OneOf if the two contained types are equal
unwrapOneOfIfEqual :: KType -> KType
unwrapOneOfIfEqual (OneOf type1 type2) = if type1 == type2 then type1 else OneOf type1 type2
unwrapOneOfIfEqual x = x

-- | wraps a KType in OneOf, adding KVoid for the second type
oneOfVoidIfMissing :: KType -> KType
oneOfVoidIfMissing (OneOf a b) = OneOf a b
oneOfVoidIfMissing x = OneOf x KVoid

-- | unifies two values of Type Either a TypeEnv into one
unifyMaybeTypeEnvs :: Either a TypeEnv -> Either a TypeEnv -> Either a TypeEnv
unifyMaybeTypeEnvs (Left e) _ = Left e
unifyMaybeTypeEnvs _ (Left e) = Left e
unifyMaybeTypeEnvs (Right e1) (Right e2) = Right (unifyTypeEnvs e1 e2)

-- | helper function for making a tuple of two values where the first is an Either a b
makeEitherTuple :: Monad f => f a1 -> a2 -> f (a1, a2)
makeEitherTuple eithera b = (,) <$> eithera <*> return b

-- | fold the updateTupeEnv' function over a foldable of KittyAST (e.g. a list)
updateManyTypeEnv :: Foldable t => TypeEnv -> t KittyAST -> Either KittyError TypeEnv
updateManyTypeEnv tenv e = foldl (\tenv' e' -> tenv' >>= flip updateTypeEnv' e') (return tenv) e

-- | inserts a variable into the type environment and produces a new type environment or an error
updateTypeEnv' :: TypeEnv -> KittyAST -> Either KittyError TypeEnv
updateTypeEnv' tenv (DefType (AssignDef varname e)) = case typeOf e tenv of
  Right t -> Right (tenv {_varTypes = M.insert varname t (_varTypes tenv)})
  Left err -> Left err
updateTypeEnv' tenv e = case typeOf e tenv of
  Right _ -> Right tenv
  Left err -> Left err

{-helper functions-}

-- | produces a string of the value after unwrapping from Either
showUnwrapped :: (Show a, Show b) => Either a b -> String
showUnwrapped (Left x) = show x
showUnwrapped (Right y) = show y
