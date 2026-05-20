{-# LANGUAGE InstanceSigs #-}

module TypeChecker
  ( typeOf,
    typeCheck,
    typeCheckPrint,
    typeCheckOutput,
    initialTypeEnv,
    updateTypeEnv,
    checkBlockType,
  )
where

import Control.Monad (foldM)
import qualified Data.Map as M
import qualified Data.Text as T
import KittyTypes
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
        Nothing  -> Left $ DoesNotExistError ("I don't know what " ++ s ++ " is — did you forget to create it?")
        Just val -> Right val
  typeOf (Call fc) env = typeOf fc env
  typeOf (Expr o e e2) env
    | typeOf e env == typeOf e2 env && typeOf e env /= Right KInt && typeOf e env /= Right KFloat =
        Left $ TypeError
          ( "I can't use " ++ [opSymb o] ++ " with " ++ showUnwrapped (typeOf e env)
              ++ ". " ++ [opSymb o] ++ " only works with wholeNumbers and decimalNumbers."
          )
    | typeOf e env == typeOf e2 env = typeOf e env
    | otherwise =
        Left $ TypeError
          ( "I can't use " ++ [opSymb o] ++ " with a " ++ showUnwrapped (typeOf e env)
              ++ " and a " ++ showUnwrapped (typeOf e2 env)
              ++ ". Both sides need to be the same type."
          )
  typeOf (Parens e) env = typeOf e env
  typeOf (BoolLit _) _ = Right KBool
  typeOf (And e1 e2) env
    | typeOf e1 env == typeOf e2 env && typeOf e1 env /= Right KBool =
        Left $ TypeError
          ( "'and' only works with truth values, but I got a "
              ++ showUnwrapped (typeOf e1 env) ++ "."
          )
    | typeOf e1 env == typeOf e2 env = typeOf e1 env
    | otherwise =
        Left $ TypeError
          ( "Both sides of 'and' need to be truth values, but I got a "
              ++ showUnwrapped (typeOf e1 env)
              ++ " and a " ++ showUnwrapped (typeOf e2 env) ++ "."
          )
  typeOf (Or e1 e2) env
    | typeOf e1 env == typeOf e2 env && typeOf e1 env /= Right KBool =
        Left $ TypeError
          ( "'or' only works with truth values, but I got a "
              ++ showUnwrapped (typeOf e1 env) ++ "."
          )
    | typeOf e1 env == typeOf e2 env = typeOf e1 env
    | otherwise =
        Left $ TypeError
          ( "Both sides of 'or' need to be truth values, but I got a "
              ++ showUnwrapped (typeOf e1 env)
              ++ " and a " ++ showUnwrapped (typeOf e2 env) ++ "."
          )
  typeOf (Xor e1 e2) env
    | typeOf e1 env == typeOf e2 env && typeOf e1 env /= Right KBool =
        Left $ TypeError
          ( "'xor' only works with truth values, but I got a "
              ++ showUnwrapped (typeOf e1 env) ++ "."
          )
    | typeOf e1 env == typeOf e2 env = typeOf e1 env
    | otherwise =
        Left $ TypeError
          ( "Both sides of 'xor' need to be truth values, but I got a "
              ++ showUnwrapped (typeOf e1 env)
              ++ " and a " ++ showUnwrapped (typeOf e2 env) ++ "."
          )
  typeOf (Not b) env =
    case typeOf b env of
      Right KBool -> Right KBool
      _ ->
        Left $ TypeError
          ( "'not' only works with truth values, but I got a "
              ++ showUnwrapped (typeOf b env) ++ "."
          )
  typeOf (Equal e1 e2) env
    | typeOf e1 env == typeOf e2 env = Right KBool
    | otherwise =
        Left $ TypeError
          ( "I can't compare a " ++ showUnwrapped (typeOf e1 env)
              ++ " with a " ++ showUnwrapped (typeOf e2 env)
              ++ " using ==. Both sides need to be the same type."
          )
  typeOf (NotEqual e1 e2) env
    | typeOf e1 env == typeOf e2 env = Right KBool
    | otherwise =
        Left $ TypeError
          ( "I can't compare a " ++ showUnwrapped (typeOf e1 env)
              ++ " with a " ++ showUnwrapped (typeOf e2 env)
              ++ " using =/=. Both sides need to be the same type."
          )
  typeOf (Less e1 e2) env
    | typeOf e1 env /= typeOf e2 env =
        Left $ TypeError
          ( "I can't use < to compare a " ++ showUnwrapped (typeOf e1 env)
              ++ " with a " ++ showUnwrapped (typeOf e2 env)
              ++ ". Both sides need to be the same type."
          )
    | typeOf e1 env == Right KInt    = Right KBool
    | typeOf e1 env == Right KFloat  = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise =
        Left $ TypeError
          ( "I can't use < with " ++ showUnwrapped (typeOf e1 env)
              ++ ". Only wholeNumbers, decimalNumbers, and text can be compared with <."
          )
  typeOf (Greater e1 e2) env
    | typeOf e1 env /= typeOf e2 env =
        Left $ TypeError
          ( "I can't use > to compare a " ++ showUnwrapped (typeOf e1 env)
              ++ " with a " ++ showUnwrapped (typeOf e2 env)
              ++ ". Both sides need to be the same type."
          )
    | typeOf e1 env == Right KInt    = Right KBool
    | typeOf e1 env == Right KFloat  = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise =
        Left $ TypeError
          ( "I can't use > with " ++ showUnwrapped (typeOf e1 env)
              ++ ". Only wholeNumbers, decimalNumbers, and text can be compared with >."
          )
  typeOf (LessEq e1 e2) env
    | typeOf e1 env /= typeOf e2 env =
        Left $ TypeError
          ( "I can't use <= to compare a " ++ showUnwrapped (typeOf e1 env)
              ++ " with a " ++ showUnwrapped (typeOf e2 env)
              ++ ". Both sides need to be the same type."
          )
    | typeOf e1 env == Right KInt    = Right KBool
    | typeOf e1 env == Right KFloat  = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise =
        Left $ TypeError
          ( "I can't use <= with " ++ showUnwrapped (typeOf e1 env)
              ++ ". Only wholeNumbers, decimalNumbers, and text can be compared with <=."
          )
  typeOf (GreaterEq e1 e2) env
    | typeOf e1 env /= typeOf e2 env =
        Left $ TypeError
          ( "I can't use >= to compare a " ++ showUnwrapped (typeOf e1 env)
              ++ " with a " ++ showUnwrapped (typeOf e2 env)
              ++ ". Both sides need to be the same type."
          )
    | typeOf e1 env == Right KInt    = Right KBool
    | typeOf e1 env == Right KFloat  = Right KBool
    | typeOf e1 env == Right KString = Right KBool
    | otherwise =
        Left $ TypeError
          ( "I can't use >= with " ++ showUnwrapped (typeOf e1 env)
              ++ ". Only wholeNumbers, decimalNumbers, and text can be compared with >=."
          )
  typeOf (If condition ifblock) env
    | typeOf condition env /= Right KBool =
        Left $ TypeError
          ( "The condition after 'if' needs to be a truth value, but I got a "
              ++ showUnwrapped (typeOf condition env) ++ "."
          )
    | null ifblock = Right KVoid
    | otherwise = case foldM updateTypeEnv' env ifblock of
        Left err   -> Left err
        Right env' -> typeOf (last ifblock) env'
  typeOf (IfElse condition ifblock elseblock) env
    | typeOf condition env /= Right KBool =
        Left $ TypeError
          ( "The condition after 'if' needs to be a truth value, but I got a "
              ++ showUnwrapped (typeOf condition env) ++ "."
          )
    | otherwise = case ifTypeOrErr of
        Left err     -> Left err
        Right typeIf -> case elseTypeOrErr of
          Left err       -> Left err
          Right typeElse -> Right $ OneOf typeIf typeElse
    where
      ifTypeOrErr   = checkIfType ifblock
      elseTypeOrErr = checkIfType elseblock
      checkIfType [] = Right KVoid
      checkIfType statements =
        case foldM updateTypeEnv' env statements of
          Left err   -> Left err
          Right env' -> typeOf (last statements) env'
  typeOf (UnwrapAs expr wantedType name block) env = do
    t <- typeOf expr env
    case t of
      OneOf a b ->
        if wantedType == a || wantedType == b
          then do
            let narrowedEnv = env {_varTypes = M.insert name wantedType (_varTypes env)}
            checkBlockType block narrowedEnv
          else Left $ TypeError
            ( "I can't unwrap this as " ++ show wantedType
                ++ " — the value is " ++ show t ++ "."
            )
      other -> Left $ TypeError
        ( "'unwrap' only works with 'one of' values, but I got a "
            ++ show other ++ "."
        )
  typeOf (While condition whileblock) env
    | typeOf condition env /= Right KBool =
        Left $ TypeError
          ( "The condition after 'while' needs to be a truth value, but I got a "
              ++ showUnwrapped (typeOf condition env) ++ "."
          )
    | null whileblock = Right KVoid
    | otherwise = case foldM updateTypeEnv' env whileblock of
        Left err   -> Left err
        Right env' -> typeOf (last whileblock) env'
  typeOf (Print (StrLit _)) _ = Right KVoid
  typeOf (Print (Letter _)) _ = Right KVoid
  typeOf (Print (IntLit _)) _ =
    Left $ TypeError
      "I can't print a wholeNumber directly. Try: print(toText(yourNumber))"
  typeOf (Print (FloatLit _)) _ =
    Left $ TypeError
      "I can't print a decimalNumber directly. Try: print(toText(yourNumber))"
  typeOf (Print (BoolLit _)) _ =
    Left $ TypeError
      "I can't print a truth value directly. Try: print(toText(yourValue))"
  typeOf (Print (DefType _)) _ =
    Left $ TypeError "I can't print a definition."
  typeOf (Print (Print _)) _ =
    Left $ TypeError "I can't print a print statement."
  typeOf (Print (Parens x)) e = typeOf (Print x) e
  typeOf (Print x) e = case typeOf x e of
    Left err        -> Left err
    Right KString   -> Right KVoid
    t ->
      Left $ TypeError
        ( "I can't print a " ++ showUnwrapped t
            ++ ". Only text and letters can be printed. Try wrapping it with toText()."
        )
  typeOf (ToText inner) env = do
    t <- typeOf inner env
    case t of
      KInt    -> Right KString
      KFloat  -> Right KString
      KBool   -> Right KString
      KChar   -> Right KString
      KString -> Right KString
      other   -> Left $ TypeError
        ( "I can't convert a " ++ show other
            ++ " to text. toText() works with wholeNumbers, decimalNumbers, truth values, and letters."
        )
  typeOf (ToNum inner) env = do
    t <- typeOf inner env
    case t of
      KString -> Right KInt
      other   -> Left $ TypeError
        ( "I can only convert text to a wholeNumber, but I got a "
            ++ show other ++ "."
        )
  typeOf (List xs) e = case traverse (`typeOf` e) xs of
    Left typerr -> Left typerr
    Right []    -> Right $ KList KVoid
    Right (t : ts) ->
      if all (== t) ts
        then Right $ KList t
        else Left $ TypeError "All items in a list need to be the same type, but I found a mix."
  typeOf (Push x xs) e = case typeOf xs e of
    Left typerr       -> Left typerr
    Right (KList KVoid) -> KList <$> typeOf x e
    Right (KList t)   -> case typeOf x e of
      Left typeerr -> Left typeerr
      Right t1 ->
        if t == t1
          then Right $ KList t
          else Left $ TypeError
            ( "I can't add a " ++ show t1
                ++ " to a list of " ++ show t
                ++ ". All items in a list need to be the same type."
            )
    _ -> Left $ TypeError "push() needs a list as its second input, but I got something else."
  typeOf (Pop inner) e = case typeOf inner e of
    Left err            -> Left err
    Right (KList KVoid) -> Left $ TypeError "I can't pop from an empty list — there's nothing in it!"
    Right (KList t)     -> Right t
    Right other         -> Left $ TypeError ("pop() needs a list, but I got a " ++ show other ++ ".")
  typeOf (Letters inner) env = do
    t <- typeOf inner env
    case t of
      KString -> Right (KList KChar)
      other   -> Left $ TypeError
        ("letters() only works on text, but I got a " ++ show other ++ ".")

checkBlockType :: [KittyAST] -> TypeEnv -> Either KittyError KType
checkBlockType [] _ = Right KVoid
checkBlockType statements env =
  case foldM updateTypeEnv' env statements of
    Left err   -> Left err
    Right env' -> typeOf (last statements) env'

instance TypeCheckable FunctionCall where
  typeOf :: FunctionCall -> TypeEnv -> Either KittyError KType
  typeOf (FunctionCall fnName args) env =
    case M.lookup fnName (_functionTypes env) of
      Nothing -> Left $ UndefinedError
        ("I don't know a function called " ++ fnName ++ " — did you define it?")
      Just (KFun (paramTypes, [retType])) ->
        if length args /= length paramTypes
          then Left $ TypeError (argCountError fnName (length paramTypes) (length args))
          else do
            argTypes <- mapM (`typeOf` env) args
            case filter (\(got, expected) -> got /= expected) (zip argTypes paramTypes) of
              []               -> Right retType
              (got, expected):_ -> Left $ TypeError
                ( "The input to " ++ fnName ++ " should be a " ++ show expected
                    ++ ", but I got a " ++ show got ++ "."
                )
      Just _ -> Left $ TypeError (fnName ++ " is not a function.")

instance TypeCheckable Definition where
  typeOf (AssignDef varname e) env = typeOf e env
  typeOf (FunctionDef (FunctionDefinition name params retType body)) env = do
    let paramEnv = foldl
          (\e (pName, pType) -> e {_varTypes = M.insert pName pType (_varTypes e)})
          env params
    bodyType <- checkBlockType body paramEnv
    if bodyType == retType
      then Right KVoid
      else if null body
        then Left $ TypeError
          ( "The function '" ++ name ++ "' says it returns a " ++ show retType
              ++ ", but there's nothing in the body."
          )
        else Left $ TypeError
          ( "The function '" ++ name ++ "' says it returns a " ++ show retType
              ++ ", but the body gives back a " ++ show bodyType ++ "."
          )

{-type checking functions functions-}

-- | parsers and type checks
typeCheck :: TypeEnv -> T.Text -> Either KittyError KType
typeCheck env text = case parseAsAST text of
  Left _    -> Left $ ParseError $ T.unpack text
  Right ast -> typeOf ast env

-- | parses, typechecks, then prints result
typeCheckPrint :: T.Text -> String
typeCheckPrint = show . typeCheck initialTypeEnv

-- | Type checks the parsed text and converts the error or the type to a string
typeCheckOutput :: TypeEnv -> T.Text -> String
typeCheckOutput env text = case typeCheck env text of
  Right x -> show x
  Left x  -> show x

-- | handles different cases of the AST types to update type environment,
-- | and return type
-- | updates Type environment only if a definition
-- | or if/else block is encountered
-- | otherwise, keeps the type enviornment unchanged
-- | also returns the type of the expression for type checking
updateTypeEnv :: TypeEnv -> T.Text -> Either KittyError (TypeEnv, KType)
updateTypeEnv tenv text =
  case parseAsAST text of
    Left _ -> Left $ ParseError (T.unpack text)
    Right (DefType (AssignDef varname e)) ->
      case typeCheck tenv text of
        Right t ->
          Right (tenv {_varTypes = M.insert varname t (_varTypes tenv)}, t)
        Left err -> Left err
    Right (DefType (FunctionDef fd)) ->
      case typeOf (DefType (FunctionDef fd)) tenv of
        Right KVoid ->
          let funType = KFun (map snd (_funcParams fd), [_funcRetType fd])
          in Right (tenv {_functionTypes = M.insert (_funcName fd) funType (_functionTypes tenv)}, KVoid)
        Left err -> Left err
        Right _   -> Left $ UndefinedError "unexpected return type from function definition"
    Right (If c e) ->
      case typeCheck tenv text of
        Right t ->
          makeEitherTuple
            (unifyMaybeTypeEnvs (updateManyTypeEnv tenv e) (Right tenv))
            t
        Left err -> Left err
    Right (IfElse c i e) -> case typeCheck tenv text of
      Right t ->
        makeEitherTuple
          ( unifyMaybeTypeEnvs
              (updateManyTypeEnv tenv i)
              (updateManyTypeEnv tenv e)
          )
          t
      Left err -> Left err
    Right ast -> (,) tenv <$> typeOf ast tenv

-- | combines two typeEnvs by combining their variable type lookup tables
-- | if variable names collide, creates a OneOf of both types if types are different
unifyTypeEnvs :: TypeEnv -> TypeEnv -> TypeEnv
unifyTypeEnvs tenv1 tenv2 =
  tenv1
    { _varTypes =
        M.map (unwrapOneOfIfEqual . oneOfVoidIfMissing) $
          M.unionWith OneOf (_varTypes tenv1) (_varTypes tenv2)
    }

-- | wraps two types in a OneOf if and only if they are different
oneOfIfDifferent :: KType -> KType -> KType
oneOfIfDifferent type1 type2 =
  if type1 == type2 then type1 else OneOf type1 type2

-- | unwraps OneOf if the two contained types are equal
unwrapOneOfIfEqual :: KType -> KType
unwrapOneOfIfEqual (OneOf type1 type2) =
  if type1 == type2 then type1 else OneOf type1 type2
unwrapOneOfIfEqual x = x

-- | wraps a KType in OneOf, adding KVoid for the second type
oneOfVoidIfMissing :: KType -> KType
oneOfVoidIfMissing (OneOf a b) = OneOf a b
oneOfVoidIfMissing x = OneOf x KVoid

-- | unifies two values of Type Either a TypeEnv into one
unifyMaybeTypeEnvs ::
  Either a TypeEnv ->
  Either a TypeEnv ->
  Either a TypeEnv
unifyMaybeTypeEnvs (Left e) _ = Left e
unifyMaybeTypeEnvs _ (Left e) = Left e
unifyMaybeTypeEnvs (Right e1) (Right e2) =
  Right (unifyTypeEnvs e1 e2)

-- | helper function for making a tuple of two values
-- | where the first is an Either a b
makeEitherTuple :: Monad f => f a1 -> a2 -> f (a1, a2)
makeEitherTuple eithera b = (,) <$> eithera <*> return b

-- | fold the updateTupeEnv' function
-- | over a foldable of KittyAST (e.g. a list)
updateManyTypeEnv ::
  Foldable t =>
  TypeEnv ->
  t KittyAST ->
  Either KittyError TypeEnv
updateManyTypeEnv tenv e =
  foldl
    (\tenv' e' -> tenv' >>= flip updateTypeEnv' e')
    (return tenv)
    e

-- | inserts a variable into the type environment
-- | and produces a new type environment or an error
updateTypeEnv' :: TypeEnv -> KittyAST -> Either KittyError TypeEnv
updateTypeEnv' tenv (DefType (AssignDef varname e)) =
  case typeOf e tenv of
    Right t  -> Right (tenv {_varTypes = M.insert varname t (_varTypes tenv)})
    Left err -> Left err
updateTypeEnv' tenv (DefType (FunctionDef fd)) =
  case typeOf (DefType (FunctionDef fd)) tenv of
    Right KVoid ->
      let funType = KFun (map snd (_funcParams fd), [_funcRetType fd])
      in Right (tenv {_functionTypes = M.insert (_funcName fd) funType (_functionTypes tenv)})
    Left err -> Left err
    Right _  -> Left $ UndefinedError "unexpected return type from function definition"
updateTypeEnv' tenv e = case typeOf e tenv of
  Right _  -> Right tenv
  Left err -> Left err

{-helper functions-}

-- | produces a string of the value after unwrapping from Either
showUnwrapped :: (Show a, Show b) => Either a b -> String
showUnwrapped (Left x)  = show x
showUnwrapped (Right y) = show y

-- | builds the wrong-argument-count error message
argCountError :: String -> Int -> Int -> String
argCountError name expected given =
  name ++ " needs " ++ show expected ++ inputs expected ++ suffix
  where
    inputs 1 = " input"
    inputs _ = " inputs"
    suffix
      | given == 0           = ", but you didn't give it any."
      | given < expected     = ", but you only gave it " ++ show given ++ "."
      | otherwise            = ", but you gave it " ++ show given ++ "."
