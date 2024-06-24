module Evaluator (parseEvalAndPrintResult, parseEvalPrintMultiline, parseRepl, evalMultiple, initialEnv) where

import Data.Foldable (foldl1)
import qualified Data.Map as M
import qualified Data.Text as T
import KittyTypes
import Parser
import Text.Parsec.Error

{-This is the definition of the evaluation
a tree-walk interpreter to traverse and execute the AST-}

-- | initial empty program environment
initialEnv :: Env
initialEnv = Env M.empty M.empty None

-- | the evaluation function
eval :: Env -> KittyAST -> Either KittyError Env
eval env (DefType (AssignDef varname vardef)) = Right $ env {_variables = M.insert varname vardef (_variables env)}
eval env (If b e) = case evalExpression env b of
  Right (BoolLit False) -> Right env
  Right (BoolLit True) -> evalMultiple env e
  Left err -> Left err
  _ -> Left $ TypeError "Condition must have type truth"
eval env (IfElse b i e) = case evalExpression env b of
  Right (BoolLit False) -> evalMultiple env e
  Right (BoolLit True) -> evalMultiple env i
  Left err -> Left err
  _ -> Left $ TypeError "Condition must have type truth"
eval env e = case evalExpression env e of
  Left err -> Left err
  Right res -> Right $ env {_tmpResult = res}

-- | evaluates the AST to an error or a new AST
evalExpression :: Env -> KittyAST -> Either KittyError KittyAST
evalExpression _ (IntLit i) = Right $ IntLit i
evalExpression _ (Expr op (IntLit i) (IntLit j)) = Right $ IntLit $ evalOp op i j
evalExpression _ (FloatLit i) = Right $ FloatLit i
evalExpression _ (Expr op (FloatLit i) (FloatLit j)) = Right $ FloatLit $ evalOp op i j
evalExpression _ (Expr op (IntLit i) (FloatLit j)) = Left $ TypeError $ show i ++ " has type integer, which can't be combined with the operator " ++ show j ++ ", a value of type float with " ++ return (opSymb op)
evalExpression _ (Expr op (FloatLit i) (IntLit j)) = Left $ TypeError $ show i ++ " has type float, which can't be combined with " ++ show j ++ ", a value of type integer, with the operator " ++ return (opSymb op) -- type names might change
evalExpression env (Expr op e1 e2) = case evalExpression env e1 of
  Left err -> Left err
  Right exp1 -> case evalExpression env e2 of
    Left err -> Left err
    Right exp2 -> evalExpression env (Expr op exp1 exp2)
evalExpression env (Parens e) = evalExpression env e
evalExpression env (Variable v) = evalVariable v env >>= evalExpression env
evalExpression env (BoolLit tf) = Right $ BoolLit tf
evalExpression env (And b1 b2)
  | evalExpression env b1 == Right (BoolLit True) && evalExpression env b2 == Right (BoolLit True) = Right $ BoolLit True
  | otherwise = Right $ BoolLit False
evalExpression env (Or b1 b2)
  | evalExpression env b1 == Right (BoolLit True) || evalExpression env b2 == Right (BoolLit True) = Right $ BoolLit True
  | otherwise = Right $ BoolLit False
evalExpression env (Xor b1 b2)
  | evalExpression env b1 == Right (BoolLit True) || evalExpression env b2 == Right (BoolLit False) = Right $ BoolLit True
  | evalExpression env b1 == Right (BoolLit False) || evalExpression env b2 == Right (BoolLit False) = Right $ BoolLit True
  | otherwise = Right $ BoolLit False
evalExpression env (Variable v) = evalVariable v env >>= evalExpression env
evalExpression env (Not (BoolLit True)) = Right $ BoolLit False
evalExpression env (Not (BoolLit False)) = Right $ BoolLit True
evalExpression env (Not b) = evalExpression env b >>= \x -> evalExpression env (Not x)
evalExpression env (GreaterEq (IntLit i) (IntLit j)) = Right $ BoolLit (i >= j)
evalExpression env (GreaterEq (FloatLit i) (FloatLit j)) = Right $ BoolLit (i >= j)
evalExpression env (GreaterEq e1 e2) = case evalExpression env e1 of
  Left err -> Left err
  Right e -> evalExpression env e2 >>= \x -> evalExpression env (GreaterEq e x)
evalExpression env (LessEq (IntLit i) (IntLit j)) = Right $ BoolLit (i <= j)
evalExpression env (LessEq (FloatLit i) (FloatLit j)) = Right $ BoolLit (i <= j)
evalExpression env (LessEq e1 e2) = case evalExpression env e1 of
  Left err -> Left err
  Right e -> evalExpression env e2 >>= \x -> evalExpression env (LessEq e x)
evalExpression env (Greater (IntLit i) (IntLit j)) = Right $ BoolLit (i > j)
evalExpression env (Greater (FloatLit i) (FloatLit j)) = Right $ BoolLit (i > j)
evalExpression env (Greater e1 e2) = case evalExpression env e1 of
  Left err -> Left err
  Right e -> evalExpression env e2 >>= \x -> evalExpression env (Greater e x)
evalExpression env (Less (IntLit i) (IntLit j)) = Right $ BoolLit (i < j)
evalExpression env (Less (FloatLit i) (FloatLit j)) = Right $ BoolLit (i < j)
evalExpression env (Less e1 e2) = case evalExpression env e1 of
  Left err -> Left err
  Right e -> evalExpression env e2 >>= \x -> evalExpression env (Less e x)
evalExpression env (Equal (IntLit i) (IntLit j)) = Right $ BoolLit (i == j)
evalExpression env (Equal (FloatLit i) (FloatLit j)) = Right $ BoolLit (i == j)
evalExpression env (Equal (BoolLit i) (BoolLit j)) = Right $ BoolLit (i == j)
evalExpression env (Equal (StrLit i) (StrLit j)) = Right $ BoolLit (i == j)
evalExpression env (Equal (Letter i) (Letter j)) = Right $ BoolLit (i == j)
evalExpression env (Equal e1 e2) = case evalExpression env e1 of
  Left err -> Left err
  Right e -> evalExpression env e2 >>= \x -> evalExpression env (Equal e x)
evalExpression env (NotEqual (IntLit i) (IntLit j)) = Right $ BoolLit (i /= j)
evalExpression env (NotEqual (FloatLit i) (FloatLit j)) = Right $ BoolLit (i /= j)
evalExpression env (NotEqual (BoolLit i) (BoolLit j)) = Right $ BoolLit (i /= j)
evalExpression env (NotEqual (StrLit i) (StrLit j)) = Right $ BoolLit (i /= j)
evalExpression env (NotEqual (Letter i) (Letter j)) = Right $ BoolLit (i /= j)
evalExpression env (NotEqual e1 e2) = case evalExpression env e1 of
  Left err -> Left err
  Right e -> evalExpression env e2 >>= \x -> evalExpression env (NotEqual e x)
evalExpression env (StrLit s) = Right (StrLit s)
evalExpression env (Letter c) = Right (Letter c)
evalExpression env (IfElse b i e) = case evalExpression env b of
  Right (BoolLit False) -> evalMultipleExpr env e
  Right (BoolLit True) -> evalMultipleExpr env i
  Left err -> Left err

-- | looking up variable in program environment
evalVariable :: String -> Env -> Either KittyError KittyAST
evalVariable v env = case M.lookup v (_variables env) of
  Nothing -> Left $ DoesNotExistError $ "the variable " ++ v ++ " does not exist"
  Just val -> Right val

-- | evaluates the AST and converts the resulting environment to a string
-- | for debugging
evalAndPrintEnv :: Env -> KittyAST -> String
evalAndPrintEnv e a = show (eval e a)


evalAndPrintResult :: Env -> KittyAST -> Either KittyError String
evalAndPrintResult e a = toOutput . _tmpResult <$> eval e a

evalOp :: ArithOperations a => Operator -> a -> a -> a
evalOp Add = add
evalOp Sub = sub
evalOp Mult = mult
evalOp Div = divide

parseEvalAndPrintEnv :: T.Text -> Either ParseError String
parseEvalAndPrintEnv text = evalAndPrintEnv initialEnv <$> parseAsAST text

parseEvalAndPrintResult :: T.Text -> IO ()
parseEvalAndPrintResult text = case evalAndPrintResult initialEnv <$> parseAsAST text of
  Right (Right x) -> putStrLn x
  Right (Left err) -> print err
  Left err -> print err

evalMultiple :: Foldable t => Env -> t KittyAST -> Either KittyError Env
evalMultiple env astlist = foldl (\env' e -> env' >>= (flip eval) e) (Right env) astlist

evalMultipleExpr :: Foldable t => Env -> t KittyAST -> Either KittyError KittyAST
evalMultipleExpr env astlist = case evalMultiple env astlist of 
  Left err -> Left err 
  Right env' -> Right $ _tmpResult env'
parseEvalMultiline :: T.Text -> Either KittyError Env
parseEvalMultiline text = case traverse parseAsAST $ T.lines text of
  Right asts -> evalMultiple initialEnv asts
  Left _ -> Left $ KittyTypes.ParseError (T.unpack text)

parseEvalPrintMultiline :: T.Text -> IO ()
parseEvalPrintMultiline text = case parseEvalMultiline text of
  Left err -> print err
  Right env -> putStrLn $ toOutput $ _tmpResult env

parseRepl :: Env -> T.Text -> Either KittyError Env
parseRepl env text = case traverse parseAsAST $ T.lines text of
  Right asts -> evalMultiple env asts
  Left _ -> Left $ KittyTypes.ParseError (T.unpack text)
