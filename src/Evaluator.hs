module Evaluator (parseEvalAndPrintResult, parseEvalPrintMultiline, parseRepl, initialEnv) where

import qualified Data.Map as M
import qualified Data.Text as T
import KittyTypes
import Parser
import Text.Parsec.Error

initialEnv :: Env
initialEnv = Env M.empty M.empty None

eval :: Env -> KittyAST -> Either KittyError Env
eval env (Expr op e1 e2) = case evalExpression env (Expr op e1 e2) of
  Left err -> Left err
  Right exp -> Right $ env {_tmpResult = exp}
-- considering disallowing re-assigning to make immutable,
-- but currently mutability is allowed
eval env (DefType (AssignDef varname vardef)) = Right $ env {_variables = M.insert varname vardef (_variables env)}
eval env (BoolLit tf) = Right $ env {_tmpResult = BoolLit tf}
eval env (IntLit i) = Right $ env {_tmpResult = IntLit i}
eval env (FloatLit f) = Right $ env {_tmpResult = FloatLit f}
eval env (StrLit s) = Right $ env {_tmpResult = StrLit s}
eval env (And b1 b2) = case evalBoolean env (And b1 b2) of
  Left err -> Left err
  Right exp -> Right $ env {_tmpResult = exp}
eval env (Or b1 b2) = case evalBoolean env (Or b1 b2) of
  Left err -> Left err
  Right exp -> Right $ env {_tmpResult = exp}
eval env (Xor b1 b2) = case evalBoolean env (Xor b1 b2) of
  Left err -> Left err
  Right exp -> Right $ env {_tmpResult = exp}
eval env (Not b) = case evalBoolean env (Not b) of
  Left err -> Left err
  Right exp -> Right $ env {_tmpResult = exp}

evalBoolean env (BoolLit tf) = Right $ BoolLit tf
evalBoolean env (And b1 b2)
  | evalBoolean env b1 == Right (BoolLit True) && evalBoolean env b2 == Right (BoolLit True) = Right $ BoolLit True
  | otherwise = Right $ BoolLit False
evalBoolean env (Or b1 b2)
  | evalBoolean env b1 == Right (BoolLit True) || evalBoolean env b2 == Right (BoolLit True) = Right $ BoolLit True
  | otherwise = Right $ BoolLit False
evalBoolean env (Xor b1 b2)
  -- could be replaced by evalBoolean env b1 /= evalBoolean env b2
  -- if we can be certain that it can't eval to anything other than BoolLit True or BoolLit False
  | evalBoolean env b1 == Right (BoolLit True) || evalBoolean env b2 == Right (BoolLit False) = Right $ BoolLit True
  | evalBoolean env b1 == Right (BoolLit False) || evalBoolean env b2 == Right (BoolLit False) = Right $ BoolLit True
  | otherwise = Right $ BoolLit False
evalBoolean env (Variable v) = evalVariable v env >>= evalBoolean env
evalBoolean env (Not (BoolLit True)) = Right $ BoolLit False
evalBoolean env (Not (BoolLit False)) = Right $ BoolLit True
evalBoolean env (Not b) = evalBoolean env b >>= \x -> evalBoolean env (Not x)

{- eval env (Boolean e) = case evalBoolean env e of
  Left err -> Left err
  Right exp -> Right $ env {_tmpResult =  exp} -}

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

evalVariable :: String -> Env -> Either KittyError KittyAST
evalVariable v env = case M.lookup v (_variables env) of
  Nothing -> Left $ DoesNotExistError $ "the variable " ++ v ++ " does not exist"
  Just val -> Right val

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
