module Evaluator (parseEvalAndPrintResult, parseEvalPrintMultiline, parseRepl, initialEnv) where

import qualified Data.Map as M
import qualified Data.Text as T
import KittyTypes
import Parser
import Text.Parsec.Error

initialEnv :: Env
initialEnv = Env M.empty M.empty None

eval :: Env -> KittyAST -> Either KittyError Env
eval env (Expr e) = case evalExpression env e of
  Left err -> Left err
  Right exp -> Right $ env {_tmpResult = Expr exp}
-- considering disallowing re-assigning to make immutable,
-- but currently mutability is allowed
eval env (DefType (AssignDef varname vardef)) = Right $ env {_variables = M.insert varname vardef (_variables env)}

evalExpression :: Env -> ArithExpr -> Either KittyError ArithExpr
evalExpression _ (IntLit i) = Right $ IntLit i
evalExpression _ (Exp op (IntLit i) (IntLit j)) = Right $ IntLit $ evalOp op i j
evalExpression _ (FloatLit i) = Right $ FloatLit i
evalExpression _ (Exp op (FloatLit i) (FloatLit j)) = Right $ FloatLit $ evalOp op i j
evalExpression _ (Exp op (IntLit i) (FloatLit j)) = Left $ TypeError $ show i ++ " has type integer, which can't be combined with the operator " ++ show j ++ ", a value of type float with " ++ return (opSymb op)
evalExpression _ (Exp op (FloatLit i) (IntLit j)) = Left $ TypeError $ show i ++ " has type float, which can't be combined with " ++ show j ++ ", a value of type integer, with the operator " ++ return (opSymb op) -- type names might change
evalExpression env (Exp op e1 e2) = case evalExpression env e1 of
  Left err -> Left err
  Right exp1 -> case evalExpression env e2 of
    Left err -> Left err
    Right exp2 -> evalExpression env (Exp op exp1 exp2)
evalExpression env (Parens e) = evalExpression env e
evalExpression env (Variable v) = evalVariable v env >>= eval2ArithExpr

evalVariable :: String -> Env -> Either KittyError KittyAST
evalVariable v env = case M.lookup v (_variables env) of
  Nothing -> Left $ DoesNotExistError $ "the variable " ++ v ++ " does not exist"
  Just val -> Right val

eval2ArithExpr :: KittyAST -> Either KittyError ArithExpr
eval2ArithExpr (Expr a) = Right a
eval2ArithExpr x = Left $ TypeError $ toOutput x ++ "can't be interpreted as an arithmetic expression"

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
  Right asts -> evalMultiple  initialEnv asts
  Left _ -> Left $  KittyTypes.ParseError  (T.unpack text)

parseEvalPrintMultiline :: T.Text -> IO ()
parseEvalPrintMultiline text = case parseEvalMultiline text of 
  Left err -> print err 
  Right env -> putStrLn $ toOutput $ _tmpResult env 

parseRepl :: Env -> T.Text -> Either KittyError Env
parseRepl env text  = case traverse parseAsAST $ T.lines text of 
  Right asts -> evalMultiple  env asts
  Left _ -> Left $  KittyTypes.ParseError  (T.unpack text)