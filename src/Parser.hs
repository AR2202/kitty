{-# LANGUAGE OverloadedStrings #-}

module Parser (intParser, parseAsInt, parseAsFloat, parseAsOperator, parseAsExp, parseAsAST, parseAsBool) where

-- import qualified Data.Text as T

-- import Text.Parsec.Expr

import qualified Data.Text as T
import KittyTypes
import Text.Parsec
import qualified Text.Parsec.Language as Lang
import Text.Parsec.Text
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Combinator

{- This is the Parser for the kitty language.
Parsing from Test directly into AST type without lexing step-}
{-Parsing arithmetic expressions-}
intParser :: Parser ArithExpr
intParser = IntLit . read <$> (spaces *> (many1 digit <|> ((++) <$> string "-" <*> many1 digit)) <* spaces)

floatParser :: Parser ArithExpr
floatParser = FloatLit . read <$> ((++) <$> (spaces *> (many1 digit <|> ((++) <$> string "-" <*> many1 digit))) <*> ((:) <$> char '.' <*> many1 digit) <* spaces)

operatorParser :: Parser Operator
operatorParser =
  parseWhichOperator <$> (spaces *> (oneOf "+-*/" <* spaces))

-- | parsing Operators into the ArithExpr type's data constructors
parseWhichOperator :: Char -> Operator
parseWhichOperator '+' = Add
parseWhichOperator '-' = Sub
parseWhichOperator '*' = Mult
parseWhichOperator '/' = Div

addop :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
addop =
  Exp Add
    <$ char
      '+'

subop :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
subop =
  Exp Sub
    <$ char
      '-'

addsub :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
addsub = addop <|> subop

mulop :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
mulop =
  Exp Mult
    <$ char
      '*'

divop :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
divop =
  Exp Div
    <$ char
      '/'

muldiv :: Parser (ArithExpr -> ArithExpr -> ArithExpr)
muldiv = mulop <|> divop

-- | parsing multiplication or division with left association
mulParser :: Parser ArithExpr
mulParser = chainl1 (try parensParser <|> try floatParser <|> try intParser <|> varParser) muldiv

-- | parsing addition or subtraction as left associative
addParser :: Parser ArithExpr
addParser = chainl1 (try mulParser <|> try parensParser <|> try floatParser <|> try intParser <|> varParser) addsub

-- | parsing parentheses
parensParser :: Parser ArithExpr
parensParser = Parens <$> between (spaces *> char '(' <* spaces) (spaces *> char ')' <* spaces) exprParser

-- | parsing an Arithmetic expression
exprParser :: Parser ArithExpr
exprParser = try addParser <|> try mulParser <|> try parensParser <|> try floatParser <|> try intParser <|> varParser

astArithParser :: Parser KittyAST
astArithParser = Expr <$> exprParser

{-Parsing Bool-}

{-BoolLit Bool | And BoolExpr BoolExpr | Or BoolExpr BoolExpr | Not BoolExpr | Var String | Xor BoolExpr BoolExpr | FCall FunctionCall-}

trueParser :: Parser BoolExpr
trueParser = BoolLit True <$ (spaces *> string "true" >> notFollowedBy alphaNum <* spaces)

falseParser :: Parser BoolExpr
falseParser = BoolLit False <$ (spaces *> string "false" >> notFollowedBy alphaNum <* spaces)

notParser :: Parser BoolExpr
notParser = Not <$> (spaces *> string "not" *> spaces *> boolParser)

boolParser :: Parser BoolExpr
boolParser = try boolopParser <|> try notParser <|> try falseParser <|> trueParser

andop :: Parser (BoolExpr -> BoolExpr -> BoolExpr)
andop =
  And
    <$ ( string
           "and"
           >> notFollowedBy alphaNum
       )

orop :: Parser (BoolExpr -> BoolExpr -> BoolExpr)
orop =
  Or
    <$ ( string
           "or"
           >> notFollowedBy alphaNum
       )

xorop :: Parser (BoolExpr -> BoolExpr -> BoolExpr)
xorop =
  Xor
    <$ ( string
           "xor"
           >> notFollowedBy alphaNum
       )

boolop :: Parser (BoolExpr -> BoolExpr -> BoolExpr)
boolop = andop <|> xorop <|> orop

-- | parsing boolean operations with left association
boolopParser :: Parser BoolExpr
boolopParser = chainl1 (try notParser <|> try falseParser <|>  trueParser ) boolop

-- | parsing boolean operations with left association
boolopvarParser :: Parser BoolExpr
boolopvarParser = chainl1 (try notParser <|> try falseParser <|> try trueParser <|> boolvarParser) boolop

-- | parse variables
boolvarParser :: Parser BoolExpr
boolvarParser = Var <$> (spaces *> many1 alphaNum <* spaces)


{- Parsing comparisons -}



eqop :: Parser  CompOp
eqop =
  Equal<$>(astSubParser
    <* string
      "==")<*> astSubParser
notEqop :: Parser  CompOp
notEqop =
  NotEqual<$>(astSubParser
    <* string
      "=/=")<*> astSubParser
lessEqop :: Parser  CompOp
lessEqop =
  LessEq<$>(astSubParser
    <* string
      "<=")<*> astSubParser
lessop :: Parser  CompOp
lessop =
  Less<$>(astSubParser
    <* string
      "<")<*> astSubParser
greaterEqop :: Parser  CompOp
greaterEqop =
  GreaterEq<$>(astSubParser
    <* string
      ">=")<*> astSubParser
greaterop :: Parser  CompOp
greaterop =
  Greater<$>(astSubParser
    <* string
      ">")<*> astSubParser

compParser :: Parser CompOp
compParser = try greaterop <|> try lessop <|> try greaterEqop<|> try lessEqop <|> try eqop<|> notEqop

{-Parsing assignments-}

-- | assigning variables
assignmentParser :: Parser Definition
assignmentParser = AssignDef <$> (spaces *> many1 alphaNum <* spaces) <* (char '=' >> noneOf "=<>") <*> (spaces *> (try astBoolParser <|> try astArithParser <|> astBoolVarParser) <* spaces)

-- | parses assignment as DefType variant of AST
astAssignParser :: Parser KittyAST
astAssignParser = DefType <$> assignmentParser

-- | parses assignment as DefType variant of AST
astBoolParser :: Parser KittyAST
astBoolParser = Boolean <$> boolParser
-- | parses assignment as DefType variant of AST
astBoolVarParser :: Parser KittyAST
astBoolVarParser = Boolean <$> boolopvarParser

astCompParser :: Parser KittyAST 
astCompParser = Comp <$> compParser
-- | parse variables
varParser :: Parser ArithExpr
varParser = Variable <$> (spaces *> many1 alphaNum <* spaces)

-- | parses any AST variant
astParser :: Parser KittyAST
astParser = try astAssignParser <|> try astCompParser<|> try astBoolParser <|> try astArithParser <|>  astBoolVarParser 

-- | parses  AST subexpression for use within CompOp
astSubParser :: Parser KittyAST
astSubParser =  try astBoolParser <|> try astArithParser <|> astBoolVarParser
{- helper function to test parsing in the console-}

-- | parsing an int from console input - no file
parseAsInt :: T.Text -> Either ParseError ArithExpr
parseAsInt = parse intParser "no file"

parseAsFloat :: T.Text -> Either ParseError ArithExpr
parseAsFloat = parse floatParser "no file"

-- | parsing an Operator from console input - no file
parseAsOperator :: T.Text -> Either ParseError Operator
parseAsOperator = parse operatorParser "no file"

-- | parsing an Expression from console input - no file
parseAsExp :: T.Text -> Either ParseError ArithExpr
parseAsExp = parse exprParser "no file"

-- | parsing an ast input - no file
parseAsAST :: T.Text -> Either ParseError KittyAST
parseAsAST = parse astParser "no file"

-- | parsing a Boolean Expression from console input - no file
parseAsBool :: T.Text -> Either ParseError BoolExpr
parseAsBool = parse boolParser "no file"
