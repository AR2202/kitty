{-# LANGUAGE OverloadedStrings #-}

module Parser (intParser, parseAsInt, parseAsFloat, parseAsOperator,  parseAsAST) where

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
intParser :: Parser KittyAST
intParser = IntLit . read <$> (spaces *> (many1 digit <|> ((++) <$> string "-" <*> many1 digit)) <* spaces)

floatParser :: Parser KittyAST
floatParser = FloatLit . read <$> ((++) <$> (spaces *> (many1 digit <|> ((++) <$> string "-" <*> many1 digit))) <*> ((:) <$> char '.' <*> many1 digit) <* spaces)

operatorParser :: Parser Operator
operatorParser =
  parseWhichOperator <$> (spaces *> (oneOf "+-*/" <* spaces))

-- | parsing Operators into the KittyAST type's data constructors
parseWhichOperator :: Char -> Operator
parseWhichOperator '+' = Add
parseWhichOperator '-' = Sub
parseWhichOperator '*' = Mult
parseWhichOperator '/' = Div

addop :: Parser (KittyAST -> KittyAST -> KittyAST)
addop =
  Expr Add
    <$ char
      '+'

subop :: Parser (KittyAST -> KittyAST -> KittyAST)
subop =
  Expr Sub
    <$ char
      '-'

addsub :: Parser (KittyAST -> KittyAST -> KittyAST)
addsub = addop <|> subop

mulop :: Parser (KittyAST -> KittyAST -> KittyAST)
mulop =
  Expr Mult
    <$ char
      '*'

divop :: Parser (KittyAST -> KittyAST -> KittyAST)
divop =
  Expr Div
    <$ char
      '/'

muldiv :: Parser (KittyAST -> KittyAST -> KittyAST)
muldiv = mulop <|> divop

-- | parsing multiplication or division with left association
mulParser :: Parser KittyAST
mulParser = chainl1 (try parensParser <|> try boolopParser <|> try floatParser <|> try intParser <|> try trueParser <|> try falseParser <|> varParser) muldiv

-- | parsing addition or subtraction as left associative
addParser :: Parser KittyAST
addParser = chainl1 (try mulParser <|> try parensParser <|> try boolopParser <|> try floatParser <|> try intParser <|> try trueParser <|> try falseParser <|> varParser) addsub

-- | parsing parentheses
parensParser :: Parser KittyAST
parensParser = Parens <$> between (spaces *> char '(' <* spaces) (spaces *> char ')' <* spaces) astParser

{- -- | parsing an Arithmetic expression
exprParser :: Parser KittyAST
exprParser = try addParser <|> try mulParser <|> try parensParser <|> try floatParser <|> try intParser <|> varParser -}



{-Parsing Bool-}

trueParser :: Parser KittyAST
trueParser = BoolLit True <$ (spaces *> string "true" >> notFollowedBy alphaNum <* spaces)

falseParser :: Parser KittyAST
falseParser = BoolLit False <$ (spaces *> string "false" >> notFollowedBy alphaNum <* spaces)

notParser :: Parser KittyAST
notParser = Not <$> (spaces *> string "not" *> spaces *> astSubParser)

{- boolParser :: Parser KittyAST
boolParser = try boolopParser <|> try notParser <|> try mulParser <|> try addParser <|> try falseParser <|> trueParser -}

andop :: Parser (KittyAST -> KittyAST -> KittyAST)
andop =
  And
    <$ ( string
           "and"
           >> notFollowedBy alphaNum
       )

orop :: Parser (KittyAST -> KittyAST -> KittyAST)
orop =
  Or
    <$ ( string
           "or"
           >> notFollowedBy alphaNum
       )

xorop :: Parser (KittyAST -> KittyAST -> KittyAST)
xorop =
  Xor
    <$ ( string
           "xor"
           >> notFollowedBy alphaNum
       )

boolop :: Parser (KittyAST -> KittyAST -> KittyAST)
boolop = andop <|> xorop <|> orop

-- | parsing boolean operations with left association
boolopParser :: Parser KittyAST
boolopParser = chainl1 (try parensParser <|>try notParser   <|> try falseParser <|> try trueParser<|> try floatParser <|> try intParser <|> varParser) boolop

-- | parsing boolean operations with left association

{- boolopvarParser :: Parser KittyAST
boolopvarParser = chainl1 (try notParser <|> try falseParser <|> try trueParser <|> boolvarParser) boolop

-- | parse variables I think this is redundant
boolvarParser :: Parser KittyAST
boolvarParser = Variable <$> (spaces *> many1 alphaNum <* spaces) -}

{- Parsing comparisons -}

eqop :: Parser KittyAST
eqop =
  Equal
    <$> ( astSubParser'
            <* string
              "=="
        )
    <*> astSubParser'

notEqop :: Parser KittyAST
notEqop =
  NotEqual
    <$> ( astSubParser'
            <* string
              "=/="
        )
    <*> astSubParser'

lessEqop :: Parser KittyAST
lessEqop =
  LessEq
    <$> ( astSubParser'
            <* string
              "<="
        )
    <*> astSubParser

lessop :: Parser KittyAST
lessop =
  Less
    <$> ( astSubParser'
            <* string
              "<"
        )
    <*> astSubParser'

greaterEqop :: Parser KittyAST
greaterEqop =
  GreaterEq
    <$> ( astSubParser'
            <* string
              ">="
        )
    <*> astSubParser'

greaterop :: Parser KittyAST
greaterop =
  Greater
    <$> ( astSubParser'
            <* string
              ">"
        )
    <*> astSubParser'

compParser :: Parser KittyAST
compParser = try greaterop <|> try lessop <|> try greaterEqop <|> try lessEqop <|> try eqop <|> notEqop

{-Parsing assignments-}

-- | assigning variables
assignmentParser :: Parser Definition
assignmentParser = AssignDef <$> (spaces *> many1 alphaNum <* spaces) <* (char '=' >> noneOf "=<>") <*> (spaces *> (try boolopParser 
 <|> 
 try addParser <|> try mulParser 
 <|> try compParser 
  <|> 
 try trueParser <|> try falseParser <|> try floatParser <|> try intParser <|> varParser) <* spaces)

-- | parses assignment as DefType variant of AST
astAssignParser :: Parser KittyAST
astAssignParser = DefType <$> assignmentParser



-- | parse variables
varParser :: Parser KittyAST
varParser = Variable <$> (spaces *> many1 alphaNum <* spaces)

-- | parses any AST variant
astParser :: Parser KittyAST
astParser = try astAssignParser <|> try compParser  <|> try addParser <|> try mulParser <|> try boolopParser<|> try falseParser <|> trueParser <|> try floatParser <|> try intParser <|> varParser

-- | parses  AST subexpression for use within KittyAST
astSubParser :: Parser KittyAST
astSubParser =  try boolopParser <|> try addParser <|> try mulParser <|> try parensParser <|> try compParser <|>try floatParser <|> try intParser <|> try trueParser <|> try falseParser <|> varParser
-- | parses  AST subexpression for use within KittyAST
astSubParser' :: Parser KittyAST
astSubParser' =   try addParser <|> try mulParser <|> try parensParser <|> try boolopParser <|>try floatParser <|> try intParser <|> try trueParser <|> try falseParser <|> varParser
{- helper function to test parsing in the console-}

-- | parsing an int from console input - no file
parseAsInt :: T.Text -> Either ParseError KittyAST
parseAsInt = parse intParser "no file"

parseAsFloat :: T.Text -> Either ParseError KittyAST
parseAsFloat = parse floatParser "no file"

-- | parsing an Operator from console input - no file
parseAsOperator :: T.Text -> Either ParseError Operator
parseAsOperator = parse operatorParser "no file"

{- -- | parsing an Expression from console input - no file
parseAsExp :: T.Text -> Either ParseError KittyAST
parseAsExp = parse exprParser "no file" -}

-- | parsing an ast input - no file
parseAsAST :: T.Text -> Either ParseError KittyAST
parseAsAST = parse astParser "no file"

{- -- | parsing a Boolean Expression from console input - no file
parseAsBool :: T.Text -> Either ParseError KittyAST
parseAsBool = parse boolParser "no file"
 -}