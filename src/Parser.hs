{-# LANGUAGE OverloadedStrings #-}

module Parser (intParser, parseUnwrap, parseAsInt, parseAsFloat, parseAsAdd, parseAsOperator, parseAsASTTest, parseAsBoolOp, parseAsAST, parseAsIf, parseAsIfCond, parseIfBlock) where

-- import qualified Data.Text as T

-- import Text.Parsec.Expr

import Control.Monad (guard, void)
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
intParser =
  IntLit . read
    <$> (spaces *> (many1 digit <|> ((++) <$> string "-" <*> many1 digit)) <* spaces)

floatParser :: Parser KittyAST
floatParser =
  FloatLit . read
    <$> ((++) <$> (spaces *> (many1 digit <|> ((++) <$> string "-" <*> many1 digit))) <*> ((:) <$> char '.' <*> many1 digit) <* spaces)

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
addsub = addop <|> subop <|> boolop

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

chainl1' ::
  ParsecT s u m t ->
  ParsecT s u m (t -> t -> t) ->
  ParsecT s u m b
chainl1' p op = do
  x <- p
  rest x
  where
    rest x =
      ( do
          f <- op
          y <- p
          rest (f x y)
      )
        <|> parserFail "Operator required"

muldiv :: Parser (KittyAST -> KittyAST -> KittyAST)
muldiv = mulop <|> divop

-- | parsing multiplication or division with left association
mulParser :: Parser KittyAST
mulParser =
  chainl1
    ( try parensParser
        <|> try floatParser
        <|> try intParser
        <|> varParser
    )
    muldiv

-- | parsing addition or subtraction as left associative
addParser :: Parser KittyAST
addParser =
  chainl1
    ( try notParser
        <|> try trueParser
        <|> try falseParser
        <|> try mulParser
        <|> try parensParser
        <|> try floatParser
        <|> try intParser
        <|> try stringParser
        <|> try charParser
        <|> varParser
    )
    addsub

-- | parsing parentheses
parensParser :: Parser KittyAST
parensParser =
  Parens
    <$> between
      (spaces *> char '(' <* spaces)
      (spaces *> char ')' <* spaces)
      astParser

{- strings -}

-- | parsing strings
stringParser :: Parser KittyAST
stringParser =
  StrLit
    <$> between
      (spaces *> char '"')
      (char '"' <* spaces)
      (many (noneOf "\"\'"))

-- | parsing chars
charParser :: Parser KittyAST
charParser =
  Letter
    <$> between
      (spaces *> char '\'')
      (char '\'' <* spaces)
      anyChar

{-Parsing Bool-}

trueParser :: Parser KittyAST
trueParser =
  BoolLit True
    <$ (spaces *> string "true" >> notFollowedBy alphaNum <* spaces)

falseParser :: Parser KittyAST
falseParser =
  BoolLit False
    <$ (spaces *> string "false" >> notFollowedBy alphaNum <* spaces)

notParser :: Parser KittyAST
notParser =
  Not
    <$> (spaces *> string "not" *> spaces *> astSubParser)

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
boolopParser =
  chainl1
    ( try parensParser
        <|> try notParser
        <|> try falseParser
        <|> try trueParser
        <|> varParser
    )
    boolop

-- | parsing boolean operations with left association

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
compParser =
  try greaterop
    <|> try lessop
    <|> try greaterEqop
    <|> try lessEqop
    <|> try eqop
    <|> notEqop

{-Parsing assignments-}

-- | assigning variables
assignmentParser :: Parser Definition
assignmentParser =
  AssignDef
    <$> (spaces *> many1 alphaNum <* spaces)
    <* (char '=' >> noneOf "=<>")
    <*> ( spaces
            *> ( try elseParser
                   <|> try ifParser
                   <|> try whileParser
                   <|> try stringParser
                   <|> try charParser
                   <|> try astSubParser
                   <|> try boolopParser
                   <|> try addParser
                   <|> try mulParser
                   <|> try compParser
                   <|> try trueParser
                   <|> try falseParser
                   <|> try floatParser
                   <|> try intParser
                   <|> varParser
               )
            <* spaces
        )

-- | parses assignment as DefType variant of AST
astAssignParser :: Parser KittyAST
astAssignParser = DefType <$> assignmentParser

{-if statements-}

-- | if parser
ifParser :: Parser KittyAST
ifParser = If <$> ifCondParser <*> ifBlockParser

elseParser :: Parser KittyAST
elseParser =
  IfElse
    <$> ifCondParser <*> ifBlockParser <*> elseBlockParser

whileParser :: Parser KittyAST
whileParser =
  While
    <$> whileCondParser <*> whileBlockParser

ifCondParser =
  between
    (spaces *> string "if" <* spaces)
    (lookAhead (spaces *> string "then" <* spaces))
    astSubParser''

whileCondParser =
  between
    (spaces *> string "while" <* spaces)
    (lookAhead (spaces *> string "do" <* spaces))
    astSubParser

ifBlockParser =
  between
    (spaces *> string "then" <* spaces)
    ( spaces *> (try (string "endif"))
        <|> (lookAhead (string "else")) <* spaces
    )
    (many astSubParser'')

elseBlockParser =
  between
    (spaces *> string "else" <* spaces)
    (spaces *> string "endif" <* spaces)
    (many astSubParser'')

whileBlockParser =
  between
    (spaces *> string "do" <* spaces)
    (spaces *> string "endwhile" <* spaces)
    (many astSubParser'')

-- | parse unwrap
unwrapVarParser =
  between
    (spaces *> string "unwrap" <* spaces)
    (lookAhead (spaces *> string "as" <* spaces))
    varParser

unwrappedTypeParser =
  between
    (spaces *> string "as" <* spaces)
    (spaces *> string "named" <* spaces)
    typeParser

unwrapParser =
  UnwrapAs
    <$> unwrapVarParser
      <*> unwrappedTypeParser
      <*> many1 alphaNum
      <*> unwrapBlockParser

-- | type name parser
-- no support for OneOf yet
typeParser :: Parser KType
typeParser =
  read
    <$> ( try (string "wholeNumber")
            <|> try (string "truth")
            <|> try (string "decimalNumber")
            <|> try (string "text")
            <|> try
              ( string "empty"
              )
            <|> string "letter"
        )

unwrapBlockParser =
  between
    (spaces *> string "andDo" <* spaces)
    (spaces *> string "endunwrap" <* spaces)
    (many astSubParser'')

-- | parse variables
varParser :: Parser KittyAST
varParser = do
  void spaces
  firstChar <- alphaNum
  rest <- many alphaNum
  let varname = firstChar : rest
  void space <|> eof <|> void endOfLine <|> void tab
  void spaces
  guard (varname `notElem` keywords)
  return $ Variable varname
  where
    keywords =
      [ "if",
        "else",
        "endif",
        "then",
        "endunwrap",
        "andDo",
        "do",
        "while",
        "endwhile",
        "print"
      ]

printParser :: Parser KittyAST
printParser =
  Print
    <$> between
      (spaces *> string "print" <* spaces <* string "(" <* spaces)
      (spaces *> string ")" <* spaces)
      astSubParser''

-- | parses any AST variant
astParser :: Parser KittyAST
astParser =
  try printParser
    <|> try elseParser
    <|> try ifParser
    <|> try whileParser
    <|> try unwrapParser
    <|> try astAssignParser
    <|> try compParser
    <|> try addParser
    <|> try mulParser
    <|> try charParser
    <|> try stringParser
    <|> try notParser
    <|> try falseParser
    <|> trueParser
    <|> try floatParser
    <|> try intParser
    <|> varParser

astTestParser :: Parser KittyAST
astTestParser =
  try elseParser
    <|> try ifParser
    <|> try whileParser
    <|> try astAssignParser
    <|> try compParser
    <|> try boolopParser
    <|> try charParser
    <|> try stringParser
    <|> try notParser
    <|> try falseParser
    <|> trueParser
    <|> try floatParser
    <|> try intParser
    <|> varParser

-- | parses  AST subexpression for use within KittyAST
astSubParser :: Parser KittyAST
astSubParser =
  try addParser <|> try elseParser
    <|> try ifParser
    <|> try mulParser
    <|> try parensParser
    <|> try compParser
    <|> try charParser
    <|> try stringParser
    <|> try floatParser
    <|> try intParser
    <|> try trueParser
    <|> try falseParser
    <|> varParser

astSubParser'' :: Parser KittyAST
astSubParser'' =
  try printParser
    <|> try elseParser
    <|> try ifParser
    <|> try astAssignParser
    <|> try addParser
    <|> try mulParser
    <|> try parensParser
    <|> try charParser
    <|> try stringParser
    <|> try floatParser
    <|> try intParser
    <|> try trueParser
    <|> try falseParser
    <|> try varParser

-- | parses  AST subexpression for use within KittyAST
astSubParser' :: Parser KittyAST
astSubParser' =
  try addParser
    <|> try mulParser
    <|> try parensParser
    <|> try charParser
    <|> try stringParser
    <|> try floatParser
    <|> try intParser
    <|> try trueParser
    <|> try falseParser
    <|> varParser

{- helper function to test parsing in the console-}

-- | parsing an int from console input - no file
parseAsInt :: T.Text -> Either ParseError KittyAST
parseAsInt = parse intParser "no file"

parseAsFloat :: T.Text -> Either ParseError KittyAST
parseAsFloat = parse floatParser "no file"

-- | parsing an Operator from console input - no file
parseAsOperator :: T.Text -> Either ParseError Operator
parseAsOperator = parse operatorParser "no file"

parseAsBoolOp :: T.Text -> Either ParseError KittyAST
parseAsBoolOp = parse boolopParser "no file"

parseAsAdd :: T.Text -> Either ParseError KittyAST
parseAsAdd = parse addParser "no file"

-- | parsing an ast input - no file
parseAsAST :: T.Text -> Either ParseError KittyAST
parseAsAST = parse astParser "no file"

parseAsASTTest :: T.Text -> Either ParseError KittyAST
parseAsASTTest = parse astTestParser "no file"

parseAsIf :: T.Text -> Either ParseError KittyAST
parseAsIf = parse (try elseParser <|> ifParser) "no file"

parseAsIfCond :: T.Text -> Either ParseError KittyAST
parseAsIfCond = parse ifCondParser "no file"

parseIfBlock :: T.Text -> Either ParseError [KittyAST]
parseIfBlock = parse ifBlockParser "no file"

parseUnwrap = parse unwrapParser "no file"