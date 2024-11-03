{-# LANGUAGE OverloadedStrings #-}

import Evaluator
import KittyTypes
import Parser
import Test.Hspec
import Test.QuickCheck
import Text.Parsec.Error
import TypeChecker

main :: IO ()
main = hspec $
  do
    -- Parsing
    -----------------------------------------
    parseAddition
    parseParens
    parseParensTimes
    parseLessThan
    parseAnd
    parseNestedBool
    parseboolParens
    parseboolParensLast
    parseBoolOp
    parseIfInWhile
    --addFailsWithoutAdd
    parseNumInsidePrint
    parseStrInsidePrint
    parseCharInsidePrint
    -- Type Checker---
    ------------------
    typeCheckCharInsidePrint
    typeCheckStrInsidePrint
    typeCheckBoolInsidePrint
    typeCheckAndInsidePrint
    typeCheckStrInsideParensPrint

parseAdd :: Expectation
parseAdd =
  parseAsAST "1 + 2"
    `shouldBe` Right (Expr Add (IntLit 1) (IntLit 2))

parseAddition :: SpecWith ()
parseAddition =
  describe "parseAsAST" $
    context "when parsing an addition expression" $
      it
        "should return Expr Add (lit1) (lit2)"
        parseAdd

parseParensChangeOrder :: Expectation
parseParensChangeOrder =
  parseAsAST " 1 + (2 - 1)"
    `shouldBe` Right (Expr Add (IntLit 1) (Parens (Expr Sub (IntLit 2) (IntLit 1))))

parseParens :: SpecWith ()
parseParens =
  describe "parseAsAST" $
    context "when parsing parentheses" $
      it
        "should parse parenthesised expression with higher precenence"
        parseParensChangeOrder

parseParensBeforeTimes :: Expectation
parseParensBeforeTimes =
  parseAsAST " 1 * (2 - 1)"
    `shouldBe` Right (Expr Mult (IntLit 1) (Parens (Expr Sub (IntLit 2) (IntLit 1))))

parseParensTimes :: SpecWith ()
parseParensTimes =
  describe "parseAsAST" $
    context "when parsing parentheses" $
      it
        "should parse parenthesised expression with higher precenence"
        parseParensBeforeTimes

lessParsedCorrectly :: Expectation
lessParsedCorrectly =
  parseAsAST "1 < 2"
    `shouldBe` Right (Less (IntLit 1) (IntLit 2))

parseLessThan :: SpecWith ()
parseLessThan =
  describe "parseAsAST" $
    context "when parsing <" $
      it
        "should parse it as Less"
        lessParsedCorrectly

andParsedCorrectly :: Expectation
andParsedCorrectly =
  parseAsAST "true and false"
    `shouldBe` Right (And (BoolLit True) (BoolLit False))

parseAnd :: SpecWith ()
parseAnd =
  describe "parseAsAST" $
    context "when parsing and with bool literals" $
      it
        "should parse it as And"
        andParsedCorrectly

nestedBoolParsedCorrectly :: Expectation
nestedBoolParsedCorrectly =
  parseAsAST "not false and (false or true)"
    `shouldBe` Right (Not (And (BoolLit False) (Parens (Or (BoolLit False) (BoolLit True)))))

-- note that this is not yet parsed correctly:
-- "(not false) and (false or true)"
-- TODO: fix this!

parseNestedBool :: SpecWith ()
parseNestedBool =
  describe "parseAsAST" $
    context "when parsing nested boolean expressions" $
      it
        "should parse it as And"
        nestedBoolParsedCorrectly

boolParensParsedCorrectly :: Expectation
boolParensParsedCorrectly =
  parseAsASTTest "(not false) and true"
    `shouldBe` Right (And (Parens (Not (BoolLit False))) (BoolLit True))

parseboolParens :: SpecWith ()
parseboolParens =
  describe "parseAsAST" $
    context "when parsing nested boolean expressions" $
      it
        "should parse it as And"
        boolParensParsedCorrectly

boolParensEndParsedCorrectly :: Expectation
boolParensEndParsedCorrectly =
  parseAsAST "false and (not true)"
    `shouldBe` Right (And (BoolLit False) (Parens (Not (BoolLit True))))

parseboolParensLast :: SpecWith ()
parseboolParensLast =
  describe "parseAsAST" $
    context "when parsing nested boolean expressions" $
      it
        "should parse it as And"
        boolParensEndParsedCorrectly

-- addShouldNotParseWithoutSymb :: Expectation
-- addShouldNotParseWithoutSymb =
--   -- currently fails
--   unwrapped (parseAsAdd "1 myvar")
--     `shouldBe` Left (ParseError "no parse")
--       where unwrapped (Left e) = Left (ParseError "no parse")
--             unwrapped (Right x) = Right x

-- addFailsWithoutAdd :: SpecWith ()
-- addFailsWithoutAdd =
--   -- currently fails
--   describe "parseAdd" $
--     context "when parsing without + or *" $
--       it
--         "should fail"
--         addShouldNotParseWithoutSymb
boolBoolOpParsedCorrectly :: Expectation
boolBoolOpParsedCorrectly =
  parseAsBoolOp "(not false) and true"
    `shouldBe` Right (And (Parens (Not (BoolLit False))) (BoolLit True))

parseBoolOp :: SpecWith ()
parseBoolOp =
  describe "parseAsAST" $
    context "when parsing nested boolean expressions" $
      it
        "should parse it as And"
        boolBoolOpParsedCorrectly

ifInsideWhile :: Expectation
ifInsideWhile =
  parseAsAST "if  true  then if true then print(2) endif endif"
    `shouldBe` Right (If (BoolLit True) [If (BoolLit True) [Print (IntLit 2)]])

parseIfInWhile :: SpecWith ()
parseIfInWhile =
  describe "parseAsAST" $
    context "when parsing if nested in while" $
      it
        "should parse it is while loop with if"
        ifInsideWhile

numInsidePrint :: Expectation
numInsidePrint =
  parseAsAST "print(2)"
    `shouldBe` Right (Print (IntLit 2))

parseNumInsidePrint :: SpecWith ()
parseNumInsidePrint =
  describe "parseAsAST" $
    context "when parsing print with an Int literal" $
      it
        "should parse it as Print (IntLit num)"
        numInsidePrint

strInsidePrint :: Expectation
strInsidePrint =
  parseAsAST "print(\"hello\")"
    `shouldBe` Right (Print (StrLit "hello"))

parseStrInsidePrint :: SpecWith ()
parseStrInsidePrint =
  describe "parseAsAST" $
    context "when parsing print with an string literal" $
      it
        "should parse it as Print (StrLit x)"
        strInsidePrint

charInsidePrint :: Expectation
charInsidePrint =
  parseAsAST "print('c')"
    `shouldBe` Right (Print (Letter 'c'))

parseCharInsidePrint :: SpecWith ()
parseCharInsidePrint =
  describe "parseAsAST" $
    context "when parsing print with a character literal" $
      it
        "should parse it as Print (Letter x)"
        charInsidePrint

----Type Checking-------------------
charInsidePrintType :: Expectation
charInsidePrintType =
  typeOf (Print (Letter 'c')) initialTypeEnv
    `shouldBe` Right KVoid

typeCheckCharInsidePrint :: SpecWith ()
typeCheckCharInsidePrint =
  describe "typeOf" $
    context "when type checking print with a character literal" $
      it
        "should succeed with Type KVoid"
        charInsidePrintType

strInsidePrintType :: Expectation
strInsidePrintType =
  typeOf (Print (StrLit "a String")) initialTypeEnv
    `shouldBe` Right KVoid

typeCheckStrInsidePrint :: SpecWith ()
typeCheckStrInsidePrint =
  describe "typeOf" $
    context "when type checking print with a string literal" $
      it
        "should succeed with Type KVoid"
        strInsidePrintType

strInsidePrintInParensType :: Expectation
strInsidePrintInParensType =
  typeOf (Print (Parens (StrLit "a String"))) initialTypeEnv
    `shouldBe` Right KVoid

typeCheckStrInsideParensPrint :: SpecWith ()
typeCheckStrInsideParensPrint =
  describe "typeOf" $
    context "when type checking print with a string literal in brackets" $
      it
        "should succeed with Type KVoid"
        strInsidePrintInParensType

boolInsidePrintType :: Expectation
boolInsidePrintType =
  typeOf (Print (BoolLit True)) initialTypeEnv
    `shouldBe` ( Left $
                   TypeError $
                     "truth value can't be printed; please convert ot text"
               )

typeCheckBoolInsidePrint :: SpecWith ()
typeCheckBoolInsidePrint =
  describe "typeOf" $
    context "when type checking print with a boolean literal" $
      it
        "should fail with Type Error"
        boolInsidePrintType


andInsidePrintType :: Expectation
andInsidePrintType =
  typeOf (Print (And (BoolLit True) (BoolLit False))) initialTypeEnv
    `shouldBe` ( Left $
                   TypeError $
                     "value of type truth can't be printed. Convert to text; only text can be printed"
               )

typeCheckAndInsidePrint :: SpecWith ()
typeCheckAndInsidePrint =
  describe "typeOf" $
    context "when type checking print with a boolean and" $
      it
        "should fail with Type Error"
        andInsidePrintType
