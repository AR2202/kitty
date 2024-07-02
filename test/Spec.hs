{-# LANGUAGE OverloadedStrings #-}

import Evaluator
import KittyTypes
import Parser
import Test.Hspec
import Test.QuickCheck
import TypeChecker
import Parser 
import Text.Parsec.Error


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
    --addFailsWithoutAdd

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
  -- currently fails
  parseAsASTTest "(not false) and true"
    `shouldBe` Right (And (Parens (Not (BoolLit False)))(BoolLit True) )


parseboolParens :: SpecWith ()
parseboolParens =
  -- currently fails
  describe "parseAsAST" $
    context "when parsing nested boolean expressions" $
      it
        "should parse it as And"
        boolParensParsedCorrectly

boolParensEndParsedCorrectly :: Expectation
boolParensEndParsedCorrectly =
  -- currently fails
  parseAsAST "false and (not true)"
    `shouldBe` Right (And (BoolLit False)(Parens (Not (BoolLit True))) )


parseboolParensLast :: SpecWith ()
parseboolParensLast =
  -- currently fails
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
  -- currently fails
  parseAsBoolOp "(not false) and true"
    `shouldBe` Right (And (Parens (Not (BoolLit False)))(BoolLit True) )

parseBoolOp :: SpecWith ()
parseBoolOp =
  -- currently fails
  describe "parseAsAST" $
    context "when parsing nested boolean expressions" $
      it
        "should parse it as And"
        boolBoolOpParsedCorrectly   