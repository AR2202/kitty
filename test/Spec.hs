{-# LANGUAGE OverloadedStrings #-}

import Evaluator
import KittyTypes
import Parser
import Test.Hspec
import Test.QuickCheck
import TypeChecker

main :: IO ()
main = hspec $
  do
    -- Parsing
    -----------------------------------------
    parseAddition
    parseParens
    parseParensTimes

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

parseParensBeforeTimes :: Expectation
parseParensBeforeTimes =
  parseAsAST " 1 * (2 - 1)"
    `shouldBe` Right (Expr Mult (IntLit 1) (Parens (Expr Sub (IntLit 2) (IntLit 1))))

parseParens :: SpecWith ()
parseParens =
  describe "parseAsAST" $
    context "when parsing parentheses" $
      it
        "should parse parenthesised expression with higher precenence"
        parseParensChangeOrder
parseParensTimes :: SpecWith ()
parseParensTimes =
  describe "parseAsAST" $
    context "when parsing parentheses" $
      it
        "should parse parenthesised expression with higher precenence"
        parseParensBeforeTimes