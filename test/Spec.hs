{-# LANGUAGE OverloadedStrings#-}
import Test.Hspec
import Test.QuickCheck
import Parser
import Evaluator
import TypeChecker
import KittyTypes
main :: IO ()
main = hspec $
    do
      -- Parsing
      -----------------------------------------
      parseAddition


parseAdd :: Expectation
parseAdd =
  parseAsAST  "1 + 2"
    `shouldBe` Right (Expr Add (IntLit 1) (IntLit 2))

parseAddition :: SpecWith ()
parseAddition =
  describe "parseAsAST" $
    context "when parsing an addition expression" $
      it
        "should return Expr Add (lit1) (lit2)"
        parseAdd