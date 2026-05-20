{-# LANGUAGE OverloadedStrings #-}

import Evaluator
import KittyTypes
import Parser
import Test.Hspec
import Test.QuickCheck
import Text.Parsec.Error
import TypeChecker
import qualified Data.Map as M

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
    parsePrintInWhile
    parseIfInIf
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
    -- Function definitions
    typeCheckFuncDefHappyPath
    typeCheckFuncDefParamsInScope
    typeCheckFuncDefReturnMismatch
    typeCheckFuncDefUnknownVar
    typeCheckFuncDefWrongTypeInBody
    typeCheckFuncDefEmptyBodyVoid
    typeCheckFuncDefEmptyBodyMismatch
    -- Function calls
    typeCheckFuncCallHappyPath
    typeCheckFuncCallExprArg
    typeCheckFuncCallMultipleParams
    typeCheckFuncCallWrongArgType
    typeCheckFuncCallTooFewArgs
    typeCheckFuncCallTooManyArgs
    typeCheckFuncCallUndefined

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
  parseAsAST "1 + (2 - 1)"
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
  parseAsAST "1 * (2 - 1)"
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
  parseAsAST "while true do if true then print(2) endif endwhile"
    `shouldBe` Right (While (BoolLit True) [If (BoolLit True) [Print (IntLit 2)]])

parseIfInWhile :: SpecWith ()
parseIfInWhile =
  describe "parseAsAST" $
    context "when parsing if nested in while" $
      it
        "should parse it as while loop with if"
        ifInsideWhile

printInsideWhile :: Expectation
printInsideWhile =
  parseAsAST "while true do print(\"true\") endwhile"
    `shouldBe` Right (While (BoolLit True)  [Print (StrLit "true")])

parsePrintInWhile :: SpecWith ()
parsePrintInWhile =
  describe "parseAsAST" $
    context "when parsing print in while" $
      it
        "should parse it as while loop with print"
        printInsideWhile

ifInsideIf :: Expectation
ifInsideIf =
  parseAsAST "if true then if true then print(2) endif endif"
    `shouldBe` Right (If (BoolLit True) [If (BoolLit True) [Print (IntLit 2)]])

parseIfInIf :: SpecWith ()
parseIfInIf =
  describe "parseAsAST" $
    context "when parsing if nested in if" $
      it
        "should parse it as nested if"
        ifInsideIf

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
                     "I can't print a truth value directly. Try: print(toText(yourValue))"
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
                     "I can't print a truth. Only text and letters can be printed. Try wrapping it with toText()."
               )

typeCheckAndInsidePrint :: SpecWith ()
typeCheckAndInsidePrint =
  describe "typeOf" $
    context "when type checking print with a boolean and" $
      it
        "should fail with Type Error"
        andInsidePrintType

----Function type checking-----------

-- | Environments pre-populated with functions for call tests
envWithDouble :: TypeEnv
envWithDouble = TypeEnv
  { _functionTypes = M.fromList [("double", KFun ([KInt], [KInt]))]
  , _varTypes = M.empty
  }

envWithAdd :: TypeEnv
envWithAdd = TypeEnv
  { _functionTypes = M.fromList [("add", KFun ([KInt, KInt], [KInt]))]
  , _varTypes = M.empty
  }

-- Definition tests

funcDefHappyPath :: Expectation
funcDefHappyPath =
  typeOf
    (DefType (FunctionDef (FunctionDefinition
      "double" [("x", KInt)] KInt
      [Expr Mult (Variable "x") (IntLit 2)])))
    initialTypeEnv
    `shouldBe` Right KVoid

typeCheckFuncDefHappyPath :: SpecWith ()
typeCheckFuncDefHappyPath =
  describe "typeOf" $
    context "when type checking a function definition with a matching return type" $
      it "should succeed with KVoid" funcDefHappyPath

funcDefParamsInScope :: Expectation
funcDefParamsInScope =
  typeOf
    (DefType (FunctionDef (FunctionDefinition
      "addOne" [("x", KInt)] KInt
      [Expr Add (Variable "x") (IntLit 1)])))
    initialTypeEnv
    `shouldBe` Right KVoid

typeCheckFuncDefParamsInScope :: SpecWith ()
typeCheckFuncDefParamsInScope =
  describe "typeOf" $
    context "when type checking a function body that uses its parameters" $
      it "should have parameters in scope" funcDefParamsInScope

funcDefReturnMismatch :: Expectation
funcDefReturnMismatch =
  typeOf
    (DefType (FunctionDef (FunctionDefinition
      "bad" [("x", KInt)] KBool
      [Variable "x"])))
    initialTypeEnv
    `shouldBe` Left (TypeError
      "The function 'bad' says it returns a truth, but the body gives back a wholeNumber.")

typeCheckFuncDefReturnMismatch :: SpecWith ()
typeCheckFuncDefReturnMismatch =
  describe "typeOf" $
    context "when a function body type doesn't match the declared return type" $
      it "should fail with a type error" funcDefReturnMismatch

funcDefUnknownVar :: Expectation
funcDefUnknownVar =
  typeOf
    (DefType (FunctionDef (FunctionDefinition
      "bad" [("x", KInt)] KInt
      [Variable "y"])))
    initialTypeEnv
    `shouldBe` Left (DoesNotExistError
      "I don't know what y is — did you forget to create it?")

typeCheckFuncDefUnknownVar :: SpecWith ()
typeCheckFuncDefUnknownVar =
  describe "typeOf" $
    context "when a function body uses a variable that isn't a parameter" $
      it "should fail with a does not exist error" funcDefUnknownVar

funcDefWrongTypeInBody :: Expectation
funcDefWrongTypeInBody =
  typeOf
    (DefType (FunctionDef (FunctionDefinition
      "bad" [("x", KInt)] KInt
      [Letters (Variable "x")])))
    initialTypeEnv
    `shouldBe` Left (TypeError
      "letters() only works on text, but I got a wholeNumber.")

typeCheckFuncDefWrongTypeInBody :: SpecWith ()
typeCheckFuncDefWrongTypeInBody =
  describe "typeOf" $
    context "when a function body uses a parameter with the wrong type" $
      it "should fail with a type error" funcDefWrongTypeInBody

funcDefEmptyBodyVoid :: Expectation
funcDefEmptyBodyVoid =
  typeOf
    (DefType (FunctionDef (FunctionDefinition "nothing" [] KVoid [])))
    initialTypeEnv
    `shouldBe` Right KVoid

typeCheckFuncDefEmptyBodyVoid :: SpecWith ()
typeCheckFuncDefEmptyBodyVoid =
  describe "typeOf" $
    context "when a function has an empty body and KVoid return type" $
      it "should succeed with KVoid" funcDefEmptyBodyVoid

funcDefEmptyBodyMismatch :: Expectation
funcDefEmptyBodyMismatch =
  typeOf
    (DefType (FunctionDef (FunctionDefinition "bad" [] KInt [])))
    initialTypeEnv
    `shouldBe` Left (TypeError
      "The function 'bad' says it returns a wholeNumber, but there's nothing in the body.")

typeCheckFuncDefEmptyBodyMismatch :: SpecWith ()
typeCheckFuncDefEmptyBodyMismatch =
  describe "typeOf" $
    context "when a function has an empty body but a non-void return type" $
      it "should fail with a type error" funcDefEmptyBodyMismatch

-- Call tests

funcCallHappyPath :: Expectation
funcCallHappyPath =
  typeOf (Call (FunctionCall "double" [IntLit 5])) envWithDouble
    `shouldBe` Right KInt

typeCheckFuncCallHappyPath :: SpecWith ()
typeCheckFuncCallHappyPath =
  describe "typeOf" $
    context "when calling a function with the correct argument type" $
      it "should return the function's return type" funcCallHappyPath

funcCallExprArg :: Expectation
funcCallExprArg =
  typeOf (Call (FunctionCall "double" [Expr Add (IntLit 1) (IntLit 2)])) envWithDouble
    `shouldBe` Right KInt

typeCheckFuncCallExprArg :: SpecWith ()
typeCheckFuncCallExprArg =
  describe "typeOf" $
    context "when calling a function with an expression as an argument" $
      it "should type check the argument expression" funcCallExprArg

funcCallMultipleParams :: Expectation
funcCallMultipleParams =
  typeOf (Call (FunctionCall "add" [IntLit 1, IntLit 2])) envWithAdd
    `shouldBe` Right KInt

typeCheckFuncCallMultipleParams :: SpecWith ()
typeCheckFuncCallMultipleParams =
  describe "typeOf" $
    context "when calling a function with multiple arguments" $
      it "should check all arguments and return the return type" funcCallMultipleParams

funcCallWrongArgType :: Expectation
funcCallWrongArgType =
  typeOf (Call (FunctionCall "double" [BoolLit True])) envWithDouble
    `shouldBe` Left (TypeError
      "The input to double should be a wholeNumber, but I got a truth.")

typeCheckFuncCallWrongArgType :: SpecWith ()
typeCheckFuncCallWrongArgType =
  describe "typeOf" $
    context "when calling a function with the wrong argument type" $
      it "should fail with a type error" funcCallWrongArgType

funcCallTooFewArgs :: Expectation
funcCallTooFewArgs =
  typeOf (Call (FunctionCall "double" [])) envWithDouble
    `shouldBe` Left (TypeError
      "double needs 1 input, but you didn't give it any.")

typeCheckFuncCallTooFewArgs :: SpecWith ()
typeCheckFuncCallTooFewArgs =
  describe "typeOf" $
    context "when calling a function with too few arguments" $
      it "should fail with a type error" funcCallTooFewArgs

funcCallTooManyArgs :: Expectation
funcCallTooManyArgs =
  typeOf (Call (FunctionCall "double" [IntLit 1, IntLit 2])) envWithDouble
    `shouldBe` Left (TypeError
      "double needs 1 input, but you gave it 2.")

typeCheckFuncCallTooManyArgs :: SpecWith ()
typeCheckFuncCallTooManyArgs =
  describe "typeOf" $
    context "when calling a function with too many arguments" $
      it "should fail with a type error" funcCallTooManyArgs

funcCallUndefined :: Expectation
funcCallUndefined =
  typeOf (Call (FunctionCall "unknown" [IntLit 1])) initialTypeEnv
    `shouldBe` Left (UndefinedError
      "I don't know a function called unknown — did you define it?")

typeCheckFuncCallUndefined :: SpecWith ()
typeCheckFuncCallUndefined =
  describe "typeOf" $
    context "when calling a function that hasn't been defined" $
      it "should fail with an undefined error" funcCallUndefined
