import Test.Tasty
import Test.Tasty.HUnit
import TestHelper

main :: IO ()
main = defaultMain (testGroup "Library Tests" [basicTest, lawParserTest, exprParserTest, deriveTest])
basicTest, lawParserTest, exprParserTest, deriveTest :: TestTree
basicTest = localOption (Timeout 1000000 "1 second") $
    testCase "Test file works" (assertBool "Something is deeply wrong with the test file" (True == True))
lawParserTest = localOption (Timeout 1000000 "1 second") $
    testCase "Law parser does not generate errors" (assertBool "Not all laws parsed successfully" ((length laws) == (length lawStrings)))
exprParserTest = localOption (Timeout 1000000 "1 second") $
    testCase "Expression parser parses as expected" (assertBool "Incorrect parse" (parseExprTest))
deriveTest = localOption (Timeout 1000000 "1 second") $
    testCase "d/dx x^2 - 4 - x^2 = 0" (assertBool "Unexpected result" (deriveFuncTest))

