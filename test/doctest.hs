import Test.DocTest
import System.Environment

main :: IO ()
main = doctest ["-XOverloadedStrings", "src"]
