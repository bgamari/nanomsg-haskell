import qualified Test.Tasty as T
import qualified Test.Tasty.Ingredients as T

import qualified BinaryProperties
import qualified Properties

tests :: T.TestTree
tests =
    T.sequentialTestGroup "tests" T.AllFinish
      [ Properties.tests
      , BinaryProperties.tests
      ]

ingredients :: [T.Ingredient]
ingredients = T.defaultIngredients

main :: IO ()
main = do
    T.defaultMainWithIngredients ingredients tests

