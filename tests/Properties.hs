{-# LANGUAGE TemplateHaskell #-}
module Main where

--import Test.QuickCheck
--import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.QuickCheck2 (testProperty)

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse ys ++ reverse xs

prop_qq :: [Int] -> Bool
prop_qq xs =
    xs == reverse (reverse xs)

prop_jalla :: Int -> Bool
prop_jalla x =
    x == x

main :: IO ()
main = $defaultMainGenerator

