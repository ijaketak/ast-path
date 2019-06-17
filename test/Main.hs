{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.ASTPath
import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show, Generic)

data Exp2 a
  = Val a
  | Term (Exp1 a)
  | Add [Exp2 a]
  deriving (Eq, Show, Generic)

data Exp1 a
  = Par (Exp2 a)
  | Mul [Exp1 a]
  deriving (Eq, Show, Generic)

data AuxList a = Nil () | Cons a (AuxList a)
  deriving (Eq, Show, Generic)

instance AST a => AST (Tree a)
instance AST a => AST (Exp1 a)
instance AST a => AST (Exp2 a)
instance AST a => AST (AuxList a)

instance AST Int where
  astPathWithHalf = terminalPath show
instance AST () where
  astPathWithHalf = nullPath

main :: IO ()
main = do
  defaultMain $ unitTests

unitTests :: TestTree
unitTests = testGroup "unit tests"
  [ testCase "simple tree" $ do
    let tr :: Tree Int
        tr = Node (Leaf 0) (Leaf 1)
        path = astPath tr
        n = "Node"
        l = "Leaf"
    assertBool "single path" $
      (n, [l, "0"], [l, "1"]) `elem` path
  , testCase "complex tree" $ do
    let tr :: Tree Int
        tr = Node (Leaf 0) (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
        path = astPath tr
        n = "Node"
        l = "Leaf"
    assertBool "path 1" $
      (n, [l, "2"], [l, "3"]) `elem` path
    assertBool "path 2" $
      (n, [l, "1"], [n, l, "2"]) `elem` path
    assertBool "path 3" $
      (n, [l, "1"], [n, l, "3"]) `elem` path
    assertBool "path 4" $
      (n, [l, "0"], [n, l, "1"]) `elem` path
    assertBool "path 5" $
      (n, [l, "0"], [n, n, l, "2"]) `elem` path
    assertBool "path 6" $
      (n, [l, "0"], [n, n, l, "3"]) `elem` path
  , testCase "arithmetics" $ do
    let ex :: Exp1 Int
        ex = Mul [Par (Add [Val 5,Val 3]), Par (Add [Val 4, Val 2,Val 1])]
        path = astPath ex
        v = "Val"
        p = "Par"
        a = "Add"
        m = "Mul"
    assertBool "path 1" $
      (a, [v, "5"], [v, "3"]) `elem` path
    assertBool "path 2" $
      (a, [v, "2"], [v, "1"]) `elem` path
    assertBool "path 3" $
      (a, [v, "4"], [v, "2"]) `elem` path
    assertBool "path 4" $
      (a, [v, "4"], [v, "1"]) `elem` path
    assertBool "path 5" $
      (m, [p, a, v, "5"], [p, a, v, "4"]) `elem` path
    assertBool "path 6" $
      (m, [p, a, v, "5"], [p, a, v, "2"]) `elem` path
    assertBool "path 7" $
      (m, [p, a, v, "5"], [p, a, v, "1"]) `elem` path
    assertBool "path 8" $
      (m, [p, a, v, "3"], [p, a, v, "4"]) `elem` path
    assertBool "path 9" $
      (m, [p, a, v, "3"], [p, a, v, "2"]) `elem` path
    assertBool "path 10" $
      (m, [p, a, v, "3"], [p, a, v, "1"]) `elem` path
  , testCase "list with auxiliary type" $ do
    let l :: AuxList Int
        l = Cons 1 $ Cons 2 $ Cons 3 $ Nil ()
        path = astPath l
        n = "Nil"
        c = "Cons"
    assertBool "path 1" $
      (c, ["2"], [c, "3"]) `elem` path
    assertBool "path 2" $
      (c, ["1"], [c, "2"]) `elem` path
    assertBool "path 3" $
      (c, ["1"], [c, c, "3"]) `elem` path
    assertBool "not contains ()" $
      "()" `notElem` concatMap toList path
  ]
