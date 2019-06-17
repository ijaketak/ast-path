-- | AST-path is a representation for predicting program properties.

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.ASTPath
  ( type HalfPath
  , type ASTPath
  , toList
  , astPath
  , AST(..)
  , treePath
  , terminalPath
  , nullPath
  , AST'(..)
  ) where

import GHC.Generics

type HalfPath = [String]

-- | (top node, [..., start node], [..., end node])
type ASTPath = (String, HalfPath, HalfPath)

toList :: ASTPath -> [String]
toList (t, ls, rs) = revApp ls $ t : rs
 where
  revApp [] ys = ys
  revApp (x:xs) ys = revApp xs $ x : ys

-- | Typeclass for tree or terminal node.
class AST a where
  {-# MINIMAL astPathWithHalf #-}
  -- | Generate paths and half paths for recursion.
  astPathWithHalf
    :: a -- ^ current tree
    -> String -- ^ label of current root node
    -> ([ASTPath], [HalfPath]) -- ^ paths and half paths
  default astPathWithHalf :: (Generic a, AST' (Rep a)) => a -> String -> ([ASTPath], [HalfPath])
  astPathWithHalf = treePath

-- | Default implementation for tree type.
treePath :: forall a. (Generic a, AST' (Rep a)) => a -> String -> ([ASTPath], [HalfPath])
treePath a c = astPathWithHalf' (from a :: Rep a a) c

-- | Typical implementation for terminal node type.
terminalPath :: (a -> String) -> a -> String -> ([ASTPath], [HalfPath])
terminalPath f x _ = ([], [[f x]])

-- | No path implementation for auxiliary data type.
nullPath :: a -> String -> ([ASTPath], [HalfPath])
nullPath _ _ = ([], [])

-- | Generate AST-paths from tree.
astPath :: AST a => a -> [ASTPath]
astPath a = fst $ astPathWithHalf a undefined

-- | Class of generic representation types that can be parsed into paths.
class AST' f where
  -- | Default implementation for generic instances of `AST`.
  astPathWithHalf' :: f a -> String -> ([ASTPath], [HalfPath])

instance AST' V1 where
  astPathWithHalf' _ _ = undefined

instance AST' U1 where
  astPathWithHalf' _ _ = ([], [])

instance (AST' f, AST' g) => AST' (f :+: g) where
  astPathWithHalf' (L1 a) = astPathWithHalf' a
  astPathWithHalf' (R1 a) = astPathWithHalf' a

instance (AST' f, AST' g) => AST' (f :*: g) where
  astPathWithHalf' (a :*: b) c = (ps, hs)
   where
    (psa, hsa) = astPathWithHalf' a c
    (psb, hsb) = astPathWithHalf' b c
    ps = psa ++ psb ++ [ (c, p1, p2) | p1 <- hsa, p2 <- hsb ]
    hs = hsa ++ hsb

instance (AST a) => AST' (K1 i a) where
  astPathWithHalf' (K1 a) = astPathWithHalf (a :: a)

instance (AST' f, Datatype d) => AST' (D1 d f) where
  astPathWithHalf' (M1 a) = astPathWithHalf' a

instance (AST' f, Constructor c) => AST' (C1 c f) where
  astPathWithHalf' (M1 a) _ = (ps, map (c:) hs)
   where
    (ps, hs) = astPathWithHalf' a c
    c = conName (undefined :: t c f a)

instance (AST' f, Selector s) => AST' (S1 s f) where
  astPathWithHalf' (M1 a) = astPathWithHalf' a

-- | Constructor of list type is ignored, same as generic product type.
-- Hence list type itself cannot be used with `astPath`.
-- If want to use, supply root node label.
instance AST a => AST [a] where
  astPathWithHalf [] _ = ([], [])
  astPathWithHalf (x:xs) c = (ps, hs)
   where
    (psh, hsh) = astPathWithHalf x c
    (pst, hst) = astPathWithHalf xs c
    ps = psh ++ pst ++ [ (c, p1, p2) | p1 <- hsh, p2 <- hst ]
    hs = hsh ++ hst
