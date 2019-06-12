# ast-path

AST-path is a representation for predicting program properties.

AST-path is described in the paper "A General Path-Based Representation for Predicting Program Properties" (PLDI'2018) <https://arxiv.org/abs/1803.09544>
and used at <https://code2vec.org/> and at <https://code2seq.org/>.

```
{-# LANGUAGE DeriveGeneric #-}
import Data.ASTPath

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show, Generic)

instance AST a => AST (Tree a)
instance AST Int where
  astPathWithHalf = terminalPath show

>>> astPath $ Node (Leaf 0) (Node (Leaf 1) (Leaf 2))
[("Node",["Leaf","1"],["Leaf","2"]),("Node",["Leaf","0"],["Node","Leaf","1"]),("Node",["Leaf","0"],["Node","Leaf","2"])]
```
