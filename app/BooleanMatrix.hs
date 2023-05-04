module BooleanMatrix where

import Data.List (transpose, intercalate)
import Control.Applicative (liftA2)

class BooleanAlgebra a where
    algebraAnd :: a -> a -> a
    algebraOr :: a -> a -> a
    algebraNot :: a -> a
    empty :: a

newtype BooleanMatrix a = BooleanMatrix { matrix :: [[a]] }

instance Functor BooleanMatrix where
    fmap :: (a -> b) -> BooleanMatrix a -> BooleanMatrix b
    fmap f (BooleanMatrix m) = BooleanMatrix $ fmap (fmap f) m

instance Applicative BooleanMatrix where
    pure :: a -> BooleanMatrix a
    pure a = BooleanMatrix [[a]]
    liftA2 :: (a -> b -> c) -> BooleanMatrix a -> BooleanMatrix b -> BooleanMatrix c
    liftA2 f (BooleanMatrix m1) (BooleanMatrix m2) = BooleanMatrix $ zipWith (zipWith f) m1 m2

instance (Show a) => Show (BooleanMatrix a) where
    show (BooleanMatrix m) = intercalate "\n" $ fmap (unwords . fmap show) m

matrixProduct :: (BooleanAlgebra a) => BooleanMatrix a -> BooleanMatrix a -> BooleanMatrix a
matrixProduct (BooleanMatrix m1) (BooleanMatrix m2) = 
    BooleanMatrix [[foldl algebraOr empty $ zipWithSize algebraAnd row col | col <- m2t] | row <- m1] where
        zipWithSize _ [] [] = []
        zipWithSize _ [] _ = error "Wrong shapes"
        zipWithSize _ _ [] = error "Wrong shapes"
        zipWithSize f (a : as) (b : bs) = f a b : zipWithSize f as bs
        m2t = transpose m2

sumMatrix :: (BooleanAlgebra a) => BooleanMatrix a -> BooleanMatrix a -> BooleanMatrix a
sumMatrix = liftA2 algebraOr

shape :: BooleanMatrix a -> Int
shape (BooleanMatrix m) = length m

closure' :: (BooleanAlgebra a) => BooleanMatrix a -> BooleanMatrix a -> Int -> BooleanMatrix a
closure' res _ 0 = res
closure' res m n = closure' (sumMatrix (matrixProduct res m) res) m (n - 1)

closure :: (BooleanAlgebra a) => BooleanMatrix a -> BooleanMatrix a
closure m = closure' m m (shape m)

data TrivialAlgebra = T | F
instance Show TrivialAlgebra where
    show T = "1"
    show F = "0"

instance BooleanAlgebra TrivialAlgebra where
    algebraAnd T T = T
    algebraAnd _ _ = F
    algebraOr F F = F
    algebraOr _ _ = T
    algebraNot F = T
    algebraNot T = F
    empty = F


floydWarshallClosure :: (BooleanAlgebra a) => BooleanMatrix a -> BooleanMatrix a
floydWarshallClosure m@(BooleanMatrix matrix) = BooleanMatrix (foldl updateMatrix matrix [0..n-1]) where
    n = shape m
    updateMatrix mtx k = [[updateElement i j k | j <- [0..n-1]] | i <- [0..n-1]] 
      where
        updateElement i j k = (mtx !! i !! j) `algebraOr` ((mtx !! i !! k) `algebraAnd` ( mtx !! k !! j))
