module BooleanMatrix where

import Data.List (transpose, intercalate)
import Control.Applicative (liftA2)

class BooleanAlgebra a where
    algebraAnd :: a -> a -> a
    algebraOr :: a -> a -> a
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
