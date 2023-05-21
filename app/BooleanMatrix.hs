{-# LANGUAGE DeriveFunctor #-}

module BooleanMatrix where

import Data.Vector (Vector, (!), length, generate)

import Algebra.Lattice

newtype BooleanMatrix a = BooleanMatrix { matrix :: Vector (Vector a) } deriving Functor

instance (Show a) => Show (BooleanMatrix a) where
    show (BooleanMatrix m) = safeInit $ foldr (\x y -> x ++ "\n" ++ y) "" (fmap (concatMap show) m) where
        safeInit [] = []
        safeInit a = init a

data TrivialAlgebra = T | F
instance Show TrivialAlgebra where
    show T = "1"
    show F = "0"

instance Lattice TrivialAlgebra where
    T /\ T = T
    _ /\ _ = F
    F \/ F = F
    _ \/ _ = T

shape :: BooleanMatrix a -> Int
shape (BooleanMatrix m) = Data.Vector.length m

floydWarshallClosure :: (Lattice a) => BooleanMatrix a -> BooleanMatrix a
floydWarshallClosure m@(BooleanMatrix matrix) = BooleanMatrix (foldl updateMatrix matrix [0..n-1]) where
    n = shape m
    updateMatrix mtx k = generate n (\i -> generate n (\j -> updateElement i j k))
      where
        updateElement i j k = (mtx ! i ! j) \/ ((mtx ! i ! k) /\ ( mtx ! k ! j))
