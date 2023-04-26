{-# LANGUAGE DeriveGeneric #-}

module Propositions where

import GHC.Generics (Generic)
import Data.Hashable

data Proposition
    = AtomProp String
    | LNot Proposition
    | LAnd Proposition Proposition
    | LOr Proposition Proposition
    | LImpl Proposition Proposition deriving (Eq, Generic)

instance Hashable Proposition

instance Show Proposition where
    show (AtomProp var) = var
    show (LNot p) = "¬(" ++ show p ++ ")"
    show (LAnd p q) = "(" ++ show p ++ ") ∧ (" ++ show q ++ ")"
    show (LOr p q) = "(" ++ show p ++ ") ∨ (" ++ show q ++ ")"
    show (LImpl p q) = "(" ++ show p ++ ") ⊃ (" ++ show q ++ ")"

notProp :: Proposition -> Proposition
notProp (LNot p) = p
notProp p = LNot p

isNot :: Proposition -> Bool
isNot (LNot _) = True
isNot _ = False

isAnd :: Proposition -> Bool
isAnd (LAnd _ _) = True
isAnd _ = False

isOr :: Proposition -> Bool
isOr (LOr _ _) = True
isOr _ = False

isImpl :: Proposition -> Bool
isImpl (LImpl _ _) = True
isImpl _ = False


