module Permission where

import Data.HashSet as HS (HashSet, singleton, intersection, union, empty)
import Algebra.Lattice ( Lattice, (/\), (\/) )

newtype Permission = Permissions (HS.HashSet String) deriving (Eq, Show)

instance Lattice Permission where
    Permissions a /\ Permissions b = Permissions (a `HS.intersection` b)
    Permissions a \/ Permissions b = Permissions (a `HS.union` b)

singleton :: String -> Permission
singleton a = Permissions (HS.singleton a)

empty :: Permission
empty = Permissions HS.empty

(/\) :: Permission -> Permission -> Permission
Permissions a /\ Permissions b = Permissions (a `HS.intersection` b)

(\/) :: Permission -> Permission -> Permission
Permissions a \/ Permissions b = Permissions (a `HS.union` b)
