module Permission where

import Data.HashSet as HS (HashSet, singleton, intersection, union)
import Algebra.Lattice ( Lattice, (/\), (\/) )

newtype Permission = Permissions (HS.HashSet String) deriving Eq

instance Lattice Permission where
    Permissions a /\ Permissions b = Permissions (a `HS.intersection` b)
    Permissions a \/ Permissions b = Permissions (a `HS.union` b)

singleton :: String -> Permission
singleton a = Permissions (HS.singleton a)
