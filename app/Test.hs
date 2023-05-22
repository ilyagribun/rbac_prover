module Test where

import Permission
import Principal
import BooleanMatrix
import Data.Vector (fromList)
import RBACPolicies

import qualified Data.HashMap.Strict as HM

facRoles :: [Role]
facRoles = ["CS", "AMI", "SE", "UnTen", "Ten", "Dir", "CAOS"]

r = singleton "r"
w = singleton "w"

facPerms :: [Permission]
facPerms = [r, w]

facUsers :: [User]
facUsers = ["Evgeniy", "Alexander", "Pasha"]

m' :: [[Permission]]
m' = [[r, r,    r,  r,  r,  empty,  empty],
      [empty, r,        empty,  empty,  empty,  empty,  empty],
      [empty, empty,    r,      empty,  empty,  empty,  empty],
      [empty, empty,    empty,  r,      empty,  empty,  empty],
      [empty, empty,    empty,  empty,  w \/ r, w\/r,  w\/r],
      [empty, empty,    empty,  empty,  empty, w \/ r, empty],
      [empty, empty,    empty,  empty,  empty, empty,  w \/ r]]
inhM = BooleanMatrix (fromList (map fromList m'))


policies = RBACPolicies {
    roles = facRoles,
    users = facUsers,
    permissions = facPerms,
    authorizedPermissions = HM.fromList [(facRoles !! 0, r), (facRoles !! 4, r \/ w)],
    servesRA = [("Evgeniy", "Dir", r \/ w)],
    inheritance = inhM
}

policies' = closurePolicies policies


