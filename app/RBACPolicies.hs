module RBACPolicies where

import BooleanMatrix
import Data.HashMap.Strict (HashMap, (!?))
import Data.Maybe (fromJust)
import Data.List (elemIndex)
import Data.Tuple.Extra (fst3, snd3, thd3)

import Data.Vector ((!))

import Algebra.Lattice ( Lattice, (/\) )

import Data.Hashable


data RBACPolicies rl us pr = RBACPolicies {
    roles :: [rl],
    users :: [us],
    permissions :: [pr],
    authorizedPermissions :: HashMap rl pr,
    servesRA :: [(us, rl, pr)],
    inheritance :: BooleanMatrix pr
}


closurePolicies :: (Lattice pr) => RBACPolicies rl us pr -> RBACPolicies rl us pr
closurePolicies (RBACPolicies r u pr ap srv inh) = RBACPolicies r u pr ap srv (floydWarshallClosure inh)


newtype Session rl us pr = Session {
    says :: (us, [(rl, pr)])
}


checkSSDConstrains :: (Eq rl, Lattice pr, Eq pr, Eq us, Hashable rl) => RBACPolicies rl us pr -> [([rl], Int)] -> Bool
checkSSDConstrains policies = all (\(rs, n) -> all (\user -> singleSSD user policies rs n) (users policies)) where
    singleSSD user policies rs n = length (filter (\r -> r `elem` rs && any (\r' -> isServes r' user && isInheritsOn r r' (ap r')) (roles policies)) (roles policies)) < n
    ap r = fromJust (authorizedPermissions policies !? r)
    isServes r' user = any (\serves -> user == fst3 serves && r' == snd3 serves) (servesRA policies)
    isInheritsOn r1 r2 p = p == p /\ (matrix (inheritance policies) ! ind r1 ! ind r2)
    ind r = fromJust $ elemIndex r (roles policies)

checkDSDConstrains :: (Eq rl, Lattice pr, Eq pr, Eq us, Hashable rl) => RBACPolicies rl us pr -> Session rl us pr -> [([rl], Int)] -> Bool
checkDSDConstrains policies session = all (uncurry singleDSD) where
    singleDSD rs n = length (filter (\r -> r `elem` rs && any (\r' -> inSession r' && isInheritsOn r r' (ap r')) (roles policies)) (roles policies)) < n
    inSession r = any (\(rl, _) -> rl == r) (snd (says session))
    isInheritsOn r1 r2 p = p == p /\ (matrix (inheritance policies) ! ind r1 ! ind r2)
    ind r = fromJust $ elemIndex r (roles policies)
    ap r = fromJust (authorizedPermissions policies !? r)

makeSingleDecision :: (Hashable rl, Lattice pr, Eq pr, Eq us) => RBACPolicies rl us pr -> (us, rl, pr) -> Bool
makeSingleDecision policies (user, role, perm) = 
    any (\serves -> isInheritsOn role (snd3 serves) perm) $ filter (\serves -> fst3 serves == user) (servesRA policies) where
        isInheritsOn r1 r2 p = p == p /\ (matrix (inheritance policies) ! ind r1 ! ind r2)
        ind r = fromJust $ elemIndex r (roles policies)

makeDecision :: (Hashable rl, Lattice pr, Eq pr, Eq us) => RBACPolicies rl us pr -> [(us, rl, pr)] -> Bool
makeDecision policies = all $ makeSingleDecision policies
