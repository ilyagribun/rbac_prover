module Prover where

import Data.HashSet as HS
import Propositions
import Data.Hashable

data Sequent = Sequent (HS.HashSet Proposition) (HS.HashSet Proposition) deriving Show

data ProofTree = Ax Sequent
    | Unary {bottom :: Sequent, top :: Maybe ProofTree}
    | Binary {bottom :: Sequent, left :: Maybe ProofTree, right :: Maybe ProofTree}

instance Show ProofTree where
    show (Ax a) = show a
    show (Unary bot top) = (show bot) ++ "\n" ++ (show top)

isAxiom :: Sequent -> Bool
isAxiom (Sequent left right) = (not . HS.null) $ HS.intersection left right

partition :: (Hashable a) => (a -> Bool) -> HS.HashSet a -> (HS.HashSet a, HS.HashSet a)
partition pred hashSet = (sat, notSat) where
    sat = HS.filter pred hashSet
    notSat = HS.difference hashSet sat

prove :: Sequent -> Maybe ProofTree
prove seq@(Sequent left right)
    | isAxiom seq || (HS.null left && HS.null right) = Just (Ax seq)
    | any isNot left || any isNot right = Just $ Unary seq (prove $ Sequent (HS.union leftWONots (HS.map dropNot rightNots)) (HS.union rightWONots (HS.map dropNot leftNots)))
    | any isAnd left || any isOr right = Just $ Unary seq (prove $ Sequent (HS.union leftWOAnds leftDropAnds) (HS.union rightWOOrs rightDropOrs))
    | any isOr left =       -- Gamma, (p1 v p2) |- Delta => (Gamma, p1 |- Delta), (Gamma, p2 |- Delta)
            let (phi: _) = HS.toList leftOrs in
            let (p1, p2) = dropOr phi in
            let left' = HS.delete phi left in
            let new1 = prove $ Sequent (HS.insert p1 left') right in
            let new2 = prove $ Sequent (HS.insert p2 left') right in
        Just $ Binary seq new1 new2
    | any isAnd right =     -- Gamma |- (p1 ^ p2), Delta => (Gamma |- p1, Delta), (Gamma |- p2, Delta)
            let (phi: _) = HS.toList rightAnds in
            let (p1, p2) = dropAnd phi in
            let right' = HS.delete phi right in
            let new1 = prove $ Sequent left (HS.insert p1 right') in
            let new2 = prove $ Sequent left (HS.insert p2 right') in
        Just $ Binary seq new1 new2
    | any isImpl left =     -- Gamma, (p1 -> p2) |- Delta => (Gamma, p2 |- Delta), (Gamma |- p1, Delta)
            let (phi: _) = HS.toList leftImps in
            let (p1, p2) = dropImp phi in
            let left' = HS.delete phi left in
            let new1 = prove $ Sequent (HS.insert p2 left') right in
            let new2 = prove $ Sequent left' (HS.insert p1 right) in
        Just $ Binary seq new1 new2
    | any isImpl right =     -- Gamma |- (p1 -> p2), Delta => (Gamma, p1 |- p2, Delta)
            let (phi: _) = HS.toList rightImps in
            let (p1, p2) = dropImp phi in
            let right' = HS.delete phi right in
            let new = prove $ Sequent (HS.insert p1 left) (HS.insert p2 right') in
        Just $ Unary seq new
    | otherwise = Just $ Unary seq Nothing
    where
        (leftNots, leftWONots) = partition isNot left
        (rightNots, rightWONots) = partition isNot right
        (leftAnds, leftWOAnds) = partition isAnd left
        (rightAnds, rightWOAnds) = partition isAnd right
        (leftOrs, leftWOOrs) = partition isOr left
        (rightOrs, rightWOOrs) = partition isOr right
        (leftImps, leftWOImps) = partition isImpl left
        (rightImps, rightWOImps) = partition isImpl right
        dropAnd (LAnd p q) = (p, q)
        dropOr (LOr p q) = (p, q)
        dropNot (LNot p) = p
        dropImp (LImpl p q) = (p, q)
        leftDropAnds = HS.foldl' (\x y -> HS.union x (HS.fromList [fst y, snd y])) HS.empty (HS.map dropAnd leftAnds)
        rightDropOrs = HS.foldl' (\x y -> HS.union x (HS.fromList [fst y, snd y])) HS.empty (HS.map dropOr rightOrs)

proveProp :: Proposition -> Maybe ProofTree
proveProp p = prove $ Sequent HS.empty (HS.singleton p)
