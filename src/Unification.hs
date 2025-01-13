module Unification where

import Syntax
import Data.List (nub, intercalate)

type Unifier a = [(Name, Term a)]


unify :: Show a => Canonical a -> Pattern a -> Unifier a
unify v p = if isPattern p
  then
    (case unify' v p of
      Nothing -> error "non-exhaustive patterns in case statement"
      Just  u -> if names == nub names
        then u
        else error $ "conflicting bindings for " ++
                     intercalate ", " (repeated names)
        where
          names = map fst u)
  else
    error $ show p ++ " is not a legal pattern for case statements"

unify' :: Canonical a -> Pattern a -> Maybe (Unifier a)
unify' (Number v   _) (Number  v'   _) | v == v' = return []
unify' (Boolean v  _) (Boolean v'   _) | v == v' = return []
unify' v              (Variable x   _) = return $ v `substitutes` x
unify' (Pair t0 t1 _) (Pair t0' t1' _) = unify' t0 t0' `mappend` unify' t1 t1'
unify' (Leaf       _) (Leaf         _) = return []
unify' (Node l1 k1 v1 r1 _) (Node l2 k2 v2 r2 _) =
  (unify' l1 l2  `mappend`
   unify' k1 k2) `mappend`
  (unify' v1 v2  `mappend`
   unify' r1 r2)
unify' _ _ = Nothing

isPattern :: Term a -> Bool
isPattern (Variable    _ _) = True
isPattern (Node  l k v r _) = all isPattern [l, k, v, r]
isPattern (Pair  t1 t2   _) = all isPattern [t1, t2]
isPattern t                 = canonical t

contains :: Pattern a -> X -> Bool
contains (Variable y       _) x = x == y
contains (Constructor _ ts _) x = any (`contains` x) ts
contains (If t0 t1 t2      _) x = any (`contains` x) [t0, t1, t2]
contains (Plus  t0 t1      _) x = t0 `contains` x || t1 `contains` x
contains (Leq   t0 t1      _) x = t0 `contains` x || t1 `contains` x
contains (Pair  t0 t1      _) x = t0 `contains` x || t1 `contains` x
contains (Fst   t0         _) x = t0 `contains` x
contains (Snd   t0         _) x = t0 `contains` x
contains (Lambda n t0      _) x = n == x || t0 `contains` x
contains (Let n t1 t2      _) x =
  n   ==        x ||
  t1 `contains` x ||
  t2 `contains` x
contains (Rec n t0          _) x = n == x || t0 `contains` x
contains (Application t1 t2 _) x = t1 `contains` x || t2 `contains` x
contains _                     _ = False

substitutes :: Pattern a -> X -> Unifier a
substitutes p x = return (x, p)

repeated :: Eq a => [a] -> [a]
repeated []     = []
repeated (x:xs) = [ x | x `elem` xs ] ++ repeated xs
