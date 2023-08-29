module Unification where

import Syntax

type Unifier a = [(Name, Term a)]

unify :: Show a => Canonical a -> Pattern a -> Maybe (Unifier a)
unify v p = if isPattern p
  -- todo! Check for conflicting bindings in unifier
  -- (map fst $ unify' v p) == (nub $ map fst $ unify' v p)
  -- Conflicting definitions for 'x'
  then unify' v p
  else error $ show p ++ " is not a legal pattern for case statements"

unify' :: Canonical a -> Pattern a -> Maybe (Unifier a)
unify' (Number v   _) (Number  v'   _) | v == v' = return []
unify' (Boolean v  _) (Boolean v'   _) | v == v' = return []
unify' v              (Variable x   _) = return $ v `substitutes` x
unify' (Pair t0 t1 _) (Pair t0' t1' _) = unify' t0 t0' `mappend` unify' t1 t1'
unify' (Leaf       _) (Leaf         _) = return $ []
unify' (Node l1 t0 r1 _) (Node l2 t0' r2 _) =
  ((unify' l1  l2)   `mappend`
   (unify' t0  t0')) `mappend`
   (unify' r1  r2)
unify' _ _ = Nothing

isPattern :: Term a -> Bool
isPattern (Variable    _ _) = True
isPattern (Node  l  t0 r _) = all isPattern [l,  t0, r]
isPattern (Pair  t1 t2   _) = all isPattern [t1, t2]
isPattern t                 = canonical t

contains :: Pattern a -> X -> Bool
contains (Variable y  _) x = x == y
contains (If t0 t1 t2 _) x = any (`contains` x) [t0, t1, t2]
contains (Plus  t0 t1 _) x = t0 `contains` x || t1 `contains` x
contains (Leq   t0 t1 _) x = t0 `contains` x || t1 `contains` x
contains (Pair  t0 t1 _) x = t0 `contains` x || t1 `contains` x
contains (Fst   t0    _) x = t0 `contains` x
contains (Snd   t0    _) x = t0 `contains` x
contains (Lambda n t0 _) x = n == x || t0 `contains` x
contains (Let n t1 t2 _) x =
  n   ==        x ||
  t1 `contains` x ||
  t2 `contains` x
contains (Rec n t0    _) x = n == x || t0 `contains` x
contains (Application t1 t2 _) x = t1 `contains` x || t2 `contains` x
contains _              _ = False

substitutes :: Pattern a -> X -> Unifier a
substitutes p x = return $ (x, p)
