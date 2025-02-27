module Unification where

import Syntax
import Data.List  (nub, intercalate)
import Data.Maybe (isNothing, fromMaybe)

type Unifier a = [(Name, Term a)]

unify :: Show a => Term a -> [(Pattern a, Term a)] -> (Unifier a, Term a)
unify _ [] = error "non-exhaustive patterns in case statement"
unify v ((p, n):ps) =
  if   isPattern p
  then
      (case unify' v p of
        Nothing -> unify v ps
        Just u  -> if   names == nub names
                   then (u, n)
                   else error $ "conflicting bindings for " ++ 
                                intercalate ", " (repeated names)
                   where
                    names = map fst u)
  else error $ show p ++ " is not a legal pattern for case statements"

unify' :: Canonical a -> Pattern a -> Maybe (Unifier a)
unify' (Number v   _) (Number  v'   _) | v == v' = return []
unify' (Boolean v  _) (Boolean v'   _) | v == v' = return []
unify' v              (Variable x   _) = return $ v `substitutes` x
unify' (Constructor c vs _) (Constructor c' vs' _)
  | c == c' && length vs == length vs'
  = validateUnifiers $ zipWith unify' vs vs'
unify' _ _ = Nothing

validateUnifiers :: [Maybe (Unifier a)] -> Maybe (Unifier a)
validateUnifiers us
  | any isNothing us = Nothing
  | otherwise        = Just $ foldr (mappend . fromMaybe []) mempty us

isPattern :: Term a -> Bool
isPattern (Variable       _ _) = True
isPattern (Constructor _ ts _) = all isPattern ts
isPattern t                    = canonical t

contains :: Pattern a -> X -> Bool
contains (Variable y       _) x = x == y
contains (Constructor _ ts _) x = any (`contains` x) ts
contains (If t0 t1 t2      _) x = any (`contains` x) [t0, t1, t2]
contains (Plus  t0 t1      _) x = t0 `contains` x || t1 `contains` x
contains (Leq   t0 t1      _) x = t0 `contains` x || t1 `contains` x
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
