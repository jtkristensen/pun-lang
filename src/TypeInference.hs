{-# LANGUAGE TypeOperators #-}

module TypeInference where

import Syntax
import Control.Monad.RWS
import Data.Maybe (fromMaybe)

data Constraint
  = Type :=: Type
  deriving Show

-- Abbreviations.
type Mapping a b  = a -> b
type Mapsto  a b  = Mapping a b -> Mapping a b
type Environment  = Mapping Name Type
type Annotation   = RWS Environment [Constraint] Index
type Substitution = [(Index, Type)]

hole :: Annotation Type
hole = Variable' <$> (get >>= \i -> put (i + 1) >> return i)

bind :: Eq x => x -> a -> x `Mapsto` a
bind x a look y = if x == y then a else look y

hasSameTypeAs :: Term Type -> Term Type -> Annotation ()
t0 `hasSameTypeAs` t1 = tell [annotation t0 :=: annotation t1]

hasType :: Term Type-> Type -> Annotation ()
t0 `hasType` tau = tell [annotation t0 :=: tau]

annotate :: Term a -> Annotation (Term Type)
annotate (Number   n _)  = return $ Number n Integer'
annotate (Boolean  b _)  = return $ Boolean b Boolean'
annotate (Variable x _)  =
  do env <- ask
     return $ Variable x $ env x
annotate (If t0 t1 t2 _) =
  do t0' <- annotate t0
     t1' <- annotate t1
     t2' <- annotate t2
     t0' `hasType` Boolean'
     t1' `hasSameTypeAs` t2'
     return $ If t0' t1' t2' (annotation t1')
annotate (Plus t0 t1 _) =
  do t0' <- annotate t0
     t1' <- annotate t1
     t0' `hasType` Integer'
     t1' `hasType` Integer'
     return $ Plus t0' t1' Integer'
annotate (Leq t0 t1 _) =
  do t0' <- annotate t0
     t1' <- annotate t1
     t0' `hasType` Integer'
     t1' `hasType` Integer'
     return $ Leq t0' t1' Boolean'
annotate (Pair t0 t1 _) =
  do t0' <- annotate t0
     t1' <- annotate t1
     return $ Pair t0' t1' (annotation t0' :*: annotation t1')
annotate (Fst t0 _) =
  do t0'  <- annotate t0
     tau1 <- hole
     tau2 <- hole
     t0'  `hasType` (tau1 :*: tau2)
     return $ Fst t0' tau1
annotate (Snd t0 _) =
  do t0'  <- annotate t0
     tau1 <- hole
     tau2 <- hole
     t0'  `hasType` (tau1 :*: tau2)
     return $ Snd t0' tau2
annotate (Lambda x t0 _) =
  do  tau <- hole
      t0' <- local (bind x tau) $ annotate t0
      return $ Lambda x t0' (tau :->: annotation t0')
annotate (Application t0 t1 _) =
  do tau <- hole
     t0' <- annotate t0
     t1' <- annotate t1
     t0' `hasType` (annotation t1' :->: tau)
     return $ Application t0' t1' tau
annotate (Let x t0 t1 _) =
  do t0' <- annotate t0
     t1' <- local (bind x (annotation t0')) $ annotate t1
     return $ Let x t0' t1' (annotation t1')
annotate (Rec f t0 _) =
  do tau <- hole
     t0' <- local (bind f tau) $ annotate t0
     return $ Rec f t0' $ annotation t0'
annotate (Leaf _) =
  do tau1 <- hole
     tau2 <- hole
     return $ Leaf (BST tau1 tau2)
annotate (Node l k v r _) =
  do l' <- annotate l
     k' <- annotate k
     v' <- annotate v
     r' <- annotate r
     l' `hasType` (BST (annotation k') (annotation v'))
     l' `hasSameTypeAs` r'
     return $ Node l' k' v' r' (annotation l')
annotate (Case t0 l (p, n) _) =
  do tau1 <- hole
     tau2 <- hole
     t0'  <- annotate t0
     t0' `hasType` (BST tau1 tau2)
     l'   <- annotate l
     p'   <- annotate p
     n'   <- annotate n
     l'  `hasSameTypeAs` n'
     return $ Case t0' l' (p', n') (annotation l')

solve :: [Constraint] -> Maybe Substitution
solve [                 ] = return mempty
solve (constraint : rest) =
  case constraint of
    Integer'      :=: Integer'      -> solve rest
    Boolean'      :=: Boolean'      -> solve rest
    (t0 :*:  t1)  :=: (t2 :*:  t3)  -> solve $ (t0 :=: t2) : (t1 :=: t3) : rest
    (t0 :->: t1)  :=: (t2 :->: t3)  -> solve $ (t0 :=: t2) : (t1 :=: t3) : rest
    (BST    k v)  :=: (BST  k' v')  -> solve $ (k  :=: k') : (v  :=: v') : rest
    (Variable' i) :=: t1            ->
      if   i `elem` indexes t1
      then (if Variable' i /= t1 then Nothing else solve rest)
      else do c <- solve (substitute t1 i <$> rest)
              return $ (i, t1) : c
    t0            :=: Variable' i ->
      if   i `elem` indexes t0
      then (if Variable' i /= t0 then Nothing else solve rest)
      else do c <- solve (substitute t0 i <$> rest)
              return $ (i, t0) : c
    _                               -> Nothing
class HasSubstitution thing where
  substitute :: Type -> Index -> (thing -> thing)

instance HasSubstitution Type where
  substitute t i (Variable' j) | i == j = t
  substitute t i (t0 :*: t1)            = substitute t i t0 :*: substitute t i t1
  substitute t i (t0 :->: t1)           = substitute t i t0 :->: substitute t i t1
  substitute t i (BST    k v)           = BST (substitute t i k) (substitute t i v)
  substitute _ _ t0                     = t0

instance HasSubstitution Constraint where
  substitute t i (t0 :=: t1) = substitute t i t0 :=: substitute t i t1

-- Utility functions.
indexes :: Type -> [Index]
indexes (Variable' i) = return i
indexes (t0 :*:  t1)  = indexes t0 ++ indexes t1
indexes (t0 :->: t1)  = indexes t0 ++ indexes t1
indexes (BST    k v)  = indexes k  ++ indexes v
indexes _             = mempty

infer :: Term a -> Index -> (Term Type, Index, [Constraint])
infer term = runRWS (annotate term) $ error . (++ " is unbound!")

-- TODO: Better error handling {^o^}!
bindings :: [Constraint] -> Substitution
bindings = fromMaybe (error "type error") . solve

refine :: Substitution -> (Type -> Type)
refine s o = refine' s o
  where
    refine' [          ] t                      = t
    refine' ((i, t) : _) (Variable' j) | i == j = refine' s t
    refine' (_   : rest) (Variable' j)          = refine' rest (Variable' j)
    refine' _            Integer'               = Integer'
    refine' _            Boolean'               = Boolean'
    refine' s'           (t0 :*: t1)            = refine' s' t0 :*:  refine' s' t1
    refine' s'           (t0 :->: t1)           = refine' s' t0 :->: refine' s' t1
    refine' s'           (BST    k v)           = BST (refine s' k) (refine s' v)

-- Just here for documentation
usage :: Term a -> Index -> (Term Type, Index)
usage t i = (fmap (refine (bindings cs)) t', j)
  where (t', j, cs) = infer t i
