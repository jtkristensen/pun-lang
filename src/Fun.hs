---------------------------------------------------
-- Title : Implementing type systems.
-- By    : Joachim Tilsted Kristensen
-- Date  : May 23 2023.
---------------------------------------------------

{-# LANGUAGE TypeOperators, DeriveFunctor #-}

module Fun where

-- We present two alternative representations of the FUN programming
-- langauge. The former is a demonstration of promoting the types of the
-- interpreted language into the language in which we are writing the
-- interpreter, which makes reasoning much easier.
-- In the latter approach, the syntax tree has been annotated with types.

import Control.Monad.RWS

-- ========================== Type annotations ======================== --

-- Abbreviations.
type Name        = String
type T0        a = Term a
type T1        a = Term a
type T2        a = Term a

-- Annotated terms.
data Term a =
    Number    Integer                 a
  | Boolean   Bool                    a
  | Variable  Name                    a
  | If          (T0 a) (T1 a) (T2 a)  a
  | Plus        (T0 a) (T1 a)         a
  | Leq         (T0 a) (T1 a)         a
  | Pair        (T0 a) (T1 a)         a
  | Fst         (T0 a)                a
  | Snd         (T0 a)                a
  | Lambda Name (T0 a)                a
  | Application        (T1 a) (T2 a)  a
  | Let Name           (T1 a) (T2 a)  a
  | Rec Name    (T0 a)                a
  deriving (Functor, Show)

-- Dealing with annotations.
class Annotated thing where
  annotation  :: thing a -> a
  annotations :: thing a -> [a]

instance Annotated Term where
  annotations (Number          _ a) = return a
  annotations (Boolean         _ a) = return a
  annotations (Variable        _ a) = return a
  annotations (If       t0 t1 t2 a) = a : ([t0, t1, t2] >>= annotations)
  annotations (Plus     t0 t1    a) = a : ([t0, t1]     >>= annotations)
  annotations (Leq      t0 t1    a) = a : ([t0, t1]     >>= annotations)
  annotations (Pair     t0 t1    a) = a : ([t0, t1]     >>= annotations)
  annotations (Let    _    t1 t2 a) = a : ([t1, t2]     >>= annotations)
  annotations (Application t1 t2 a) = a : ([t1, t2]     >>= annotations)
  annotations (Fst      t0       a) = a : annotations t0
  annotations (Snd      t0       a) = a : annotations t0
  annotations (Lambda _ t0       a) = a : annotations t0
  annotations (Rec    _ t0       a) = a : annotations t0
  annotation  term                  = head $ annotations term

-- ============ Type Inference alla Andrzej Filinski ================= --

-- Open and closed types.
data Open = Hole Index | Num' | Bool' | Open :**: Open | Open :-->: Open
  deriving (Eq, Show)
data Type =              Num  | Bool  | Type :*:  Type | Type :->:  Type
  deriving (Eq, Show)

-- Abbreviations.
type Mapping a b  = a -> b
type Mapsto  a b  = Mapping a b -> Mapping a b
type Environment  = Mapping Name Open
type Index        = Integer
type Annotation   = RWS Environment [Constraint] Index
type Substitution = [(Index, Open)]

-- /!\ The important part
data Constraint = Open :==: Open
  deriving Show

hole :: Annotation Open
hole = Hole <$> (get >>= \i -> put (i + 1) >> return i)

bind :: Eq x => x -> a -> x `Mapsto` a
bind x a look = \y -> if x == y then a else look y

hasSameTypeAs :: Term Open -> Term Open -> Annotation ()
t0 `hasSameTypeAs` t1 = tell [annotation t0 :==: annotation t1]

hasType :: Term Open -> Open -> Annotation ()
t0 `hasType` tau = tell [annotation t0 :==: tau]

annotate :: Term a -> Annotation (Term Open)
annotate (Number   n _)  = return $ Number n Num'
annotate (Boolean  b _)  = return $ Boolean b Bool'
annotate (Variable x _)  =
  do env <- ask
     return $ Variable x $ env x
annotate (If t0 t1 t2 _) =
  do t0' <- annotate t0
     t1' <- annotate t1
     t2' <- annotate t2
     t0' `hasType` Bool'
     t1' `hasSameTypeAs` t2'
     return $ If t0' t1' t2' (annotation t0')
annotate (Plus t0 t1 _) =
  do t0' <- annotate t0
     t1' <- annotate t1
     t0' `hasType` Num'
     t1' `hasType` Num'
     return $ Plus t0' t1' Num'
annotate (Leq t0 t1 _) =
  do t0' <- annotate t0
     t1' <- annotate t1
     t0' `hasType` Num'
     t1' `hasType` Num'
     return $ Leq t0' t1' Bool'
annotate (Pair t0 t1 _) =
  do t0' <- annotate t0
     t1' <- annotate t1
     return $ Pair t0' t1' (annotation t0' :**: annotation t1')
annotate (Fst t0 _) =
  do t0'  <- annotate t0
     tau1 <- hole
     tau2 <- hole
     t0'  `hasType` (tau1 :**: tau2)
     return $ Fst t0' tau1
annotate (Snd t0 _) =
  do t0'  <- annotate t0
     tau1 <- hole
     tau2 <- hole
     t0'  `hasType` (tau1 :**: tau2)
     return $ Snd t0' tau2
annotate (Lambda x t0 _) =
  do  tau <- hole
      t0' <- local (bind x tau) $ annotate t0
      return $ Lambda x t0' (tau :-->: annotation t0')
annotate (Application t0 t1 _) =
  do tau <- hole
     t0' <- annotate t0
     t1' <- annotate t1
     t0' `hasType` (annotation t1' :-->: tau)
     return $ Application t0' t1' tau
annotate (Let x t0 t1 _) =
  do t0' <- annotate t0
     t1' <- local (bind x (annotation t0')) $ annotate t1
     return $ Let x t0' t1' (annotation t1')
annotate (Rec f t0 _) =
  do tau <- hole
     t0' <- local (bind f tau) $ annotate t0
     return $ Rec f t0' $ annotation t0'

solve :: [Constraint] -> Maybe Substitution
solve [                 ] = return $ mempty
solve (constraint : rest) =
  case constraint of
    Num'          :==: Num'          -> solve rest
    Bool'         :==: Bool'         -> solve rest
    (t0 :**:  t1) :==: (t2 :**:  t3) -> solve $ (t0 :==: t2) : (t1 :==: t3) : rest
    (t0 :-->: t1) :==: (t2 :-->: t3) -> solve $ (t0 :==: t2) : (t1 :==: t3) : rest
    (Hole i     ) :==: t1            ->
      if   i `elem` indexes t1
      then (if (Hole i) /= t1 then Nothing else solve rest)
      else do c <- solve (substitute t1 i <$> rest)
              return $ (i, t1) : c
    t0            :==: (Hole i)      ->
      if   i `elem` indexes t0
      then (if (Hole i) /= t0 then Nothing else solve rest)
      else do c <- solve (substitute t0 i <$> rest)
              return $ (i, t0) : c
    _                                -> Nothing

class HasSubstitution thing where
  substitute :: Open -> Index -> (thing -> thing)

instance HasSubstitution Open where
  substitute t i (Hole j) | i == j = t
  substitute t i (t0 :**: t1)      = substitute t i t0 :**: substitute t i t1
  substitute t i (t0 :-->: t1)     = substitute t i t0 :-->: substitute t i t1
  substitute _ _ t0                = t0

instance HasSubstitution Constraint where
  substitute t i (t0 :==: t1) = substitute t i t0 :==: substitute t i t1

-- Utility functions.
indexes :: Open -> [Index]
indexes (Hole i     ) = return i
indexes (t0 :**:  t1) = indexes t0 ++ indexes t1
indexes (t0 :-->: t1) = indexes t0 ++ indexes t1
indexes _             = mempty

type_inference :: Term a -> Term Type
type_inference term =
  let gamma x                 = error $ x ++ " is unbound!"
      (term', _, constraints) = runRWS (annotate term) gamma 0
      bindings                =
        case solve constraints of
          Nothing       -> error "type error"
          Just solution -> solution
      refine :: Substitution -> Open -> Type
      refine s o = refine' s o
        where
          refine' [          ] (Hole _)          = Num --How about polymorphism ?
          refine' ((i, t) : _) (Hole j) | i == j = refine' s t
          refine' (_   : rest) (Hole j)          = refine' rest (Hole j)
          refine' _            Num'              = Num
          refine' _            Bool'             = Bool
          refine' s'           (t0 :**: t1)      = refine' s' t0 :*:  refine' s' t1
          refine' s'           (t0 :-->: t1)     = refine' s' t0 :->: refine' s' t1
  in refine bindings <$> term'
