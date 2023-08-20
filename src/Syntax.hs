{-# LANGUAGE DeriveFunctor #-}

module Syntax where

-- Abbreviations.
type Name        = String
type X           = Name
type C           = Name
type P           = Name
type Index       = Integer
type T0        a = Term a
type T1        a = Term a
type T2        a = Term a

data Program a
  = Declaration X     Type     (Program a)
  | Definition  X     (Term a) (Program a)
  | Property    P [X] (Term a) (Program a)
  | EndOfProgram
  deriving (Functor, Eq, Show)

data Type
  = Variable' Index
  | Integer'
  | Boolean'
  | Type :*: Type
  | Type :->: Type
  deriving (Eq, Show)

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
  deriving (Functor, Eq, Show)

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

definitions :: Program a -> [(X, Term a)]
definitions (Definition  x t rest) = (x, t) : definitions rest
definitions (Declaration _ _ rest) = definitions rest
definitions (Property  _ _ _ rest) = definitions rest
definitions _                      = mempty

declarations :: Program a -> [(X, Type)]
declarations (Definition  _ _ rest) = declarations rest
declarations (Declaration x t rest) = (x, t) : declarations rest
declarations (Property  _ _ _ rest) = declarations rest
declarations _                      = mempty

properties :: Program a -> [(P, ([X], Term a))]
properties (Definition  _ _ rest) = properties rest
properties (Declaration _ _ rest) = properties rest
properties (Property  p x t rest) = (p, (x, t)) : properties rest
properties _                      = mempty

indicies :: Type -> [Index]
indicies (Variable' a) = [a]
indicies  Integer'     = []
indicies  Boolean'     = []
indicies (t1 :*:   t2) = indicies t1 <> indicies t2
indicies (t1 :->:  t2) = indicies t1 <> indicies t2
