{-# LANGUAGE DeriveFunctor #-}

module Syntax where

import Data.List (intercalate)

-- Abbreviations.
type Name        = String
type F           = Name
type X           = Name
type C           = Name
type D           = Name
type P           = Name
type Index       = Integer
type T0        a = Term a
type T1        a = Term a
type T2        a = Term a
type Left      a = Term a
type Right     a = Term a
type K         a = Term a
type V         a = Term a
type Key         = Type
type Value       = Type
type Leaf      a = Term a
type Node      a = (Pattern a, Term a)
type Pattern   a = Term a
type Canonical a = Term a

data Program a
  = Data        D [TypeConstructor] (Program a)
  | Declaration X           Type    (Program a)
  | Definition  F          (Term a) (Program a)
  | Property    P [(X, a)] (Term a) (Program a)
  | EndOfProgram
  deriving (Functor, Eq)

data TypeConstructor = TypeConstructor C [Type]
  deriving Eq

data Type
  = Variable' Index
  | Integer'
  | Boolean'
  | Unit'
  | Type :*: Type
  | Type :->: Type
  | Algebraic D
  | BST Key Value
  deriving (Eq, Show)

data Term a =
    Number    Integer                   a
  | Boolean   Bool                      a
  | Unit                                a
  | Leaf                                a
  | Constructor C [Term a]              a
  | Node (Left a) (K a) (V a) (Right a) a
  | Case (T0 a) (Leaf a) (Node a)       a
  | Variable  Name                      a
  | If          (T0 a) (T1 a) (T2 a)    a
  | Plus        (T0 a) (T1 a)           a
  | Leq         (T0 a) (T1 a)           a
  | Pair        (T0 a) (T1 a)           a
  | Fst         (T0 a)                  a
  | Snd         (T0 a)                  a
  | Lambda Name (T0 a)                  a
  | Application        (T1 a) (T2 a)    a
  | Let Name           (T1 a) (T2 a)    a
  | Rec Name    (T0 a)                  a
  deriving (Functor, Eq)

-- Dealing with annotations.
class Annotated thing where
  annotation  :: thing a -> a
  annotations :: thing a -> [a]

instance Annotated Term where
  annotations (Number          _ a) = return a
  annotations (Boolean         _ a) = return a
  annotations (Variable        _ a) = return a
  annotations (Unit              a) = return a
  annotations (Constructor _ ts  a) = a : (ts           >>= annotations)
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
  annotations (Leaf              a) = return a
  annotations (Node      l k v r a) = a : ([l, k, v, r]  >>= annotations)
  annotations (Case  t0 l (p, n) a) = a : ([t0, l, p, n] >>= annotations)
  annotation  term                  = head $ annotations term

putParens :: String -> String
putParens = ("(" ++) . (++ ")")

instance Show a => Show (Program a) where
  show (Data d taus rest) =
    "data " ++ d ++ " = " ++ show taus ++ "\n\n" ++ show rest
  show (Declaration x t rest) =
    x ++ " :: " ++ show t ++ "\n\n" ++ show rest
  show (Definition x t rest) =
    x ++ " = " ++ show t ++ "\n\n" ++ show rest
  show (Property p xs t rest) =
    "property " ++ p ++ " " ++ show xs ++ " . " ++ show t ++ "\n\n" ++ show rest
  show EndOfProgram = ""

instance Show TypeConstructor where
  show (TypeConstructor c []) = c
  show (TypeConstructor c cs) = c ++ " (" ++ intercalate ", " (map show cs) ++ ")"

instance Show (Term a) where
  -- todo (minimally bracketed printer + tests) --
  show (Number  n         _) = show n
  show (Boolean b         _) = show b
  show (Unit              _) = "unit"
  show (Constructor c ts  _) = c ++ " [" ++ intercalate ", " (map show ts) ++ "]"
  show (Leaf              _) = "leaf"
  show (Node l k v r      _) = "[node " ++ show l ++ show k ++ show v ++ show r ++ "]"
  show (Case t l (p, n)   _) =
         "case " ++ show t ++ " of ; leaf -> " ++ show l ++ "; " ++ show p ++ " -> " ++ show n
  show (Variable n        _) = n
  show (If t0 t1 t2       _) = "if " ++ show t0  ++ " then " ++ show t1 ++ " else " ++ show t2
  show (Plus t0 t1        _) = putParens (show t0) ++ " + "  ++ putParens (show t1)
  show (Leq  t0 t1        _) = putParens (show t0) ++ " <= " ++ putParens (show t1)
  show (Pair t0 t1        _) = putParens $ show t0 ++ ", "   ++ show t1
  show (Fst  t0           _) = "fst " ++ putParens (show t0)
  show (Snd  t0           _) = "snd " ++ putParens (show t0)
  show (Lambda x t0       _) = putParens $ "\\" ++ x ++ " -> " ++ show t0
  show (Application t0 t1 _) = show t0 ++ " " ++ putParens (show t1)
  show (Let x t0 t1       _) = "let " ++ x ++ " = " ++ show t0 ++ " in " ++ show t1
  show (Rec x t0          _) = "rec " ++ x ++ " . " ++ show t0

canonical :: Term a -> Bool
canonical (Number  _        _) = True
canonical (Boolean _        _) = True
canonical (Unit             _) = True
canonical (Constructor _ ts _) = all canonical ts
canonical (Pair    t1 t2    _) = canonical t1 && canonical t2
canonical (Lambda  {}        ) = True
canonical (Leaf             _) = True
canonical (Node   l k v r   _) = all canonical [l, k, v, r]
canonical _                    = False

dataDeclarations :: Program a -> [(D, [TypeConstructor])]
dataDeclarations (Data        d ts rest) = (d, ts) : dataDeclarations rest
dataDeclarations (Definition  _ _  rest) = dataDeclarations rest
dataDeclarations (Declaration _ _  rest) = dataDeclarations rest
dataDeclarations (Property  _ _ _  rest) = dataDeclarations rest
dataDeclarations _                       = mempty

definitions :: Program a -> [(F, Term a)]
definitions (Data        d ts rest) = definitions rest
definitions (Definition  x t  rest) = (x, t) : definitions rest
definitions (Declaration _ _  rest) = definitions rest
definitions (Property  _ _ _  rest) = definitions rest
definitions _                       = mempty

declarations :: Program a -> [(X, Type)]
declarations (Data        d ts rest) = declarations rest
declarations (Definition  _ _  rest) = declarations rest
declarations (Declaration x t  rest) = (x, t) : declarations rest
declarations (Property  _ _ _  rest) = declarations rest
declarations _                       = mempty

properties :: Program a -> [(P, ([(X, a)], Term a))]
properties (Data        d ts rest) = properties rest
properties (Definition  _ _  rest) = properties rest
properties (Declaration _ _  rest) = properties rest
properties (Property  p x t  rest) = (p, (x, t)) : properties rest
properties _                       = mempty

indices :: Type -> [Index]
indices (Variable' a)  = [a]
indices  Integer'      = []
indices  Boolean'      = []
indices  Unit'         = []
indices (t1 :*:   t2)  = indices t1 <> indices t2
indices (t1 :->:  t2)  = indices t1 <> indices t2
indices (BST   t1 t2)  = indices t1 <> indices t2
indices (Algebraic d)  = []

instance Semigroup (Program a) where
  (Declaration x t p1) <> p2 = Declaration x  t (p1 <> p2)
  (Definition  x t p1) <> p2 = Definition  x  t (p1 <> p2)
  (Property p xs t p1) <> p2 = Property  p xs t (p1 <> p2)
  EndOfProgram         <> p2 = p2

instance Monoid (Program a) where
  mempty  = EndOfProgram
  mappend = (<>)

freeVariables :: Term a -> [Name]
freeVariables (Number _ _) = mempty
freeVariables (Boolean _ _) = mempty
freeVariables (Unit      _) = mempty
freeVariables (Leaf      _) = mempty
freeVariables (Constructor _ ts _) =
  foldr (\t fvs -> fvs <> freeVariables t) mempty ts
freeVariables (Node l k v r _) =
     freeVariables l
  <> freeVariables k
  <> freeVariables v
  <> freeVariables r
freeVariables (Case t l (p, b) _) =
     freeVariables t
  <> freeVariables l
  <> freeVariables p
  <> freeVariables b
freeVariables (Variable x _) = return x
freeVariables (If t0 t1 t2 _) =
     freeVariables t0
  <> freeVariables t1
  <> freeVariables t2
freeVariables (Plus t0 t1 _) =
     freeVariables t0
  <> freeVariables t1
freeVariables (Leq t0 t1 _) =
     freeVariables t0
  <> freeVariables t1
freeVariables (Pair t0 t1 _) =
     freeVariables t0
  <> freeVariables t1
freeVariables (Fst t0 _) = freeVariables t0
freeVariables (Snd t0 _) = freeVariables t0
freeVariables (Lambda x t0 _) = [ y | y <- freeVariables t0 , x /= y ]
freeVariables (Application t0 t1 _) =
     freeVariables t0
  <> freeVariables t1
freeVariables (Let x t0 t1 _) =
     freeVariables t0
  <> [ y | y <- freeVariables t1, y /= x ]
freeVariables (Rec x t0 _) = [ y | y <- freeVariables t0 , x /= y ]

-- /!\ optimisation : declarations can be thrown away at runtime to make the program smaller.
withoutDeclarations :: Program a -> Program a
withoutDeclarations (Declaration _ _ p)  =                     withoutDeclarations p
withoutDeclarations (Definition  x t p)  = Definition   x  t $ withoutDeclarations p
withoutDeclarations (Property  n xs t p) = Property   n xs t $ withoutDeclarations p
withoutDeclarations EndOfProgram         = EndOfProgram

declarationsUpFront :: Program a -> Program a
declarationsUpFront p =
  foldr (uncurry Declaration) (withoutDeclarations p) (declarations p)
