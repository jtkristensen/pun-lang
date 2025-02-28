{-# LANGUAGE DeriveFunctor #-}

module Syntax where

import Data.List (intercalate)

-- Abbreviations.
type Name             = String
type F                = Name
type X                = Name
type C                = Name
type D                = Name
type P                = Name
type Index            = Integer
type T0             a = Term a
type T1             a = Term a
type T2             a = Term a
type Left           a = Term a
type Right          a = Term a
type K              a = Term a
type V              a = Term a
type Key              = Type
type Value            = Type
type Leaf           a = Term a
type Node           a = (Pattern a, Term a)
type Pattern        a = Term a
type Canonical      a = Term a
type DataDeclarations = [(D, [TypeConstructor])]

data Program a
  = Data        D [TypeConstructor] (Program a)
  | Declaration X           Type    (Program a)
  | Definition  F          (Term a) (Program a)
  | Property    P [(X, a)] (Term a) (Program a)
  | EndOfProgram
  deriving (Functor, Eq)

data TypeConstructor = TypeConstructor C [Type]
  deriving (Eq)

data Type
  = Variable' Index
  | Integer'
  | Boolean'
  | Type :->: Type
  | Algebraic D
  deriving (Eq, Show)

data Term a =
    Number    Integer                   a
  | Boolean   Bool                      a
  | Constructor C [Term a]              a
  | Case (T0 a) [(Pattern a, Term a)]   a
  | Variable  Name                      a
  | If          (T0 a) (T1 a) (T2 a)    a
  | Plus        (T0 a) (T1 a)           a
  | Leq         (T0 a) (T1 a)           a
  | Equal       (T0 a) (T1 a)           a
  | Lambda Name (T0 a)                  a
  | Application        (T1 a) (T2 a)    a
  | Let Name           (T1 a) (T2 a)    a
  | Rec Name    (T0 a)                  a
  deriving (Functor)

-- Shell commands
data ShellCommand a =
    Quit
  | Load     FilePath
  | Evaluate (Term a)
  deriving Show

-- Dealing with annotations.
class Annotated thing where
  annotation  :: thing a -> a
  annotations :: thing a -> [a]

instance Annotated Term where
  annotations (Number          _ a) = return a
  annotations (Boolean         _ a) = return a
  annotations (Variable        _ a) = return a
  annotations (Constructor _ ts  a) = a : (ts           >>= annotations)
  annotations (If       t0 t1 t2 a) = a : ([t0, t1, t2] >>= annotations)
  annotations (Plus     t0 t1    a) = a : ([t0, t1]     >>= annotations)
  annotations (Leq      t0 t1    a) = a : ([t0, t1]     >>= annotations)
  annotations (Let    _    t1 t2 a) = a : ([t1, t2]     >>= annotations)
  annotations (Application t1 t2 a) = a : ([t1, t2]     >>= annotations)
  annotations (Equal    t0 t1    a) = a : ([t0, t1]     >>= annotations)
  annotations (Lambda _ t0       a) = a : annotations t0
  annotations (Rec    _ t0       a) = a : annotations t0
  annotations (Case  t cs        a) = a : (t:concatMap (\(p, r) -> [p, r]) cs >>= annotations)
  annotation  term                  = head $ annotations term

-- Utility functions
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

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
  show (Constructor c ts  _) = c ++ " [" ++ intercalate ", " (map show ts) ++ "]"
  show (Case t cs         _) = "case " ++ show t ++ " of\n" ++ intercalate "\n" (map (\(x, y) -> "  ; " ++ show x ++  " -> " ++ show y) cs)
  show (Variable n        _) = n
  show (If t0 t1 t2       _) = "if " ++ show t0  ++ " then " ++ show t1 ++ " else " ++ show t2
  show (Plus  t0 t1       _) = putParens (show t0) ++ " + "  ++ putParens (show t1)
  show (Leq   t0 t1       _) = putParens (show t0) ++ " <= " ++ putParens (show t1)
  show (Equal t0 t1       _) = show t0 ++ " == " ++ show t1
  show (Lambda x t0       _) = putParens $ "\\" ++ x ++ " -> " ++ show t0
  show (Application t0 t1 _) = show t0 ++ " " ++ putParens (show t1)
  show (Let x t0 t1       _) = "let " ++ x ++ " = " ++ show t0 ++ " in " ++ show t1
  show (Rec x t0          _) = "rec " ++ x ++ " . " ++ show t0

instance Eq (Term a) where
  (Number          n _) == (Number          n' _) = n == n'
  (Boolean         b _) == (Boolean         b' _) = b == b'
  (Constructor  c ts _) == (Constructor c' ts' _) = c == c' &&
                                                    and (zipWith (==) ts ts')
  (Variable        x _) == (Variable         y _) = x == y
  (If       t0 t1 t2 _) == (If     t0' t1' t2' _) = t0 == t0' &&
                                                    t1 == t1' &&
                                                    t2 == t2'
  (Plus        t0 t1 _) == (Plus       t0' t1' _) = t0 == t0' && t1 == t1'
  (Leq         t0 t1 _) == (Leq        t0' t1' _) = t0 == t0' && t1 == t1'
  (Equal       t0 t1 _) == (Equal      t0' t1' _) = t0 == t0' && t1 == t1'
  (Lambda      x  t  _) == (Lambda     x'  t'  _) = x  == x'  && t  == t'
  (Let       x t0 t1 _) == (Let      y t0' t1' _) = x == y   &&
                                                  t0 == t0' &&
                                                  t1 == t1'
  (Case t0 cases _) == (Case t0' cases' _) =
    t0 == t0' &&
    all (\((x, y), (x', y')) -> x == x' && y == y') (zip cases cases')
  (Application t1 t2 _) == (Application t1' t2' _) = t1 == t1' && t2 == t2'
  (Rec          x t0 _) == (Rec          x' t0' _) = x == x' && t0 == t0'
  _                     == _                       = False

canonical :: Term a -> Bool
canonical (Number  _        _) = True
canonical (Boolean _        _) = True
canonical (Constructor _ ts _) = all canonical ts
canonical (Lambda  {}        ) = True
canonical _                    = False

dataDeclarations :: Program a -> DataDeclarations
dataDeclarations (Data        d ts rest) = (d, ts) : dataDeclarations rest
dataDeclarations (Definition  _ _  rest) = dataDeclarations rest
dataDeclarations (Declaration _ _  rest) = dataDeclarations rest
dataDeclarations (Property  _ _ _  rest) = dataDeclarations rest
dataDeclarations _                       = mempty

typeConstructors :: Program a -> [(TypeConstructor, D)]
typeConstructors p = concatMap (fromConstructor . swap) (dataDeclarations p)
  where
    fromConstructor :: ([TypeConstructor], D) -> [(TypeConstructor, D)]
    fromConstructor (ctrs, t) = [ (c, t) | c <- ctrs ]

typeConstructorNames :: Program a -> [(C, D)]
typeConstructorNames p = map (\(TypeConstructor c _, t) -> (c, t)) (typeConstructors p)

typeConstructorFields :: Program a -> [(C, [Type])]
typeConstructorFields p = map (\(TypeConstructor c ts) -> (c, ts)) (concatMap snd (dataDeclarations p))

constructors :: DataDeclarations -> D -> [TypeConstructor]
constructors dataDecls d = case lookup d dataDecls of
  Just cs -> cs
  Nothing -> error $ "Algebraic data type '" ++ d ++ "' not defined."

constructorFields :: TypeConstructor -> [Type]
constructorFields (TypeConstructor _ types) = types

definitions :: Program a -> [(F, Term a)]
definitions (Data        _ _ rest) = definitions rest
definitions (Definition  x t rest) = (x, t) : definitions rest
definitions (Declaration _ _ rest) = definitions rest
definitions (Property  _ _ _ rest) = definitions rest
definitions _                      = mempty

declarations :: Program a -> [(X, Type)]
declarations (Data        _ _ rest) = declarations rest
declarations (Definition  _ _ rest) = declarations rest
declarations (Declaration x t rest) = (x, t) : declarations rest
declarations (Property  _ _ _ rest) = declarations rest
declarations _                      = mempty

properties :: Program a -> [(P, ([(X, a)], Term a))]
properties (Data        _ _ rest) = properties rest
properties (Definition  _ _ rest) = properties rest
properties (Declaration _ _ rest) = properties rest
properties (Property  p x t rest) = (p, (x, t)) : properties rest
properties _                      = mempty

indices :: Type -> [Index]
indices (Variable' a) = [a]
indices  Integer'     = []
indices  Boolean'     = []
indices (t1 :->:  t2) = indices t1 <> indices t2
indices (Algebraic _) = []

instance Semigroup (Program a) where
  (Data        d ts p1) <> p2 = Data        d ts (p1 <> p2)
  (Declaration x t  p1) <> p2 = Declaration x  t (p1 <> p2)
  (Definition  x t  p1) <> p2 = Definition  x  t (p1 <> p2)
  (Property p xs t  p1) <> p2 = Property  p xs t (p1 <> p2)
  EndOfProgram          <> p2 = p2

instance Monoid (Program a) where
  mempty  = EndOfProgram
  mappend = (<>)

freeVariables :: Term a -> [Name]
freeVariables (Number  _ _) = mempty
freeVariables (Boolean _ _) = mempty
freeVariables (Constructor _ ts _) =
  foldr (\t fvs -> fvs <> freeVariables t) mempty ts
freeVariables (Case t cs _) =
     freeVariables t
  <> foldr (\(p, t') fvs -> fvs <> freeVariables p <> freeVariables t') mempty cs
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
freeVariables (Equal t0 t1 _) = freeVariables t0 <> freeVariables t1
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
withoutDeclarations (Declaration _ _  p) =                     withoutDeclarations p
withoutDeclarations (Data        d ts p) = Data         d ts $ withoutDeclarations p
withoutDeclarations (Definition  x t  p) = Definition   x  t $ withoutDeclarations p
withoutDeclarations (Property  n xs t p) = Property   n xs t $ withoutDeclarations p
withoutDeclarations EndOfProgram         = EndOfProgram

declarationsUpFront :: Program a -> Program a
declarationsUpFront p =
  foldr (uncurry Declaration) (withoutDeclarations p) (declarations p)
