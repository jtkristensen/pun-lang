
module Interpreter (normalize, substitute) where

import Syntax
-- import TypeInference hiding (substitute)
import Control.Monad.Reader
import Control.Monad (when)

type Runtime a = Reader (Program a)
type Unifier a = [(Name, Pattern a)]

normalize :: Program a -> (Term a -> Term a)
normalize p t = runReader (interpret t) p

interpret :: Term a -> Runtime a (Term a)
interpret  t | canonical t = return t
interpret (Variable x _) =
  do program <- ask
     case map snd $ filter ((==x) . fst) (definitions program) of
       [ ] -> error $ "unbound variable " ++ x
       [t] -> interpret t -- (disallow shadowing at toplevel).
       _   -> error $ "ambiguous bindings for " ++ x
interpret (If cond t1 t2 _) =
  do b <- interpret cond >>= bool
     interpret (if b then t1 else t2)
interpret (Plus t1 t2 a) =
  do m <- interpret t1 >>= number
     n <- interpret t2 >>= number
     return $ Number (m + n) a
interpret (Leq  t1 t2 a) =
  do m <- interpret t1 >>= number
     n <- interpret t2 >>= number
     return $ Boolean (m <= n) a
interpret (Pair t1 t2 a) =
  do v1 <- interpret t1
     v2 <- interpret t2
     return $ Pair v1 v2 a
interpret (Fst p _) =
  do ts <- interpret p >>= pair
     return $ fst ts
interpret (Snd p _) =
  do ts <- interpret p >>= pair
     return $ snd ts
interpret (Application t1 t2 _) =
  do f <- interpret t1 >>= function
     x <- interpret t2
     interpret (f x)
interpret (Let x t0 t1 a) =
  do notAtTopLevel (x, a)
     interpret t0 >>= interpret . substitute x t1
interpret (Rec x t0 a) =
  do notAtTopLevel (x, a)
     interpret (substitute x t0 (Rec x t0 a))
-- TODO: Should Leaf be canonical?
interpret (Leaf a) = return $ Leaf a
interpret (Node l t0 r a) = undefined
interpret (Case t0 l (p, t) a) = undefined
interpret _ = error "expected a non-canonical term!"

-- utility -- (todo : better error messages).

bool :: Term a -> Runtime a Bool
bool (Boolean b _) = return b
bool _             = error "expected a boolean value"

number :: Term a -> Runtime a Integer
number (Number n _) = return n
number _            = error "expected an integer"

pair :: Term a -> Runtime a (Term a, Term a)
pair (Pair t1 t2 _) = return (t1, t2)
pair _              = error "expected a pair"

function :: Term a -> Runtime a (Term a -> Term a)
function (Lambda x t a) =
  do notAtTopLevel (x, a)
     return $ substitute x t
function _ = error "expected a function"

substitute :: X -> Term a -> (Term a -> Term a)
substitute x t v = -- computes t[v/x].
  case t of
    (Variable y  _) | x == y -> v
    (If t1 t2 t3 a)          -> If   (f t1) (f t2) (f t3)                    a
    (Plus t1 t2  a)          -> Plus (f t1) (f t2)                           a
    (Leq  t1 t2  a)          -> Leq  (f t1) (f t2)                           a
    (Pair t1 t2  a)          -> Pair (f t1) (f t2)                           a
    (Fst  t1     a)          -> Fst  (f t1)                                  a
    (Snd  t1     a)          -> Snd  (f t1)                                  a
    (Lambda y t1 a) | x /= y -> Lambda y (f t1)                              a
    (Application t1 t2 a)    -> Application (f t1) (f t2)                    a
    (Let y t1 t2 a)          -> Let y (f t1) ((if x == y then id else f) t2) a
    (Rec y t1    a) | x /= y -> Rec y (f t1)                                 a
    _                        -> t
  where
    f = flip (substitute x) v

-- Todo : can be avoided by renaming toplevel stuff, or by extending the
-- runtime monad to deal with variable bindings.
notAtTopLevel :: (X, a) -> Runtime a ()
notAtTopLevel (x, _) =
  do program <- ask
     when (x `elem` (fst <$> definitions program)) $
       error $ "the name " ++ x ++ "shadows the top level declaration of " ++ x
       
-- Term unification (for case-statements)
unify :: Term a -> Term a -> Maybe (Unifier a)
unify (Number v _) (Number v' _)
  | v == v' = return []
unify p@(Plus  t0 t1 t) (Number v _) =
  do let (Number v' _) = simpleEval p
     if v == v' then return [] else Nothing
unify (Boolean v _) (Boolean v' _)
  | v == v' = return $ []
unify p@(Leq t0 t1 a) (Boolean  v _) =
  do let (Boolean v' _) = simpleEval p
     if v == v' then return [] else Nothing
unify (Variable x t) p
  | not (p `contains` x) = return $ p `substitutes` x
-- unify (Leaf        _) (Leaf        _) = return $ []
-- unify (Variable n1 _) (Variable n2 _) | n1 == n2 = return $ []
-- unify (Node l1 t0 r1 _) (Node l2 t0' r2 _) =
--   do unify l1  l2
--      unify t0  t0'
--      unify r1  r2
-- unify (If t0 t1 t2) (If )
unify _ _ = Nothing

contains :: Pattern a -> X -> Bool
contains (Variable y _) x = x == y
contains (Pair t0 t1 _) x = t0 `contains` x || t1 `contains` x
contains _              _ = False

substitutes :: Pattern a -> X -> Unifier a
substitutes p x = return $ (x, p)

simpleEval :: Term a -> Term a
simpleEval (Plus t0 t1 a) =
  case (simpleEval t0, simpleEval t1) of
    ((Number v _), (Number v' _)) -> Number (v + v') a
    _ -> error "expected two integers"
simpleEval (Leq t0 t1 a) =
  case (simpleEval t0, simpleEval t1) of
    ((Number v _), (Number v' _)) -> Boolean (v <= v') a
    _ -> error "expected two integers"
simpleEval t = t
>>>>>>> 1a8c110 (Unification attempt for integers, booleans, and variables)
