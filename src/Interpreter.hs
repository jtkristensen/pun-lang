
module Interpreter where

import Syntax
import Control.Monad.Reader

type Runtime a = Reader (Program a)

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
     interpret t0 >>= interpret . subst x t1
interpret (Rec x t0 a) =
  do notAtTopLevel (x, a)
     interpret (subst x t0 (Rec x t0 a))
interpret _ = error "expected a non-canonical term!"

-- utility -- (todo : better error messages).

bool :: (Term a) -> Runtime a Bool
bool (Boolean b _) = return b
bool _             = error "expected a boolean value"

number :: (Term a) -> Runtime a Integer
number (Number n _) = return n
number _            = error "expected an integer"

pair :: (Term a) -> Runtime a (Term a, Term a)
pair (Pair t1 t2 _) = return (t1, t2)
pair _              = error "expected a pair"

function :: (Term a) -> Runtime a (Term a -> Term a)
function (Lambda x t a) =
  do notAtTopLevel (x, a)
     return $ subst x t
function _ = error "expected a function"

subst :: X -> Term a -> (Term a -> Term a)
subst x t v = -- computes t[v/x].
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
    f = flip (subst x) v

-- Todo : can be avoided by renaming toplevel stuff, or by extending the
-- runtime monad to deal with variable bindings.
notAtTopLevel :: (X, a) -> Runtime a ()
notAtTopLevel (x, _) =
  do program <- ask
     if x `elem` (fst <$> definitions program)
       then error $ "the name " ++ x ++ "shadows the top level declaration of " ++ x
       else return ()
