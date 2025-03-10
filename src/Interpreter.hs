
module Interpreter (normalize, substitute) where

import Syntax
import Unification
import Control.Monad.Reader

type Runtime a = Reader (Program a)

normalize :: Show a => Program a -> (Term a -> Term a)
normalize p t = runReader (interpret t) p

interpret :: Show a => Term a -> Runtime a (Term a)
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
interpret (Equal t0 t1 a) =
  do x <- interpret t0 >>= nonFunction
     y <- interpret t1 >>= nonFunction
     return $ Boolean (void x == void y) a
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
interpret (Constructor c ts a) =
  do ts' <- mapM interpret ts
     return $ Constructor c ts' a
interpret (Case t cs _) = do
  v <- interpret t
  interpret $ substituteWithUnifier (unify v cs)
interpret _ = error "expected a non-canonical term!"

-- utility -- (todo : better error messages).

bool :: Term a -> Runtime a Bool
bool (Boolean b _) = return b
bool _             = error "expected a boolean value"

number :: Show a => Term a -> Runtime a Integer
number (Number n _) = return n
number t            = error $ "expected an integer, but got a " ++ show t

function :: Term a -> Runtime a (Term a -> Term a)
function (Lambda x t a) =
  do notAtTopLevel (x, a)
     return $ substitute x t
function _ = error "expected a function"

nonFunction :: Term a -> Runtime a (Term a)
nonFunction (Lambda {}) = error "expected a non-function"
nonFunction t           = return t

substitute :: X -> Term a -> (Term a -> Term a)
substitute x t v = -- computes t[v/x].
  case t of
    (Variable y  _) | x == y -> v
    (If t1 t2 t3 a)          -> If    (f t1) (f t2) (f t3)                    a
    (Plus  t1 t2 a)          -> Plus  (f t1) (f t2)                           a
    (Leq   t1 t2 a)          -> Leq   (f t1) (f t2)                           a
    (Equal t1 t2 a)          -> Equal (f t1) (f t2)                           a
    (Lambda y t1 a) | x /= y -> Lambda y (f t1)                               a
    (Application t1 t2 a)    -> Application (f t1) (f t2)                     a
    (Let y t1 t2 a)          -> Let y (f t1) ((if x == y then id else f) t2)  a
    (Rec y t1    a) | x /= y -> Rec y (f t1)                                  a
    (Constructor c ts a)     -> Constructor c (map f ts)                      a
    (Case t0 cs   a)         ->
      Case (f t0) (map (\(p, n) -> (p, (if x `elem` freeVariables p then id else f) n)) cs) a
    _                        -> t
  where
    f = flip (substitute x) v

substituteWithUnifier :: (Unifier a, Term a) -> Term a
substituteWithUnifier (xs, t) =
  foldr (\(x, v) t' -> substitute x t' v) t xs

-- Todo : can be avoided by renaming toplevel stuff, or by extending the
-- runtime monad to deal with variable bindings.
notAtTopLevel :: (X, a) -> Runtime a ()
notAtTopLevel (x, _) =
  do program <- ask
     when (x `elem` (fst <$> definitions program)) $
       error $ "the name " ++ x ++ "shadows the top level declaration of " ++ x
