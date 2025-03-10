{-# LANGUAGE TypeOperators #-}

module TypeInference where

import Syntax
import Environment
import Data.Functor ((<&>))
import Control.Arrow (second)
import Control.Monad (zipWithM_)

data Constraint
  = Type :=: Type
  deriving Show

-- Abbreviations.
type Mapping a b  = a -> b
type Mapsto  a b  = Mapping a b -> Mapping a b
type Bindings     = Mapping Name Type
type Annotation a = ERWS a Bindings [Constraint] Index
type Substitution = [(Index, Type)]

hole :: Annotation a Type
hole = Variable' <$> (get >>= \i -> put (i + 1) >> return i)

bind :: Eq x => x -> a -> x `Mapsto` a
bind x a look y = if x == y then a else look y

hasSameTypeAs :: Term Type -> Term Type -> Annotation a ()
t0 `hasSameTypeAs` t1 = tell [annotation t0 :=: annotation t1]

hasType :: Term Type -> Type -> Annotation a ()
t0 `hasType` tau = tell [annotation t0 :=: tau]

haveTypes :: [Term Type] -> [Type] -> Annotation a ()
haveTypes = zipWithM_ hasType

annotate :: Term a -> Annotation a (Term Type)
annotate (Number   n _)  = return $ Number n Integer'
annotate (Boolean  b _)  = return $ Boolean b Boolean'
annotate (Variable x _)  =
  do env <- ask
     return $ Variable x $ env x
annotate (Constructor c ts _) =
  do vs   <- mapM annotate ts
     env  <- environment
     adt  <- datatype env c
     taus <- fieldTypes env c
     vs `haveTypes` taus
     return $ Constructor c vs (Algebraic adt)
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
annotate (Equal t0 t1 _) =
  do t0' <- annotate t0
     t1' <- annotate t1
     t0' `hasSameTypeAs` t1'
     return $ Equal t0' t1' Boolean'
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
annotate (Case t cs _) =
  do tau  <- hole
     t'   <- annotate t
     t' `hasType` tau
     cs'  <- mapM annotatePattern cs
     tau' <- hole
     mapM_ ((`hasType` tau) . fst) cs'
     mapM_ ((`hasType` tau') . snd) cs'
     return $ Case t' cs' tau'
  where
    liftFV :: [(X, Type)] -> (Bindings -> Bindings)
    liftFV [] f = f
    liftFV ((x, t'') : rest) f = bind x t'' $ liftFV rest f
    annotatePattern (p, n) = do
      fvs <- mapM (\x -> (,) x <$> hole) $ freeVariables p
      p'  <- local (liftFV fvs) $ annotate p
      n'  <- local (liftFV fvs) $ annotate n
      return (p', n')

solve :: [Constraint] -> Maybe Substitution
solve [                 ] = return mempty
solve (constraint : rest) =
  case constraint of
    Integer'      :=: Integer'      -> solve rest
    Boolean'      :=: Boolean'      -> solve rest
    (Algebraic d) :=: (Algebraic c) | c == d -> solve rest
    (t0 :->: t1)  :=: (t2 :->: t3)  -> solve $ (t0 :=: t2) : (t1 :=: t3) : rest
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
    _                               -> error $ show constraint
class HasSubstitution thing where
  substitute :: Type -> Index -> (thing -> thing)

instance HasSubstitution Type where
  substitute t i (Variable' j) | i == j = t
  substitute t i (t0  :->: t1)          = substitute t i t0 :->: substitute t i t1
  substitute _ _ t0                     = t0

instance HasSubstitution Constraint where
  substitute t i (t0 :=: t1) = substitute t i t0 :=: substitute t i t1

fillTypeHoles :: Substitution -> Substitution
fillTypeHoles ((i, Variable' j) : rest) = (i, Integer') : (j, Integer') : fillTypeHoles rest
fillTypeHoles (realizedType     : rest) = realizedType  : fillTypeHoles rest
fillTypeHoles [                       ] = [ ]

-- Utility functions.
indexes :: Type -> [Index]
indexes (Variable' i) = return i
indexes (t0 :->: t1)  = indexes t0 ++ indexes t1
indexes _             = mempty

emptyBindings :: Bindings
emptyBindings = error . (++ " is unbound!")

infer :: Term a -> Index -> (Term Type, Index, [Constraint])
infer term = runERWS (annotate term) EndOfProgram emptyBindings

-- alpha renaming.
alpha :: Index -> (Type -> (Index, Type))
alpha i t = (if null (indices t) then i else i + maximum (indices t) + 1, increment t)
  where increment Integer'        = Integer'
        increment Boolean'        = Boolean'
        increment (Algebraic d)   = Algebraic d
        increment (Variable' j)   = Variable' (i + j)
        increment (t1 :->: t2 )   = increment t1 :->: increment t2

alphaADT :: Index -> [TypeConstructor] -> (Index, [TypeConstructor])
alphaADT i = foldr (\c (j, k) -> second (: k) (alphaDef j c)) (i, [])

alphaDef :: Index -> TypeConstructor -> (Index, TypeConstructor)
alphaDef i (TypeConstructor c cs) = second (TypeConstructor c)
                                    (foldr (\t (j, ts) ->
                                              second (: ts) (alpha j t)) (i, []) cs)

-- TODO: Better error handling {^o^}!
bindings :: [Constraint] -> Substitution
bindings constraints = case solve constraints of
  Just cs -> fillTypeHoles cs
  Nothing -> error "type error"

refine :: Substitution -> (Type -> Type)
refine s o = refine' s o
  where
    refine' [          ] t                      = t
    refine' ((i, t) : _) (Variable' j) | i == j = refine' s t
    refine' (_   : rest) (Variable' j)          = refine' rest (Variable' j)
    refine' _            Integer'               = Integer'
    refine' _            Boolean'               = Boolean'
    refine' _            (Algebraic d)          = Algebraic d
    refine' s'           (t0 :->: t1)           = refine' s' t0 :->: refine' s' t1

type GlobalEnv = X -> Maybe Type

inferP :: (Show a) => Program a -> Program Type
inferP program = refine (bindings $ cs ++ cs') <$> pt
  where
    (pt, _, cs) = runERWS program' program emptyBindings 0
    cs'         = [ t' :=: annotation t''
                  | (x, t' ) <- declarations pt
                  , (y, t'') <- definitions  pt
                  , x == y
                  ]
    program' = inferP' program
    inferP' (Data d taus p) =
      do i <- get
         let (j, taus') = alphaADT i taus
         put j
         rest' <- local (bind d (Algebraic d)) $ inferP' p
         return $ Data d taus' rest'
    inferP' (Declaration x t p) =
      do i <- get
         let (j, tau) = alpha i t
         put j
         -- This forces that all recursive functions must have type
         -- declarations. It also forces the ML style monomorphism
         -- constraint on recursive things at top-level. This may be be more
         -- restrictive than what we want, but on the other hand, it was
         -- easy to implement {^_^}.
         p' <- local (bind x tau) $ inferP' p
         return $ Declaration x tau p'
    inferP' (Definition x t p) =
      do t' <- annotate t
         p' <- inferP' p
         return $ Definition x t' p'
    inferP' (Property q params t p) =
      do params' <- mapM (\(x, _) -> hole <&> (,) x) params
         t'      <- local (update params') $ annotate t
         t' `hasType` Boolean'
         p'      <- inferP' p
         return $ Property q params' t' p'
      where
        update ps f x =
          case lookup x ps of
            Just tau -> tau
            Nothing  -> f x
    inferP' EndOfProgram = return EndOfProgram

inferT :: Show a => Term a -> Term Type
inferT t =
  let (t', _, _) = runERWS (annotate t) EndOfProgram emptyBindings 0
  in  t'

-- Just here for documentation
usage :: Term a -> Index -> (Term Type, Index)
usage t i = (fmap (refine (bindings cs)) t', j)
  where (t', j, cs) = infer t i
