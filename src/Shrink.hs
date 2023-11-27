module Shrink where

import Syntax
import Interpreter           (normalize, substitute)
import Control.Monad         (liftM, ap)
import Test.Tasty.QuickCheck (shrinkIntegral, Gen, oneof)

type Body         = Term Type
type Part         = (X, Term Type)
type ShrinkedPart = (X, Term Type)
type Parts        = [Part]
type PropConfig   = Program Type -> Body -> Parts

newtype Shrink a = Shrink { runShrink :: Program Type -> Term Type -> Parts -> Gen (a, Parts) }

instance Monad Shrink where
  return = pure
  ma >>= f = Shrink $ \program body parts -> do
    (a, parts') <- runShrink ma program body parts
    runShrink (f a) program body parts'

instance Functor Shrink where
  fmap = liftM

instance Applicative Shrink where
  pure a = Shrink $ \_ _ parts -> return (a, parts);
  (<*>)  = ap

-- ---------------------------- Helper functions ----------------------------
swap :: Part -> Parts -> Parts
swap newPart@(name, _) (part:parts) =
  if   (fst part == name)
  then newPart:parts
  else    part:(swap newPart parts)
swap _ [] = []

replace :: Term a -> Integer -> Term a
replace (Number _ type') int' = Number int' type'
replace t                _    = t

term :: (Term a, [(X, Term a)]) -> Term a
term = uncurry $ foldr (\(x, v) t -> substitute x t v)

oneof' :: [Shrink a] -> Shrink a
oneof' shrinks = Shrink $ \program body parts -> do
  gens <- mapM (\s -> runShrink s program body parts) shrinks
  oneof (return <$> gens)

eval' :: Parts -> Shrink (Term Type)
eval' parts = Shrink $ \program body parts' -> return (normalize program (term (body, parts)), parts')

evalsToFalse :: Part -> Term Type -> Shrink Bool
evalsToFalse (name, _) newTerm = do 
  parts <- getParts
  let parts' = swap (name, newTerm) parts
  check <- eval' parts'
  case check of
    (Boolean False _) -> return True
    _                 -> return False

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys) = 
  if   x == y
  then remove x ys
  else y:(remove x ys)

closestToZero :: Integer -> [Integer] -> Integer
closestToZero current []          = current
closestToZero current (n:numbers) =
  if   (0 - (abs n)) > (0 - (abs current))
  then (closestToZero n       numbers)
  else (closestToZero current numbers)

smallest :: [Integer] -> Part -> Shrink (Maybe Integer)
smallest [] _ = return Nothing
smallest shrinked part@(_, Number int _) = do
  let smaller = closestToZero int shrinked
  smallerEvalsToFalse <- evalsToFalse part (Number smaller Integer')
  case smallerEvalsToFalse of
    True  -> return $ Just smaller
    False -> do 
      let removedSmaller = remove smaller shrinked
      smallest removedSmaller part
smallest _ _ = return Nothing

-- Returns the term with the function applied to the subterms
applyToSubterms :: (Term Type -> Term Type) -> Term Type -> Term Type
applyToSubterms f (Node          l k v r      t) = Node          (f l ) (f k ) (f v) (f r) t
applyToSubterms f (Case          t0 l (p, t') t) = Case          (f t0) (f l ) (p, f t')   t
applyToSubterms f (If            t0 t1 t2     t) = If            (f t0) (f t1) (f t2)      t
applyToSubterms f (Plus          t0 t1        t) = Plus          (f t0) (f t1)             t
applyToSubterms f (Leq           t0 t1        t) = Leq           (f t0) (f t1)             t
applyToSubterms f (Pair          t0 t1        t) = Pair          (f t0) (f t1)             t
applyToSubterms f (Lambda      n t0           t) = Lambda      n (f t0)                    t
applyToSubterms f (Rec         n t0           t) = Rec         n (f t0)                    t
applyToSubterms f (Application   t0 t1        t) = Application   (f t0) (f t1)             t
applyToSubterms f (Fst (Pair     t0 t1 t')    t) = Fst (Pair     (f t0)    t1  t')         t
applyToSubterms f (Snd (Pair     t0 t1 t')    t) = Snd (Pair        t0  (f t1) t')         t
applyToSubterms _ t                              = t

-- Applies function to subterms
applyToSubterms' :: (Term Type -> a) -> Term Type -> [a]
applyToSubterms' f (Node          l k v r      _) = fmap f [l, k, v, r]
applyToSubterms' f (Case          t0 l (_, t') _) = fmap f [t0, l, t']
applyToSubterms' f (If            t0 t1 t2     _) = fmap f [t0, t1, t2]
applyToSubterms' f (Plus          t0 t1        _) = fmap f [t0, t1]
applyToSubterms' f (Leq           t0 t1        _) = fmap f [t0, t1]
applyToSubterms' f (Pair          t0 t1        _) = fmap f [t0, t1]
applyToSubterms' f (Application   t0 t1        _) = fmap f [t0, t1]
applyToSubterms' f (Lambda      _ t0           _) = [f t0]
applyToSubterms' f (Rec         _ t0           _) = [f t0]
applyToSubterms' f (Fst (Pair     t0 _ _)      _) = [f t0]
applyToSubterms' f (Snd (Pair     _ t1 _)      _) = [f t1]
applyToSubterms' f t                              = [f t]

-- ---------------------------- Operations of the Shrink monad ----------------------------
-- Updates one part
updatePart :: Part -> (Shrink a -> Shrink a)
updatePart part shrinkM = Shrink $ \program body parts -> (runShrink shrinkM) program body (swap part parts)

getProgram :: Shrink (Program Type)
getProgram = Shrink $ \program _ parts -> return (program, parts)

getParts :: Shrink Parts
getParts = Shrink $ \_ _ parts -> return (parts, parts)

-- ---------------------------- Shrinking ----------------------------
shrinkAll :: Parts -> Shrink Parts
shrinkAll []           = getParts
shrinkAll ((name, term'):parts) = do
  program <- getProgram
  let evaluatedTerm = normalize program term'
  maybeShrinked <- shrink (name, evaluatedTerm)
  case maybeShrinked of
    Just    shrinkedPart -> updatePart shrinkedPart  (shrinkAll parts)
    Nothing              -> updatePart (name, evaluatedTerm) (shrinkAll parts)

shrink :: Part -> Shrink (Maybe ShrinkedPart)
shrink part@(name, term'@(Number int _)) =
  case shrinkIntegral int of
    []       -> return Nothing
    shrinked -> do
      smaller <- smallest shrinked part
      case smaller of
        Just small -> return $ Just (name, (replace term' small))
        Nothing    -> return Nothing
shrink (name, term'@(Lambda _ _ _)) = do
  let shrinkedFun = removeBloat term'
  return $ Just (name, shrinkedFun)
shrink part@(name, term'@(Node     _ _ _ _ _)) = do
  shrinkedNode <- shrinkNode name term' part
  return $ Just (name, shrinkedNode)
shrink (name, (Pair left right type')) = do
  let shrinkedLeft  = removeBloat left
  let shrinkedRight = removeBloat right
  return $ Just (name, (Pair shrinkedLeft shrinkedRight type'))
shrink _ = return Nothing

shrinkNode :: Name -> Term Type -> Part -> Shrink (Term Type)
shrinkNode name node@(Node left _ _ right _) part = do
  evalLeft  <- evalsToFalse part left
  evalRight <- evalsToFalse part right
  case evalLeft of
    True  -> shrinkNode name left part
    False -> do
      case evalRight of
        True  -> shrinkNode name right part
        False -> return node
shrinkNode _ term' _ = return term' 

removeBloat :: Term Type -> Term Type
removeBloat (Let     n  t0 t1     t) = 
  case findFunOccurrence n t1 of
    True  -> Let n (removeBloat t0) (removeBloat t1) t
    False -> removeBloat t1                             -- removing outer let
removeBloat (Fst  (Pair t0 _ _)   _) = removeBloat t0   -- removing outer fst
removeBloat (Snd  (Pair _ t1 _)   _) = removeBloat t1   -- removing outer snd
removeBloat term'                    = applyToSubterms removeBloat term'

findFunOccurrence :: Name -> Term Type -> Bool
findFunOccurrence name (Variable    n          _) = name == n
findFunOccurrence _    (Number      _          _) = False
findFunOccurrence _    (Boolean     _          _) = False
findFunOccurrence _    (Unit                   _) = False
findFunOccurrence _    (Leaf                   _) = False
findFunOccurrence name term'                      = any (== True) (applyToSubterms' (findFunOccurrence name) term')