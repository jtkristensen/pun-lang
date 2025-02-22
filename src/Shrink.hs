module Shrink where

import Syntax
import Interpreter           (normalize, substitute)
import Control.Monad         (liftM, ap)
import Test.Tasty.QuickCheck (shrinkIntegral, Gen, oneof)

type ParameterName      = String
-- TODO: Or PropertyTerm?
type PropertyBody       = Term Type
type GeneratedArgument  = (ParameterName, Term Type)
type ShrinkedArgument   = (ParameterName, Term Type)
type ShrinkedPart       = (ParameterName, Term Type)
type ShrinkedArguments  = [ShrinkedArgument]
type GeneratedArguments = [GeneratedArgument]

-- ---------------------------------- Monad ----------------------------------

newtype Shrink a = Shrink { runShrink :: Program Type -> PropertyBody -> GeneratedArguments -> Gen (a, GeneratedArguments) }

instance Monad Shrink where
  return = pure
  ma >>= f = Shrink $ \program propertyBody generatedArguments -> do
    (a, generatedArguments') <- runShrink ma program propertyBody generatedArguments
    runShrink (f a) program propertyBody generatedArguments'

instance Functor Shrink where
  fmap = liftM

instance Applicative Shrink where
  pure a = Shrink $ \_ _ generatedArguments -> return (a, generatedArguments);
  (<*>)  = ap

-- ------------------------- Shrink monad operations -------------------------

-- TODO: Other names than ones that start with `get`
getProgram :: Shrink (Program Type)
getProgram = Shrink $ \program _ generatedArguments -> return (program, generatedArguments)

getShrinkedArgs :: Shrink ShrinkedArguments
getShrinkedArgs = Shrink $ \_ _ generatedArgs -> return (generatedArgs, generatedArgs)

updateArguments :: ShrinkedArgument -> (Shrink a -> Shrink a)
updateArguments shrinkedArgument shrinkM = Shrink $ \program propertyBody generatedArguments ->
    (runShrink shrinkM) program propertyBody (replace shrinkedArgument generatedArguments)

-- ---------------------------- Helper functions ----------------------------

applyToSubterms :: (Term Type -> Term Type) -> Term Type -> Term Type
applyToSubterms f (Node          l k v r      t) = Node          (f l ) (f k ) (f v) (f r) t
-- TODO: Case, ADTs, Equal etc. 
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

applyToSubterms' :: (Term Type -> a) -> Term Type -> [a]
-- TODO: Case, ADTs, Equal, etc.
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

replace :: ShrinkedArgument -> GeneratedArguments -> ShrinkedArguments
replace _ [] = []
replace shrinkedArgument@(name, _) (generatedArgument@(name', _):args) =
    if   name == name'
    then shrinkedArgument:args
    else generatedArgument:args

closestToZero :: Integer -> [Integer] -> Integer
closestToZero current []          = current
closestToZero current (n:numbers) =
  if   (0 - (abs n)) > (0 - (abs current))
  then (closestToZero n       numbers)
  else (closestToZero current numbers)

-- smallest :: [Integer] -> GeneratedArgument -> Shrink (Maybe Integer)
-- smallest [] _ = return Nothing
-- smallest shrinks generatedArugment@(_, Number int) = do
--     let smaller = closestToZero int shrinks

propertyIsFalse :: ShrinkedArgument -> Shrink Bool
propertyIsFalse shrinkedArg = undefined

appearsIn :: Name -> Term Type -> Bool
-- TODO: Case, ADT, Equals etc
appearsIn name (Variable n _) = name == n
appearsIn _ (Number _ _) = False
appearsIn _    (Boolean     _          _) = False
appearsIn _    (Unit                   _) = False
appearsIn name term'                      = any (== True) (applyToSubterms' (appearsIn name) term')

removeOuterTerms :: Term Type -> Term Type
removeOuterTerms (Lambda varName t0 t) =
    if   varName `appearsIn` t0
    then Lambda varName (removeOuterTerms t0) t
    else removeOuterTerms t0
removeOuterTerms (Let varName t0 t1 t) =
    if   varName `appearsIn` t1
    then Let varName (removeOuterTerms t0) (removeOuterTerms t1) t
    else removeOuterTerms t1
removeOuterTerms (Fst (Pair t0 _ _) _) = removeOuterTerms t0
removeOuterTerms (Snd (Pair _ t1 _) _) = removeOuterTerms t1
removeOuterTerms term'                 = applyToSubterms removeOuterTerms term'

-- -------------------------------- Shrinking --------------------------------

shrink' :: GeneratedArgument -> Shrink ShrinkedArgument
-- TODO: Case, ADTs, Equal etc. 
-- For shrinking constructors one could either remove something
-- if it contains something of itself or map shrink on the types
-- that it contains
shrink' (name, argument@(Constructor _ terms _)) = undefined
shrink' generatedArgument@(name, argument@(Number int _)) = undefined
--     case shrinkIntegral int of
--         []      -> generatedArgument
--         shrinks -> do
shrink' (name, argument@(Pair _ _ _)) = do
    shrinkedArgument <- return $ applyToSubterms shrink' argument
    -- TODO: Check falseProperty
    return $ shrinkedArgument
shrink' (name, argument@(Equal _ _ _)) = do
    shrinkedArgument <- return $ applyToSubterms shrink' argument
    -- TODO: Check falseProperty
    return $ shrinkedArgument
shrink' (name, argument) = do
    let shrinkedArgument = removeOuterTerms argument
    falseProperty <- propertyIsFalse (name, shrinkedArgument)
    if   falseProperty
    then return (name, shrinkedArgument)
    else return (name, argument)

shrink :: GeneratedArguments -> Shrink ShrinkedArguments
shrink []                                     = getShrinkedArgs
shrink (generatedArgument:generatedArguments) = do
    shrinkedArgument <- shrink' generatedArgument
    updateArguments shrinkedArgument (shrink generatedArguments)