module Shrink where

import Syntax
import Interpreter           (normalize, substitute)
import Control.Monad         (liftM, ap)
import Test.Tasty.QuickCheck (generate, shrinkIntegral, Gen)

-- TODO: find out what (Term a, [(X, Term a)]) is
-- what's the alias eller noe? 
term :: (Term a, [(X, Term a)]) -> Term a
term = uncurry $ foldr (\(x, v) t -> substitute x t v)

type Body        = Term Type
type Part        = (X, Term Type)
type Parts       = [Part]
type PropConfig  = Program Type -> Body -> Parts

{-
Gen a instead of IO and then I just have to make the output of type IO,
then I can use generate - look at the code for that above

Must be in the gen monad for me to use oneof and other things

-}
-- newtype Shrink a = Shrink { runShrink :: Program Type -> Term Type -> Parts -> IO (a, Parts) } 
newtype Shrink a = Shrink { runShrink :: Program Type -> Term Type -> Parts -> Gen (a, Parts) }

-- can generalise later with m?

-- get for getting parts
-- update for updating parts

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

-- shrink' :: [(X, Term Type)] -> Shrink IO (Term Type)
-- shrink' :: [(X, Term Type)] -> Shrink IO a
-- shrink' [] = return ()
shrink' _  = undefined

-- shrink' part@(name, term) = 
--   case (normalize program term') of
--     (Number int type') -> shrinkNum

-- TODO: better variable names
swap :: (X, Term a) -> [(X, Term a)] -> [(X, Term a)]
swap t@(name, _) (p:params) =
  if (fst p == name)
  then t:(swap t params)
  else p:(swap t params)
swap _ [] = []

closestToZero :: Integer -> [Integer] -> Integer
closestToZero current []          = current
closestToZero current (n:numbers) =
  if   (0 - (abs n)) > (0 - (abs current))
  then (closestToZero n       numbers)
  else (closestToZero current numbers)

{-
eval' :: [(X, Term Type)] -> Shrink (Term Type)
eval' thing = Shrink $ \program body _ -> return (normalize program (term (body, thing)))
-}

{-
-- TODO: refactor, it's a bit unreadable
shrinkNum :: Show a => Program a -> Term a -> [(X, Term a)] -> (X, Term a) -> (X, Term a)
shrinkNum program body parts part@(name, (Number int type')) = do
  -- shrinkIntegral from QuickCheck
  case shrinkIntegral int of
    []              -> part
    -- get different shrinks of number
    shrinkedNumbers -> do
      -- filter on the numbers that evaluate the term to false (probably all in the case of add?)
      let shrinkFalse = [num | num <- shrinkedNumbers, evalsToFalse program num type' ]
      -- find number closest to zero (smallest shrink)
      let smallest = closestToZero (shrinkedNumbers !! 0) shrinkFalse
      (name, Number smallest type')
  where
    evalsToFalse p i t' =
      let thing = swap (name, Number i t') parts in
      case eval' thing of
        (Boolean False _) -> True
        _                 -> False
shrinkNum _ _ _ part = part

shrink part@(name, term') = 
  ShrinkMonad $ \program body parts -> 
    case (normalize program term') of
      (Number int       type' ) -> return (shrinkNum program body parts (name, (Number int type')))
      _ -> return body
-}
-- TODO: are there any parameters I can get rid of?
{-
shrink :: Show a => Program a -> Term a -> [(X, Term a)] -> (X, Term a) -> (X, Term a)
shrink program body parts part@(name, term') = 
  case (normalize program term') of
    (Number int       type' ) -> (shrinkNum program body parts (name, (Number int type')))
    -- (Node   l k v r   type'') -> 
    _                         -> part
-}