module Shrink where

import Syntax
import Interpreter           (normalize, substitute)
import Control.Monad         (liftM, ap)
-- import Control.Monad.Trans
import Test.Tasty.QuickCheck (shrinkIntegral, Gen, oneof)

term :: (Term a, [(X, Term a)]) -> Term a
term = uncurry $ foldr (\(x, v) t -> substitute x t v)

type Body        = Term Type
type Part        = (X, Term Type)
type Parts       = [Part]
type PropConfig  = Program Type -> Body -> Parts

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

{-
instance MonadTrans Shrink where
  lift = Shrink . (liftM Gen)
-}

-- ---------------------------- Operations of the Shrink monad ----------------------------
-- Updates one part
updatePart :: Part -> (Shrink a -> Shrink a)
updatePart part' shrinkM = Shrink $ \program body parts -> (runShrink shrinkM) program body (swap part' parts)

-- Updates all parts
updateParts :: Parts -> (Shrink a -> Shrink a)
updateParts parts' shrinkM = Shrink $ \program body _ -> (runShrink shrinkM) program body parts'

getParts :: Shrink Parts
getParts = Shrink $ \_ _ parts -> return (parts, parts)

-- How to round-robin?
shrinkAll []                   = undefined
shrinkAll ((name, term'):rest) = shrink' 

{-
shrink :: Show a => Program a -> Term a -> [(X, Term a)] -> (X, Term a) -> (X, Term a)
shrink program body parts part@(name, term') = 
  case (normalize program term') of
    (Number int       type' ) -> (shrinkNum program body parts (name, (Number int type')))
    -- (Node   l k v r   type'') -> 
    _                         -> part
-}

eval' :: Parts -> Shrink (Term Type)
eval' thing = Shrink $ \program body parts -> return (normalize program (term (body, thing)), parts)

shrink' :: Part -> Gen (a, Parts)
shrink' _  = undefined
{-
shrink' part@(name, term') = do
  evalTerm <- eval' term'
  case (evalTerm) of
    (Number int type') -> return (shrinkNum (name, (Number int type')))
-}

shrinkNum :: Part -> Gen Part
shrinkNum part@(name, (Number int type')) = do
  case shrinkIntegral int of
    []              -> return part
    shrinkedNumbers -> do
      -- let shrinkFalse = return <$> ([num | num <- shrinkedNumbers, evalsToFalse num type'])
      -- still need to check if it makes the whole thing evaluate to false
      smaller <- oneof (return <$> shrinkedNumbers)
      return (name, Number smaller type')
    where
      evalsToFalse i t' = do
        parts <- getParts
        let thing = swap (name, Number i t') parts
        check <- eval' thing
        case check of
          (Boolean False _) -> return True
          _                 -> return False

-- ---------------------------- Helper functions ----------------------------
-- TODO: better variable names
swap :: Part -> Parts -> Parts
swap t@(name, _) (p:params) =
  if (fst p == name)
  then t:(swap t params)
  else p:(swap t params)
swap _ [] = []

{-
closestToZero :: Integer -> [Integer] -> Integer
closestToZero current []          = current
closestToZero current (n:numbers) =
  if   (0 - (abs n)) > (0 - (abs current))
  then (closestToZero n       numbers)
  else (closestToZero current numbers)
-}