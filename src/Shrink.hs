module Shrink where

import Syntax
import Interpreter           (normalize, substitute)
import Control.Monad         (liftM, ap)
-- import Control.Monad.Trans
import Test.Tasty.QuickCheck (shrinkIntegral, Gen, oneof)

type Body         = Term Type
type Part         = (X, Term Type)
type ShrinkedPart = (X, Term Type)
type Parts        = [Part]
type PropConfig   = Program Type -> Body -> Parts

-- TODO: Do I really need Gen when only 'oneof' is used once?
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

-- TODO: own lift
-- liftShrink :: (a -> b) -> (Shrink a -> Shrink b)

-- ---------------------------- Helper functions ----------------------------
swap :: Part -> Parts -> Parts
swap newPart@(name, _) (part:parts) =
  if   (fst part == name)
  then newPart:parts
  else    part:(swap newPart parts)
swap _ [] = []

replace :: Term a -> Integer -> Term a
replace (Number _ type') int' = (Number int' type')
replace _ _ = undefined

term :: (Term a, [(X, Term a)]) -> Term a
term = uncurry $ foldr (\(x, v) t -> substitute x t v)

oneof' :: [Shrink a] -> Shrink a
oneof' shrinks = Shrink $ \program body parts -> do
  gens <- mapM (\s -> runShrink s program body parts) shrinks
  oneof (return <$> gens)

f' :: [Shrink a] -> ([Gen (a, Parts)] -> Gen (a, Parts)) -> Shrink a
f' shrinks g = Shrink $ \program body parts -> do
  gens <- mapM (\s -> runShrink s program body parts) shrinks
  g (return <$> gens)

-- something that takes a Gen (a, Parts) and
-- returns a Gen (b, Parts)

-- function Gen (a, Parts) -> Gen (b, Parts)
-- turn to Shrink a -> Shrink b

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

-- TODO: Refactor
smallest :: [Integer] -> Part -> Shrink (Maybe Integer)
smallest [] _ = return Nothing
smallest shrinked part@(_, Number int _) = do
  let smaller = closestToZero int shrinked
  evalSmaller <- evalsToFalse part (Number smaller Integer')
  case evalSmaller of
    -- True meaning it does evaluate to False
    True  -> return $ Just smaller
    False -> smallest (remove smaller shrinked) part
smallest _ _ = return Nothing

-- ---------------------------- Operations of the Shrink monad ----------------------------
-- Updates one part
updatePart :: Part -> (Shrink a -> Shrink a)
updatePart part shrinkM = Shrink $ \program body parts -> (runShrink shrinkM) program body (swap part parts)

getProgram :: Shrink (Program Type)
getProgram = Shrink $ \program _ parts -> return (program, parts)

-- Updates all parts
updateParts :: Parts -> (Shrink a -> Shrink a)
updateParts parts' shrinkM = Shrink $ \program body _ -> (runShrink shrinkM) program body parts'

-- TODO: fix
getParts :: Shrink Parts
getParts = Shrink $ \_ _ parts -> return (parts, parts)

-- ---------------------------- Shrinking ----------------------------
-- TODO: replace a in return of monad Gen (a, Parts)?
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
        Just small -> return (Just (name, (replace term' small)))
        Nothing    -> return Nothing
shrink (_, (Variable _ _)) = return Nothing -- TODO: get value it corresponds to and shrink that?
shrink (name, term'@(Lambda _ _ _)) = do
  let shrinkedFun = shrinkFun term'
  return $ Just (name, shrinkedFun)
shrink part@(name, term'@(Node     _ _ _ _ _)) = do
  -- TODO: check if shrinked at all?
  shrinkedNode <- shrinkNode name term' part
  return $ Just (name, shrinkedNode)
-- TODO: shrink Pair
shrink _ = return Nothing

-- TODO: refactor/change name - shrinkTree instead?
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

shrinkFun :: Term Type -> Term Type
shrinkFun (Node l k v r         t) = (Node (shrinkFun l ) (shrinkFun k) 
                                           (shrinkFun v ) (shrinkFun r)             t)
shrinkFun (Case t0 l (p, t') t) = (Case (shrinkFun t0) (shrinkFun l) (p, shrinkFun t') t)
shrinkFun (If   t0 t1 t2     t) = (If   (shrinkFun t0) (shrinkFun t1) (shrinkFun t2) t)
shrinkFun (Plus t0 t1    t) = (Plus (shrinkFun t0) (shrinkFun t1) t)
shrinkFun (Leq  t0 t1    t) = (Leq  (shrinkFun t0) (shrinkFun t1) t)
shrinkFun (Pair t0 t1    t) = (Pair (shrinkFun t0) (shrinkFun t1) t)
shrinkFun (Fst  (Pair t0 _ _)       _) = shrinkFun t0
shrinkFun (Snd  (Pair _ t1 _)       _) = shrinkFun t1
shrinkFun (Application    t0 t1 t) = (Application (shrinkFun t0) (shrinkFun t1) t)
shrinkFun (Lambda  n  t0        t) = (Lambda n (shrinkFun t0) t)
shrinkFun (Rec     n  t0        t) = (Rec n (shrinkFun t0) t)
shrinkFun (Let     n  t0 t1     t) = 
  case findName n t1 of
    True  -> (Let n (shrinkFun t0) (shrinkFun t1) t)
    False -> (shrinkFun t1) -- removing the outer let
shrinkFun term' = term'     -- number, bool, unit, leaf and variable (?)

checkIfFound :: Name -> [Term Type] -> Bool
checkIfFound name terms = True `elem` fmap (findName name) terms

-- TODO: refactor
findName :: Name -> Term Type -> Bool
findName name (Node        l  k  v  r _) = checkIfFound name [l,  k, v , r]
findName name (Case        t0 l  n    _) = checkIfFound name [t0, l, snd n]
findName name (Variable    n          _) = (name == n)
findName name (If          t0 t1 t2   _) = checkIfFound name [t0, t1, t2  ]
findName name (Plus        t0 t1      _) = checkIfFound name [t0, t1      ]
findName name (Leq         t0 t1      _) = checkIfFound name [t0, t1      ]
findName name (Pair        t0 t1      _) = checkIfFound name [t0, t1      ]
findName name (Fst         t0         _) = findName     name  t0
findName name (Snd         t0         _) = findName     name  t0
findName name (Lambda      _  t0      _) = findName     name  t0
findName name (Application t0 t1      _) = checkIfFound name [t0, t1      ]
findName name (Let         _  t0 t1   _) = checkIfFound name [t0, t1      ]
findName name (Rec         _  t0      _) = findName     name  t0
findName _    _                          = False -- number, boolean, unit and leaf