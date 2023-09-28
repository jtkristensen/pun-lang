module Main (main) where

import Syntax
import Parser                (parsePunProgram, Info)
import TypeInference         (inferP)
import Interpreter           (normalize, substitute)
import GeneratorGenerator    (generateGenerator)
import Control.Monad         (void, liftM, ap)
import Control.Arrow         (second)
import Data.Functor          ((<&>))
import System.Exit           (die)
import System.Environment    (getArgs)
import System.IO             (hFlush, stdout)
import Test.Tasty.QuickCheck (generate, shrinkIntegral)

type ErrorMessage = String

data Action =
    Shell         (Program Type)
  | PropertyCheck (Program Type)
  | TypeCheck     (Program Info)
  | Fail  ErrorMessage

main :: IO ()
main = getArgs >>= run . action

run :: IO Action -> IO ()
run a = a >>= \what ->
  case what of
    (PropertyCheck program) -> check program
    (TypeCheck     program) -> typed program >>= print
    (Fail          message) -> die message
    (Shell         program) -> shell program

numberOfTests :: Integer
numberOfTests = 50

strengthen :: Monad m => (a, m b) -> m (a, b)
strengthen (a, mb) = mb <&> (,) a

-- TODO: find out what (Term a, [(X, Term a)]) is
-- what's the alias eller noe? 
term :: (Term a, [(X, Term a)]) -> Term a
term = uncurry $ foldr (\(x, v) t -> substitute x t v)

check :: Program Type -> IO ()
check program = void $ mapM check1 (properties program)
  where
    check1 (name, (args, body)) =
      do putStr $ "testing:" ++ name ++ "> "
         iter numberOfTests
      where
        config   = ([], [], declarations program)
        genParts = mapM strengthen $ second (generateGenerator config) <$> args
        -- term     = uncurry $ foldr (\(x, v) t -> substitute x t v)
        eval     = normalize program
        iter 0   = putStrLn " ok"
        iter n   =
          do parts <- generate genParts
             test  <- term <$> return (body, parts)
             case eval test of
               Boolean True _ -> putStr "." >> hFlush stdout >> iter (n - 1)
               _              ->
                 do putStrLn "x failed:"
                    putStr "shrinking> " >> hFlush stdout
                    counterexample <- smaller parts
                    print counterexample
                    putStrLn $ "after " ++ show (numberOfTests - n) ++ " tests."
        smaller parts =
            -- TODO: better shrink (and message)
          do putStr ""
             term <$> return (body, map (shrink program body parts) parts)

type Body a = Term a
type Part a = (X, Term a)
type Parts a = [Part a]
type Information a = Program a -> Body a -> Parts a

newtype ShrinkMonad a = ShrinkMonad { runShrink :: Information a -> Part a }

-- instance Monad ShrinkMonad where
--   -- Minimal value to return
--   return a     = ShrinkMonad $ \_ -> a
--   shrink >>= f = ShrinkMonad $ \_ ->

instance Functor ShrinkMonad where
  fmap = liftM

instance Applicative ShrinkMonad where
  pure = return;
  (<*>) = ap

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
      case normalize p (term (body, thing)) of
        (Boolean False _) -> True
        _                 -> False
shrinkNum _ _ _ part = part

-- TODO: are there any parameters I can get rid of?
shrink :: Show a => Program a -> Term a -> [(X, Term a)] -> (X, Term a) -> (X, Term a)
shrink program body parts part@(name, term') = 
  case (normalize program term') of
    (Number int       type' ) -> (shrinkNum program body parts (name, (Number int type')))
    -- (Node   l k v r   type'') -> 
    _                         -> part
            
parse :: String -> IO (Program Info)
parse file =
  do unchecked <- parsePunProgram file
     case unchecked of
       Left  problems -> die $ show problems
       Right program  -> return program

typed :: Program Info -> IO (Program Type)
typed = return . inferP . declarationsUpFront

shell :: Program Type -> IO ()
shell _program =
  do -- todo --
    die "future work"

-- todo: refactor.
action :: [String] -> IO Action
action [           file] = Shell         <$> (parse file >>= typed)
action ["--check", file] = PropertyCheck <$> (parse file >>= typed)
action ["--types", file] = TypeCheck     <$> parse file
action _                 = return $ Fail
  "Usage:\n\
     \ pun --check <program>.pun (checks properties)\n\
     \ pun --types <program>.pun (checks types)"
