module Main (main) where

import Syntax
import Parser                (parsePunProgram, Info)
import TypeInference         (inferP)
import Interpreter           (normalize, substitute)
import GeneratorGenerator    (generateGenerator)
import Control.Monad         (void)
import Control.Arrow         (second)
import Data.Functor          ((<&>))
import System.Exit           (die)
import System.Environment    (getArgs)
import System.IO             (hFlush, stdout)
import Test.Tasty.QuickCheck (generate)

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
    (Shell         _      ) -> die "future work"

numberOfTests :: Integer
numberOfTests = 50

strengthen :: Monad m => (a, m b) -> m (a, b)
strengthen (a, mb) = mb <&> (,) a

check :: Program Type -> IO ()
check program = void $ mapM check1 (properties program)
  where
    check1 (name, (args, body)) =
      do putStr $ "testing " ++ name ++ ": "
         iter numberOfTests
      where
        config = ([], [], declarations program)
        gen    = generate $ mapM strengthen $ second (generateGenerator config) <$> args
        term   = uncurry $ foldr (\(x, v) t -> substitute x t v)
        iter 0 = putStrLn " ok"
        iter n =
          do parts <- gen
             p     <- term <$> return (body, parts)
             case normalize program p of
               Boolean True _ -> putStr "." >> hFlush stdout >> iter (n - 1)
               _              ->
                 do print "failed with counter example :"
                    print $ show p
                    print $ "after " ++ show (numberOfTests - n) ++ " tests."

parse :: String -> IO (Program Info)
parse file =
  do unchecked <- parsePunProgram file
     case unchecked of
       Left  problems -> die $ show problems
       Right program  -> return program

typed :: Program Info -> IO (Program Type)
typed = return . inferP . declarationsUpFront

-- todo: refactor.
action :: [String] -> IO Action
action [           file] = Shell         <$> (parse file >>= typed)
action ["--check", file] = PropertyCheck <$> (parse file >>= typed)
action ["--types", file] = TypeCheck     <$> parse file
action _                 = return $ Fail
  "Usage:\n\
     \ pun --check <program>.pun (checks properties)\n\
     \ pun --types <program>.pun (checks types)"
