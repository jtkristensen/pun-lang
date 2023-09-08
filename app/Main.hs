module Main (main) where

import Syntax
import Parser        (parsePunProgram, Info)
import TypeInference (inferP)
import Interpreter   (normalize)

import Control.Monad      (void)
import System.Exit        (die)
import System.Environment (getArgs)

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

check :: Program Type -> IO ()
check program = void $ mapM checkProperty (properties program)
  where
    checkProperty (name, (args, body)) =
      print $ normalize program body

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
