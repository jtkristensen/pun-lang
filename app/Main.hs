module Main (main) where

import Syntax
import Parser        (parsePunProgram, Info)
import TypeInference (inferP)
-- import Interpreter  (normalize)

import System.Exit        (die)
import System.Environment (getArgs)

type ErrorMessage = String

data Action =
    Shell     (Program Type)
  | Check     (Program Type)
  | TypeCheck (Program Info)
  | Fail  ErrorMessage

main :: IO ()
main = getArgs >>= run . action

run :: IO Action -> IO ()
run a =
  do what <- a
     case what of
       (Shell     _program) -> undefined
       (Check     _program) -> undefined
       (TypeCheck program) -> print $ inferP $ declarationsUpFront program
       (Fail  message)  -> die message

action :: [String] -> IO Action
action [           _file] = undefined -- Shell <$> parsePunProgram file
action ["--check", _file] = undefined -- Check <$> parsePunProgram file
action ["--types", file] =
  do p <- parsePunProgram file
     case p of
       Left problems -> die $ show problems
       Right program -> return $ TypeCheck program
action _                 = return $ Fail
  "Usage:\n\
     \ pun         <program>.pun (starts a shell)\n\
     \ pun --check <program>.pun (checks properties)\n\
     \ pun --types <program>.pun (checks types)"
