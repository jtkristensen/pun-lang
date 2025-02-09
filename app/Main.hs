module Main (main) where

import Syntax
import Parser
  ( parsePunProgram, Info, parseString, term_
  , parseShellCommand, ShellCommand(..)
  )
import TypeInference         (inferP, inferT)
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
    (Shell         program) -> shell program

numberOfTests :: Integer
numberOfTests = 50

strengthen :: Monad m => (a, m b) -> m (a, b)
strengthen (a, mb) = mb <&> (,) a

check :: Program Type -> IO ()
check program = void $ mapM check1 (properties program)
  where
    check1 (name, (args, body)) =
      do putStr $ "testing:" ++ name ++ "> "
         iter numberOfTests
      where
        dataDecls = dataDeclarations program
        config    = ([], [], declarations program)
        genParts  = mapM strengthen $ second (generateGenerator dataDecls config) <$> args
        term      = uncurry $ foldr (\(x, v) t -> substitute x t v)
        eval      = normalize program
        iter 0    = putStrLn " ok"
        iter n    =
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
             term <$> return (body, map (second eval) parts)

parse :: String -> IO (Program Info)
parse file =
  do unchecked <- parsePunProgram file
     case unchecked of
       Left  problems -> die $ show problems
       Right program  -> return program

typed :: Program Info -> IO (Program Type)
typed = return . inferP . declarationsUpFront

shell :: Program Type -> IO ()
shell program =
  do input  <- readLine
     result <- parseShellCommand input
     case result of
       (Left  err    ) -> die $ show err
       (Right command) -> execute program command

execute :: Program Type -> ShellCommand -> IO ()
execute _ Quit        = putStrLn "Quitting pun shell." >> return ()
execute _ (Load path) =
  do prog <- loadProgram path
     putStrLn $ "Loaded file " ++ show path ++ "."
     shell prog
execute program (Evaluate expr) =
  do parsed <- parseLine expr
     term   <- return $ inferT parsed
     print $ normalize program term
     shell program

-- Shell helpers
readLine :: IO String
readLine =
  do putStr "pun> "
     hFlush stdout
     getLine

parseLine :: String -> IO (Term Info)
parseLine input =
  case parseString term_ input of
    Left  err -> error $ "Parse error: " ++ show err
    Right t   -> return t

loadProgram :: String -> IO (Program Type)
loadProgram file = parse file >>= typed

-- todo: refactor.
action :: [String] -> IO Action
action ["--check", file] = PropertyCheck <$> (parse file >>= typed)
action ["--types", file] = TypeCheck     <$> parse file
action [           file] = Shell         <$> (parse file >>= typed)
action [ ]               = return $ Shell EndOfProgram
action _                 = return $ Fail
  "Usage:\n\
     \ pun --check <program>.pun (checks properties)\n\
     \ pun --types <program>.pun (checks types)"
