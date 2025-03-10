
module Parser
  ( Parser, Info, term_, type_, program_, nat_, int_
  , Source, runParser, parseString, problems, parsePunProgram, Problem
  , parseTerm, parseShellCommand
  )
where

import Syntax
import Data.List (nub, (\\))
import Data.Functor ((<&>))

import Text.Parsec
import Control.Monad  (void, when)

-- Shorthands.
type Source           = String
type Parser           = Parsec Source ()
type Info             = (SourcePos, SourcePos)
type Transformation a = (a -> a)

-- todo, extend these errors to carry sourse positions info.
data Problem =
    SeveralDeclarationsOf          X
  | SeveralDefinitionsOf           F
  | PropertyIsDeclaredMoreThanOnce P
  | DeclaredButNotDefined          F
  | DoesNotParse                   ParseError
  deriving (Eq, Show)

problems :: Program Info -> [Problem]
problems p =
     [ SeveralDefinitionsOf              f | f <- ts \\ nub ts ]
  <> [ SeveralDeclarationsOf             f | f <- ds \\ nub ds ]
     <> [ PropertyIsDeclaredMoreThanOnce q | q <- ps \\ nub ps ]
  <> [ DeclaredButNotDefined f | f <- ds , f `notElem` ts ]
  where
    ts = fst <$> definitions  p
    ds = fst <$> declarations p
    ps = fst <$> properties   p

-- * Usage:

parseString :: Parser a -> Source -> Either ParseError a
parseString p = runParser p () "<repl>"

parsePunProgram :: Source -> IO (Either [Problem] (Program Info))
parsePunProgram path =
  do src <- readFile path
     return $
       case runParser (many whitespace >> program_) () path src of
         (Left  err ) -> Left $ return $ DoesNotParse err
         (Right code) ->
           case problems code of
             [   ] -> return code
             _     -> Left $ problems code

parseTerm :: String -> IO (Term Info)
parseTerm input =
  case parseString term_ input of
    Left  err -> error $ "Parse error: " ++ show err
    Right t   -> return t

parseShellCommand :: String -> IO (Either Problem (ShellCommand Info))
parseShellCommand input =
  return $
    case runParser (many whitespace >> shellCommand) () "<repl>" input of
      (Left  err    ) -> Left  $ DoesNotParse err
      (Right command) -> Right command

-- * Implementation:

type_ :: Parser Type
type_ =
  choice
  [ try $ type' >>= \t1 -> symbol "->" >> (t1 :->:) <$> type_
  , type'
  ]
  where
    type' =
      choice
      [ Integer'  <$  symbol "integer"
      , Boolean'  <$  symbol "boolean"
      , Variable' <$> nat_
      , parens type_
      , Algebraic <$> constructorName
      ]

simple :: Parser (Term Info)
simple =
  choice
  [ info $ int_  <&> Number
  , info $ bool_ <&> Boolean
  , info $ name  <&> Variable
  , info $ Constructor <$> constructorName <*> option [] (brackets (sepBy term_ (symbol ",")))
  , parens term_
  ]

term_ :: Parser (Term Info)
term_ =
  choice $
    -- should we make "==" non-assoc?
    foldr (flip chainl1) simple [equal, leq, add, app] :
   map info
  [do _  <- keyword "case"
      t  <- term_
      _  <- keyword "of"
      cs <- many1 caseBranch
      return $ Case t cs
  , do If <$> pre "if" term_ <*> pre "then" term_ <*> pre "else" term_
  , pre "\\" $ Lambda <$> name <*> pre "->" term_
  , pre "let" $ Let <$> name <*> pre "=" term_ <*> pre "in" term_
  , pre "rec" $ Rec <$> name <*> pre "." term_
  ]
  where
    lift1 op t1 t2 = op t1 t2 (fst $ annotation t1, snd $ annotation t2)
    leq            = pre "<=" $ return $ lift1 Leq
    add            = pre  "+" $ return $ lift1 Plus
    equal          = pre "==" $ return $ lift1 Equal
    app            = return $ lift1 Application

caseBranch :: Parser (Pattern Info, Term Info)
caseBranch =
  do _ <- symbol ";"
     p <- simple
     _ <- symbol "->"
     r <- term_
     return (p, r)

constructorName :: Parser Name
constructorName = lexeme $ (:) <$> upper <*> many letter

typeConstructor :: Parser TypeConstructor
typeConstructor = 
  do 
    c  <- constructorName
    ts <- option [] $ brackets (sepBy1 type_ (symbol ","))
    return $ TypeConstructor c ts

data_ :: Parser (Transformation (Program Info))
data_ =
  do _  <- keyword "data"
     d  <- constructorName
     _  <- symbol "="
     cs <- typeConstructor `sepBy1` symbol "|"
     _  <- symbol "."
     return $ Data d cs

declaration_ :: Parser (Transformation (Program Info))
declaration_ =
  do f <- name
     _ <- symbol ":"
     t <- type_
     _ <- symbol "."
     return $ Declaration f t

definition_ :: Parser (Transformation (Program Info))
definition_ =
  do f    <- name
     args <- many $ info $ (,) <$> name
     _    <- symbol "="
     t    <- term_
     _    <- symbol "."
     return $ Definition f $ foldr (\(x, a) t' -> Lambda x t' a) t args

property_ :: Parser (X, Info) -> Parser (Transformation (Program Info))
property_ xa =
  do _  <- keyword "property"
     p  <- name
     xs <- many xa
     _  <- symbol "."
     t  <- term_
     _  <- symbol "."
     return $ Property p xs t

program_ :: Parser (Program Info)
program_ =
  fmap (foldr (\a b -> a b) EndOfProgram) $
  do p <- many (choice [ try declaration_, definition_, data_, property_ (info ((,) <$> name))])
     _ <- eof
     return p

-- * Shell commands

quit :: Parser (ShellCommand Info)
quit =
  do _ <- symbol ":q"
     return Quit

load :: Parser (ShellCommand Info)
load =
  do _        <- symbol ":l"
     filepath <- many1 anyChar
     _        <- many whitespace
     return $ Load filepath

evaluate :: Parser (ShellCommand Info)
evaluate =
  do expression <- term_
     _          <- many whitespace
     return $ Evaluate expression

shellCommand :: Parser (ShellCommand Info)
shellCommand =
  choice
    [ quit
    , load
    , evaluate
    ]

-- * Utility:

pre :: String -> Parser a -> Parser a
pre  s p = symbol s >> p

-- post :: String -> Parser a -> Parser a
-- post s p = p >>= \x -> symbol s >> return x

nat_ :: Parser Integer
nat_ = lexeme $ read <$> many1 digit

int_ :: Parser Integer
int_ = option id (symbol "~" >> return negate) <*> nat_

bool_ :: Parser Bool
bool_ =
  choice
  [ symbol "true"  >> return True
  , symbol "false" >> return False
  ]

nameStart :: Parser Char
nameStart = choice [lower, digit, dash, underscore]

name :: Parser Name
name = try $
  do n <- lexeme $ (:) <$> nameStart <*> many charAllowedInName
     when (isReserved n) $ fail $ "Unexpected keyword " ++ n
     return n

-- Adds source position information to a parser that consumes it.
info :: Parser (Info -> a) -> Parser a
info p =
  do i <- getPosition
     m <- p
     j <- getPosition
     return (m (i, j))

-- Parses between "(" and ")".
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parses between "[" and "]".
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- These are reserved keywords in the Jeopardy language.
reserved :: [Name]
reserved =
  [ "property"
  , "if", "then", "else"
  , "case", "of"
  , "let", "in", "rec"
  , "data"
  ]

-- Parses p and any trailing whitespace following it.
lexeme :: Parser a -> Parser a
lexeme p =
  do a <- p
     _ <- many whitespace
     return a

comment :: Parser ()
comment = void $ lexeme $ symbol "//" >> many (noneOf ['\n'])

whitespace :: Parser ()
whitespace = comment <|> void space

-- Holds if a name constitutes a reserved keyword.
isReserved :: Name -> Bool
isReserved = flip elem reserved

-- Treats a string as a parsable symbol.
symbol :: String -> Parser ()
symbol s = void $ lexeme $ try $ string s

-- Parses a known reserved keyword.
keyword :: String -> Parser ()
keyword k = try $ void $ lexeme $
  do _ <- string k
     notFollowedBy charAllowedInName

-- Parses the character '_'.
underscore :: Parser Char
underscore =
  char '_'

-- Parses the character '-'.
dash :: Parser Char
dash =
  char '-'

-- Parses any character that is allowed to occur in a name.
charAllowedInName :: Parser Char
charAllowedInName =
  try $ choice [ letter , digit , dash, underscore ]
