
module Parser where

import Syntax
import Data.List (nub, (\\))

import Text.Parsec
import Control.Monad  (void, when)
import Data.Bifunctor (bimap, first, second)

-- Shorthands.
type Source      = String
type ParserState = (Integer, [X])
type Parser      = Parsec Source ParserState
type Info        = (SourcePos, SourcePos)

-- Should this go into the parser?
data Problem =
    SeveralDeclarationsOf          X
  | SeveralDefinitionsOf           X
  | PropertyIsDeclaredMoreThanOnes P

problems :: Program a -> Maybe Problem
problems p = definitions' \+ declarations' \+ properties'
  where
    Nothing \+ t = t
    t       \+ _ = t
    ts           = fst <$> definitions  p
    ds           = fst <$> declarations p
    ps           = fst <$> properties   p
    definitions' =
      case ts \\ nub ts of
        (x : _) -> return $ SeveralDefinitionsOf x
        _       -> Nothing
    declarations' =
      case ds \\ nub ds of
        (x : _) -> return $ SeveralDeclarationsOf x
        _       -> Nothing
    properties' =
      case ps \\ nub ps of
        (x : _) -> return $ PropertyIsDeclaredMoreThanOnes x
        _       -> Nothing

-- * Usage:

parseString :: Parser a -> Source -> Either ParseError a
parseString p = runParser p (0, []) "<no-such-file>"

-- * Implementation:

term_ :: Parser (Term Info)
term_ = undefined

-- -- * Utility:

-- Supplies a fresh existential identifier.
fresh :: Parser Name
fresh =
  do (i, xs) <- getState
     let  x = "_x" ++ show i
     y <- if   x `elem` xs
          then modifyState (first (+1)) >> fresh
          else return x
     modifyState $ bimap (+1) (y:)
     return y

-- Parses a name.
name :: Parser Name
name = try $
  do n <- lexeme $ many1 charAllowedInName
     when (isReserved n) $ fail $ "Unexpected keyword " ++ n
     modifyState $ second (n:)
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
reserved = ["main", "data", "invert", "case", "of"]

-- Parses p and anny trailing whitespace following it.
lexeme :: Parser a -> Parser a
lexeme p =
  do a <- p
     _ <- many (void space)
     return a

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
