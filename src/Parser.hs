
module Parser where

import Syntax
import Data.List (nub, (\\))

import Text.Parsec
import Control.Monad  (void, when)
import Data.Bifunctor (bimap, first, second)

-- Shorthands.
type Source      = String
type Parser      = Parsec Source ()
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
parseString p = runParser p () "<no-such-file>"

-- * Implementation:

type_ :: Parser Type
type_ =
  choice
  [ try $ type' >>= \t1 -> symbol "->" >> (t1 :->:) <$> type_
  , type'
  ]
  where
    type'  =
      choice
      [ try $ parens $ type'' >>= \t1 -> symbol  "," >> (t1  :*:) <$> type_
      , type''
      ]
    type'' =
      choice
      [ symbol "integer" >> return Integer'
      , symbol "boolean" >> return Boolean'
      , symbol "bst" >> BST <$> type' <*> type'
      , Variable' <$> nat
      , parens type_
      ]

simple :: Parser (Term Info)
simple =
  choice
  [ info $ int  >>= return . Number
  , info $ bool >>= return . Boolean
  , info $ symbol "leaf" >> return Leaf
  , info $ name >>= return . Variable
  , try $ parens $ term_
  , info $
    brackets $
      symbol "node" >>
        Node <$> term_ <*> term_ <*> term_
  ]

term_ :: Parser (Term Info)
term_ =
  choice $
    chainl1 simple (return $ \f x -> Application f x $ about f x) :
   map info
  [ do _ <- keyword "case"
       t <- term_
       _ <- keyword "of"
       _ <- symbol ";" >> symbol "leaf" >> symbol "->"
       l <- term_
       p <- symbol ";" >> term_
       _ <- symbol "->"
       r <- term_
       return $ Case t l (p, r)
  , do If <$> (pre "if" term_) <*> (pre "then" term_) <*> (pre "else" term_)
  , parens $ Pair <$> term_ <*> (pre "," term_)
  , pre "fst" (Fst <$> term_)
  , pre "snd" (Snd <$> term_)
  , pre "\\" $ Lambda <$> name <*> (pre "->" term_)
  , pre "let" $ Let <$> name <*> pre "=" term_ <*> term_
  , pre "rec" $ Rec <$> name <*> pre "." term_
  ]
  where
    about t1 t2   = (fst $ annotation t1, snd $ annotation t2)
    operator      =
      choice $
      [ pre  "+" $ return $ \t1 t2 -> Plus t1 t2 $ about t1 t2
      , pre "<=" $ return $ \t1 t2 -> Leq t1 t2 $ about t1 t2
      , return $ \f x -> Application f x $ about f x
      ]

-- -- * Utility:

pre, post :: String -> Parser a -> Parser a
pre  s p = symbol s >> p
post s p = p >>= \x -> symbol s >> return x

nat :: Parser Integer
nat = lexeme $ read <$> many1 digit

int :: Parser Integer
int = option id (symbol "~" >> return negate) <*> nat

bool :: Parser Bool
bool =
  choice
  [ symbol "true"  >> return True
  , symbol "false" >> return False
  ]

name :: Parser Name
name = try $
  do n <- lexeme $ many1 charAllowedInName
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
  , "leaf", "node", "case", "of"
  , "fst", "snd"
  , "let", "rec"
  ]

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
