
module Parser
  ( Parser, Info, term_, type_, program_, nat_, int_
  , runParser, parseString, problems
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

-- Should this go int_o the parser?
data Problem =
    SeveralDeclarationsOf          X
  | SeveralDefinitionsOf           X
  | PropertyIsDeclaredMoreThanOnes P

problems :: Program a -> [Problem]
problems p = definitions' <> declarations' <> properties'
  where
    ts           = fst <$> definitions  p
    ds           = fst <$> declarations p
    ps           = fst <$> properties   p
    definitions' =
      case ts \\ nub ts of
        (x : _) -> return $ SeveralDefinitionsOf x
        _       -> mempty
    declarations' =
      case ds \\ nub ds of
        (x : _) -> return $ SeveralDeclarationsOf x
        _       -> mempty
    properties' =
      case ps \\ nub ps of
        (x : _) -> return $ PropertyIsDeclaredMoreThanOnes x
        _       -> mempty

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
      , Variable' <$> nat_
      , parens type_
      ]

simple :: Parser (Term Info)
simple =
  choice
  [ info $ int_  <&> Number
  , info $ bool_ <&> Boolean
  , info $ symbol "leaf" >> return Leaf
  , info $ name <&> Variable
  , info $ try $ parens $ Pair <$> term_ <*> pre "," term_
  , parens term_
  , info $
    brackets $
      symbol "node" >>
        Node <$> simple <*> simple <*> simple <*> simple
  ]

term_ :: Parser (Term Info)
term_ =
  choice $
    chainl1 simple operator :
   map info
  [ do _ <- keyword "case"
       t <- term_
       _ <- keyword "of"
       _ <- symbol ";" >> symbol "leaf" >> symbol "->"
       l <- term_
       p <- symbol ";" >> simple
       _ <- symbol "->"
       r <- term_
       return $ Case t l (p, r)
  , do If <$> pre "if" term_ <*> pre "then" term_ <*> pre "else" term_
  , pre "fst" (Fst <$> term_)
  , pre "snd" (Snd <$> term_)
  , pre "\\" $ Lambda <$> name <*> pre "->" term_
  , pre "let" $ Let <$> name <*> pre "=" term_ <*> term_
  , pre "rec" $ Rec <$> name <*> pre "." term_
  ]
  where
    about t1 t2   = (fst $ annotation t1, snd $ annotation t2)
    operator      =
      choice
        [ pre  "+" $ return $ \t1 t2 -> Plus t1 t2 $ about t1 t2
        , pre "<=" $ return $ \t1 t2 -> Leq t1 t2 $ about t1 t2
        , return $ \f x -> Application f x $ about f x
        ]

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
  do p <- many (choice [ try declaration_, definition_, property_ (info ((,) <$> name))])
     _ <- eof
     return p

-- -- * Utility:

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
