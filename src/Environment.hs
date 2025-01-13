module Environment where

import Syntax

import           Control.Arrow
import qualified Control.Monad.RWS as RWS


-- * Program Environment

-- Definition
data Environment m a =
  Environment
    { function      :: F -> m (Term a)
    , definitionsIn :: [(Name, Term a)]
    , datatype      :: C -> m D
    , fieldTypes    :: C -> m [Type]
    , constructors  :: D -> m [TypeConstructor]
    }

-- Implementation
programEnvironment :: Monad m => Program a -> Environment m a
programEnvironment p =
  Environment
    { function = \f ->
        case lookup f (definitions p) of
          Just def -> return def
          Nothing  -> error $
            "Couldn't find definition for function '" ++ f ++ "'"
    , definitionsIn = definitions p
    , datatype = \c ->
        case lookup c (typeConstructorNames p) of
          Just  d -> return d
          Nothing -> error $
            "Couldn't find data type declaration for constructor '" ++ c ++ "'"
    , fieldTypes   = \c ->
        case lookup c (typeConstructorFields p) of
          Just ts -> return ts
          Nothing -> error $ "Couldn't find constructor with name '" ++ c ++ "'"
    , constructors = \d ->
        case lookup d (dataDeclarations p) of
          Just cs -> return cs
          Nothing -> error $ "Couldn't find data type with name '" ++ d ++ "'"
    }

-- * Environment Reader Writer State monad
newtype ERWS e r w s a =
  ERWS { coERWS :: RWS.RWS (Environment (ERWS e r w s) e, r) w s a }

instance Monoid w => Monad (ERWS e r w s) where
  return  = pure
  m >>= f = ERWS $ coERWS m >>= coERWS . f

instance Monoid w => Applicative (ERWS e r w s) where
  pure = ERWS . RWS.return
  e0 <*> e1 = e0 >>= \f -> f <$> e1

instance Monoid w => Functor (ERWS e r w s) where
  fmap f = ERWS . fmap f . coERWS

runERWS :: Monoid w => ERWS e r w s a -> Program e -> r -> s -> (a, s, w)
runERWS erws p r = RWS.runRWS (coERWS erws) (programEnvironment p, r)

-- Environment
environment :: Monoid w => ERWS e r w s (Environment (ERWS e r w s) e)
environment = ERWS $ fst <$> RWS.ask

-- Reader
local :: Monoid w => (r -> r) -> (ERWS e r w s b -> ERWS e r w s b)
local f = ERWS . RWS.local (second f) . coERWS

ask :: Monoid w => ERWS e r w s r
ask = ERWS $ snd <$> RWS.ask

-- Writer
tell :: Monoid w => w -> ERWS e r w s ()
tell = ERWS . RWS.tell

-- State
put :: Monoid w => s -> ERWS e r w s ()
put = ERWS . RWS.put

get :: Monoid w => ERWS e r w s s
get = ERWS RWS.get

modify :: Monoid w => (s -> s) -> ERWS e r w s ()
modify f = ERWS $ RWS.modify f
