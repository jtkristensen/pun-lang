
module PropertyChecker where

import Syntax
import TypeInference

import Test.Tasty.QuickCheck

type Generator       = Gen (Term Type)
type CurrentIndices  = Substitution
type CurrentBindings = [(Name, Type)]

generateGenerator :: (CurrentIndices, CurrentBindings) -> (Type -> Generator)
generateGenerator s t = sized (generateGeneratorSized s t)

generateGeneratorSized :: (CurrentIndices, CurrentBindings) -> (Type -> Int -> Generator)
generateGeneratorSized s@(is, bs) Integer' sizedNumber          =
     case sizedNumber > 1 of
          True -> oneof $
                    [ flip Number  Integer' <$> arbitrary
                    , do cond <- generateGeneratorSized s Boolean' (sizedNumber `div` 2)
                         t1   <- generateGeneratorSized s Integer' (sizedNumber `div` 2)
                         t2   <- generateGeneratorSized s Integer' (sizedNumber `div` 2)
                         return $ If cond t1 t2 Integer'
                    , do t1   <- generateGeneratorSized s Integer' (sizedNumber `div` 2)
                         t2   <- generateGeneratorSized s Integer' (sizedNumber `div` 2)
                         return $ Plus t1 t2 Integer'
                    , do t1    <- generateGeneratorSized s Integer' (sizedNumber `div` 2)
                         type2 <- generateType is
                         t2    <- generateGeneratorSized s type2 (sizedNumber `div` 2)
                         return $ Fst (Pair t1 t2 $ Integer' :*: type2) Integer'
                    , do type1 <- generateType is
                         t1    <- generateGeneratorSized s type1 (sizedNumber `div` 2)
                         t2    <- generateGeneratorSized s Integer' (sizedNumber `div` 2)
                         return $ Snd (Pair t1 t2 $ type1 :*: Integer') Integer'
                    , do argType <- generateType is
                         f       <- generateGeneratorSized s (argType :->: Integer') (sizedNumber `div` 2)
                         arg     <- generateGeneratorSized s argType (sizedNumber `div` 2)
                         return $ Application f arg Integer'
                    --     Gamma |- t1 : T1         Gamma[x -> T1] |- t2 : T
                    -- Let --------------------------------------------------
                    --             Gamma |- let x = t1 in t2 : T
                    , do x     <- generateName
                         type1 <- generateType is
                         t1    <- generateGeneratorSized s type1 (sizedNumber `div` 2)
                         t2    <- generateGeneratorSized (is, (x, type1) : filter ((/=x) . fst) bs) Integer' (sizedNumber `div` 2)
                         return $ Let x t1 t2 Integer'
                    --            Gamma[x -> T] |- rec x t : T
                    -- Let --------------------------------------------------
                    --               Gamma |- rec x t : T
                    , do x     <- generateName
                         t1    <- generateGeneratorSized (is, (x, Integer') : filter ((/=x) . fst) bs) Integer' (sizedNumber `div` 2)
                         return $ Rec x t1 Integer'
                         ]
                    ++ (return . flip Variable Integer' <$> [ n | (n, t) <- bs , t == Integer' ])
          False -> flip Number  Integer' <$> arbitrary
generateGeneratorSized s@(is, bs) Boolean' sizedNumber           =
     case sizedNumber > 1 of
          True -> oneof $
                    [ flip Boolean  Boolean' <$> arbitrary
                    , do cond <- generateGeneratorSized s Boolean' (sizedNumber `div` 2)
                         t1   <- generateGeneratorSized s Boolean' (sizedNumber `div` 2)
                         t2   <- generateGeneratorSized s Boolean' (sizedNumber `div` 2)
                         return $ If cond t1 t2 Boolean'
                    , do t1   <- generateGeneratorSized s Integer' (sizedNumber `div` 2)
                         t2   <- generateGeneratorSized s Integer' (sizedNumber `div` 2)
                         return $ Leq t1 t2 Boolean'
                    , do t1    <- generateGeneratorSized s Boolean' (sizedNumber `div` 2)
                         type2 <- generateType is
                         t2    <- generateGeneratorSized s type2 (sizedNumber `div` 2)
                         return $ Fst (Pair t1 t2 $ Boolean' :*: type2) Boolean'
                    , do type1 <- generateType is
                         t1    <- generateGeneratorSized s type1 (sizedNumber `div` 2)
                         t2    <- generateGeneratorSized s Boolean' (sizedNumber `div` 2)
                         return $ Snd (Pair t1 t2 $ type1 :*: Boolean') Boolean'
                    , do argType <- generateType is
                         f       <- generateGeneratorSized s (argType :->: Boolean') (sizedNumber `div` 2)
                         arg     <- generateGeneratorSized s argType (sizedNumber `div` 2)
                         return $ Application f arg Boolean'
                    , do x     <- generateName
                         type1 <- generateType is
                         t1    <- generateGeneratorSized s type1 (sizedNumber `div` 2)
                         t2    <- generateGeneratorSized (is, (x, type1) : filter ((/=x) . fst) bs) Boolean' (sizedNumber `div` 2)
                         return $ Let x t1 t2 Boolean'
                    , do x     <- generateName
                         t1    <- generateGeneratorSized (is, (x, Boolean') : filter ((/=x) . fst) bs) Boolean' (sizedNumber `div` 2)
                         return $ Rec x t1 Boolean'
                         ]
                    ++ (return . flip Variable Boolean' <$> [ n | (n, t) <- bs , t == Boolean' ])
          False -> flip Boolean Boolean' <$> arbitrary
generateGeneratorSized s (Variable' index) sizedNumber   = generateGeneratorSized s (resolve index $ fst s) (sizedNumber `div` 2)
-- Todo, generate more interesting things here!
generateGeneratorSized s t@(type1 :*: type2) sizedNumber =
  do t1 <- generateGeneratorSized s type1 (sizedNumber `div` 2)
     t2 <- generateGeneratorSized s type2 (sizedNumber `div` 2)
     return $ Pair t1 t2 t
-- Todo, generate more interesting things here!
generateGeneratorSized (is, bs) (type1 :->: type2) sizedNumber =
  do x  <- generateName
     t0 <- generateGeneratorSized (is, (x, type1) : filter ((/=x) . fst) bs) type2 (sizedNumber `div` 2)
     return $ Lambda x t0 type2

resolve :: Index -> CurrentIndices -> Type
resolve i cs =
  case lookup i cs of
    Just s -> 
      case s of
        (Variable' i') -> resolve i' cs
        _              -> s
    Nothing -> error $ "Unable to resolve index to type. Index "
               ++ show i ++ " was not among current indices"

generateName :: Gen Name
generateName = elements $ pure <$> ['a'..'z']

generateType :: CurrentIndices -> Gen Type
generateType cs =
  oneof $
    [ return Integer'
    , return Boolean'
    , do type1 <- generateType cs
         type2 <- generateType cs
         return $ type1 :*: type2
    , do type1 <- generateType cs
         type2 <- generateType cs
         return $ type1 :->: type2
    ] ++ (return . Variable' . fst <$> cs)

-- Check takes the components of a property, and returns a generator for terms of type `Boolean'` that we can evaluate inside of QuickCheck.
check :: [(Name, Type)] -> Term Type -> Gen [(Name, Term Type)]
check _ _ = undefined

