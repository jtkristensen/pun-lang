
module PropertyChecker where

import Syntax
import TypeInference

import Test.Tasty.QuickCheck

type Generator        = Gen (Term Type)
type CurrentIndices   = Substitution
type CurrentBindings  = [(Name, Type)]
type TopLevelBindings = [(Name, Type)]
type ProgramState     = (CurrentIndices, CurrentBindings, TopLevelBindings)

frac :: Int -> Int
frac size = size `div` 2

generateGenerator :: ProgramState -> (Type -> Generator)
generateGenerator s t = sized (generateGeneratorSized s t)

generateGeneratorSized :: ProgramState -> (Type -> Int -> Generator)
generateGeneratorSized s          Integer' 0 = generateGeneratorSized s Integer' 1
generateGeneratorSized _          Integer' 1 = flip Number  Integer' <$> arbitrary
generateGeneratorSized s@(is, bs, ts) Integer' size =
  frequency $ zip [1..]
    [ flip Number  Integer' <$> arbitrary
    , do cond <- generateGeneratorSized s Boolean'  (frac size)
         t1   <- generateGeneratorSized s Integer'  (frac size)
         t2   <- generateGeneratorSized s Integer'  (frac size)
         return $ If cond t1 t2 Integer'
    , do t1   <- generateGeneratorSized s Integer'  (frac size)
         t2   <- generateGeneratorSized s Integer'  (frac size)
         return $ Plus t1 t2 Integer'
    , do t1    <- generateGeneratorSized s Integer' (frac size)
         type2 <- generateType is []
         t2    <- generateGeneratorSized s type2    (frac size)
         return $ Fst (Pair t1 t2 $ Integer' :*: type2) Integer'
    , do type1 <- generateType is []
         t1    <- generateGeneratorSized s type1    (frac size)
         t2    <- generateGeneratorSized s Integer' (frac size)
         return $ Snd (Pair t1 t2 $ type1 :*: Integer') Integer'
    , do argType <- generateType is []
         f       <- generateGeneratorSized s (argType :->: Integer') (frac size)
         arg     <- generateGeneratorSized s argType (frac size)
         return $ Application f arg Integer'
    --     Gamma |- t1 : T1         Gamma[x -> T1] |- t2 : T
    -- Let --------------------------------------------------
    --             Gamma |- let x = t1 in t2 : T
    , do x     <- generateName ts
         type1 <- generateType is (map snd bs)
         t1    <- generateGeneratorSized s type1 (frac size)
         t2    <- generateGeneratorSized (is, (x, type1) : filter ((/=x) . fst) bs, ts) Integer' (frac size)
         return $ Let x t1 t2 Integer'
    --            Gamma[x -> T] |- rec x t : T
    -- Let --------------------------------------------------
    --               Gamma |- rec x t : T
    , do x     <- generateName ts
         t1    <- generateGeneratorSized (is, (x, Integer') : filter ((/=x) . fst) bs, ts) Integer' (frac size)
         return $ Rec x t1 Integer'
         ]
    ++ ((\a -> (15, return a)) . flip Variable Integer' <$> [ n | (n, t) <- bs , t == Integer' ])
generateGeneratorSized s          Boolean' 0 = generateGeneratorSized s Boolean' 1
generateGeneratorSized _          Boolean' 1 = flip Boolean Boolean' <$> arbitrary
generateGeneratorSized s@(is, bs, ts) Boolean' size           =
  frequency $ zip [1..]
    [ flip Boolean  Boolean' <$> arbitrary
    , do cond <- generateGeneratorSized s Boolean'  (frac size)
         t1   <- generateGeneratorSized s Boolean'  (frac size)
         t2   <- generateGeneratorSized s Boolean'  (frac size)
         return $ If cond t1 t2 Boolean'
    , do t1   <- generateGeneratorSized s Integer'  (frac size)
         t2   <- generateGeneratorSized s Integer'  (frac size)
         return $ Leq t1 t2 Boolean'
    , do t1    <- generateGeneratorSized s Boolean' (frac size)
         type2 <- generateType is []
         t2    <- generateGeneratorSized s type2    (frac size)
         return $ Fst (Pair t1 t2 $ Boolean' :*: type2) Boolean'
    , do type1 <- generateType is []
         t1    <- generateGeneratorSized s type1    (frac size)
         t2    <- generateGeneratorSized s Boolean' (frac size)
         return $ Snd (Pair t1 t2 $ type1 :*: Boolean') Boolean'
    , do argType <- generateType is []
         f       <- generateGeneratorSized s (argType :->: Boolean') (frac size)
         arg     <- generateGeneratorSized s argType (frac size)
         return $ Application f arg Boolean'
    , do x     <- generateName ts
         type1 <- generateType is (map snd bs)
         t1    <- generateGeneratorSized s type1 (frac size)
         t2    <- generateGeneratorSized (is, (x, type1) : filter ((/=x) . fst) bs, ts) Boolean' (frac size)
         return $ Let x t1 t2 Boolean'
    , do x     <- generateName ts
         t1    <- generateGeneratorSized (is, (x, Boolean') : filter ((/=x) . fst) bs, ts) Boolean' (frac size)
         return $ Rec x t1 Boolean'
         ]
    ++ ((\a -> (15, return a)) . flip Variable Boolean' <$> [ n | (n, t) <- bs , t == Boolean' ])
generateGeneratorSized s@(is, _, _) (Variable' index) size   =
  generateGeneratorSized s (resolve index is) (frac size)
-- Todo, generate more interesting things here!
generateGeneratorSized s t@(type1 :*: type2) size =
  do t1 <- generateGeneratorSized s type1 (frac size)
     t2 <- generateGeneratorSized s type2 (frac size)
     return $ Pair t1 t2 t
-- Todo, generate more interesting things here!
generateGeneratorSized (is, bs, ts) (type1 :->: type2) size =
  do x  <- generateName ts
     t0 <- generateGeneratorSized (is, (x, type1) : filter ((/=x) . fst) bs, ts) type2 (frac size)
     return $ Lambda x t0 type2

resolve :: Index -> CurrentIndices -> Type
resolve i is =
  case lookup i is of
    Just s ->
      case s of
        (Variable' i') -> resolve i' is
        _              -> s
    Nothing -> error $ "Unable to resolve index " ++ show i ++
      " to type, was not among current indices."

generateName :: TopLevelBindings -> Gen Name
generateName ts = (elements $ pure <$> ['a'..'z'])
                  `suchThat` (\a -> notElem a $ map fst ts)

generateType :: CurrentIndices -> [Type] -> Gen Type
generateType is bindingTypes =
  oneof $
    [ return Integer'
    , return Boolean'
    , do type1 <- generateType is []
         type2 <- generateType is []
         return $ type1 :*: type2
    , do type1 <- generateType is []
         type2 <- generateType is []
         return $ type1 :->: type2
    ] ++ (return . Variable' . fst <$> is)
      ++ map return bindingTypes

-- should Thing = Gen Bool ?
-- should Thing be Testable ?
-- what does a counterexample of Thing look like?
type Thing = Property

check :: [(Name, Type)] -> Term Type -> Thing
check _ _ = undefined
