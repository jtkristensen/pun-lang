
module PropertyChecker where

import Syntax
import TypeInference
import Interpreter

import Test.Tasty.QuickCheck

type Generator            = Gen (Term Type)
type CurrentIndices       = Substitution
type CurrentBindings      = [(Name, Type)]
type TopLevelBindings     = [(Name, Type)]
type ProgramConfiguration = (CurrentIndices, CurrentBindings, TopLevelBindings)

decrease :: Int -> Int
decrease size = size `div` 2

generateGenerator :: ProgramConfiguration -> (Type -> Generator)
generateGenerator s t = sized (generateGeneratorSized s t)

generateGeneratorSized :: ProgramConfiguration -> (Type -> Int -> Generator)
generateGeneratorSized _          Unit'    _ = return $ Unit Unit'
generateGeneratorSized s          Integer' 0 = generateGeneratorSized s Integer' 1
generateGeneratorSized _          Integer' 1 = flip Number  Integer' <$> arbitrary
generateGeneratorSized s@(is, bs, ts) Integer' size =
  frequency $ zip [1..]
    [ flip Number  Integer' <$> arbitrary
    , do cond <- generateGeneratorSized s Boolean'  (decrease size)
         t1   <- generateGeneratorSized s Integer'  (decrease size)
         t2   <- generateGeneratorSized s Integer'  (decrease size)
         return $ If cond t1 t2 Integer'
    , do t1   <- generateGeneratorSized s Integer'  (decrease size)
         t2   <- generateGeneratorSized s Integer'  (decrease size)
         return $ Plus t1 t2 Integer'
    , do t1    <- generateGeneratorSized s Integer' (decrease size)
         type2 <- generateType is (map snd ts)
         t2    <- generateGeneratorSized s type2    (decrease size)
         return $ Fst (Pair t1 t2 $ Integer' :*: type2) Integer'
    , do type1 <- generateType is (map snd ts)
         t1    <- generateGeneratorSized s type1    (decrease size)
         t2    <- generateGeneratorSized s Integer' (decrease size)
         return $ Snd (Pair t1 t2 $ type1 :*: Integer') Integer'
    , do argType <- generateType is (map snd ts)
         f       <- generateGeneratorSized s (argType :->: Integer') (decrease size)
         arg     <- generateGeneratorSized s argType (decrease size)
         return $ Application f arg Integer'
    , do x     <- generateName ts
         type1 <- generateType is (map snd bs)
         t1    <- generateGeneratorSized s type1 (decrease size)
         t2    <- generateGeneratorSized (is, (x, type1) : filter ((/=x) . fst) bs, ts) Integer' (decrease size)
         return $ Let x t1 t2 Integer'
    , do x     <- generateName ts
         -- this is the trivially terminating recursive term, because x does not occur !
         t1    <- generateGeneratorSized (is, filter ((/=x) . fst) bs, ts) Integer' (decrease size)
         return $ Rec x t1 Integer'
         ]
    ++ ((\a -> (15, return a)) . flip Variable Integer' <$> [ n | (n, t) <- bs , t == Integer' ])
generateGeneratorSized s          Boolean' 0 = generateGeneratorSized s Boolean' 1
generateGeneratorSized _          Boolean' 1 = flip Boolean Boolean' <$> arbitrary
generateGeneratorSized s@(is, bs, ts) Boolean' size           =
  frequency $ zip [1..]
    [ flip Boolean  Boolean' <$> arbitrary
    , do cond <- generateGeneratorSized s Boolean'  (decrease size)
         t1   <- generateGeneratorSized s Boolean'  (decrease size)
         t2   <- generateGeneratorSized s Boolean'  (decrease size)
         return $ If cond t1 t2 Boolean'
    , do t1   <- generateGeneratorSized s Integer'  (decrease size)
         t2   <- generateGeneratorSized s Integer'  (decrease size)
         return $ Leq t1 t2 Boolean'
    , do t1    <- generateGeneratorSized s Boolean' (decrease size)
         type2 <- generateType is (map snd ts)
         t2    <- generateGeneratorSized s type2    (decrease size)
         return $ Fst (Pair t1 t2 $ Boolean' :*: type2) Boolean'
    , do type1 <- generateType is (map snd ts)
         t1    <- generateGeneratorSized s type1    (decrease size)
         t2    <- generateGeneratorSized s Boolean' (decrease size)
         return $ Snd (Pair t1 t2 $ type1 :*: Boolean') Boolean'
    , do argType <- generateType is (map snd ts)
         f       <- generateGeneratorSized s (argType :->: Boolean') (decrease size)
         arg     <- generateGeneratorSized s argType (decrease size)
         return $ Application f arg Boolean'
    , do x     <- generateName ts
         type1 <- generateType is (map snd bs)
         t1    <- generateGeneratorSized s type1 (decrease size)
         t2    <- generateGeneratorSized (is, (x, type1) : filter ((/=x) . fst) bs, ts) Boolean' (decrease size)
         return $ Let x t1 t2 Boolean'
    , do x     <- generateName ts
         -- this is the trivially terminating recursive term, because x does not occur !
         t1    <- generateGeneratorSized (is, filter ((/=x) . fst) bs, ts) Boolean' (decrease size)
         return $ Rec x t1 Boolean'
         ]
    ++ ((\a -> (15, return a)) . flip Variable Boolean' <$> [ n | (n, t) <- bs , t == Boolean' ])
generateGeneratorSized s@(is, _, _) (Variable' index) size   =
  generateGeneratorSized s (resolve index is) (decrease size)
-- Todo, generate more interesting things here!
generateGeneratorSized s t@(type1 :*: type2) size =
  do t1 <- generateGeneratorSized s type1 (decrease size)
     t2 <- generateGeneratorSized s type2 (decrease size)
     return $ Pair t1 t2 t
-- Todo, generate more interesting things here!
generateGeneratorSized (is, bs, ts) (type1 :->: type2) size =
  do x  <- generateName ts
     t0 <- generateGeneratorSized (is, (x, type1) : filter ((/=x) . fst) bs, ts) type2 (decrease size)
     return $ Lambda x t0 type2
generateGeneratorSized s bst@(BST type1 type2) size =
  oneof [ do k <- generateGeneratorSized s type1 (decrease size)
             v <- generateGeneratorSized s type2 (decrease size)
             l <- generateGeneratorSized s bst   (decrease size)
             r <- generateGeneratorSized s bst   (decrease size)
             return $ Node l k v r bst
        , return $ Leaf (BST type1 type2)
        ]

resolve :: Index -> CurrentIndices -> Type
resolve i is =
  case lookup i is of
    Just s ->
      case s of
        (Variable' i') -> resolve i' is
        _              -> s
    Nothing -> Integer' -- (we actually have several choices here /!\.

generateName :: TopLevelBindings -> Gen Name
generateName ts = elements (pure <$> ['a'..'z'])
                  `suchThat` (\a -> notElem a $ map fst ts)

generateType :: CurrentIndices -> [Type] -> Gen Type
generateType is bindingTypes =
  oneof $
    [ return Integer'
    , return Boolean'
    -- , return Unit'
    , do type1 <- generateType is bindingTypes
         type2 <- generateType is bindingTypes
         return $ type1 :*: type2
    , do type1 <- generateType is bindingTypes
         type2 <- generateType is bindingTypes
         return $ type1 :->: type2
    ] ++ (return . Variable' . fst <$> is)
      ++ map return bindingTypes

propertyToCheck :: Program Type -> [(Name, Type)] -> Term Type -> Gen (Term Type)
propertyToCheck p bs t =
  do terms  <- mapM strengthen $ (\(a, b) -> (a, generateGenerator programConfig b)) <$> bs
     return $ foldr (\(x, v) t' -> Interpreter.substitute x t' v) t terms
  where
    programConfig = ([], [], declarations p)
    strengthen (a, mb) = mb >>= return . (,) a
