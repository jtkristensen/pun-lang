
module GeneratorGenerator where

import Syntax

import Test.Tasty.QuickCheck
  (Gen, oneof, frequency, arbitrary, elements, suchThat, sized)

type LocalIndices         = [(Index, Type)]
type LocalBindings        = [(Name,  Type)]
type GlobalBindings       = [(Name,  Type)]
type ProgramConfiguration = (LocalIndices, LocalBindings, GlobalBindings)

decrease :: Int -> Int
decrease size = size `div` 2

generateGenerator :: DataDeclarations -> ProgramConfiguration -> (Type -> Gen (Term Type))
generateGenerator ds s t = sized (generateGeneratorSized ds s t)

generateCanonicalGenerator :: DataDeclarations -> ProgramConfiguration -> (Type -> (Int -> Gen (Term Type)))
generateCanonicalGenerator _ _ Unit'          _    = return $ Unit Unit'
generateCanonicalGenerator _ _ Integer'       _    = flip Number  Integer' <$> arbitrary
generateCanonicalGenerator _ _ Boolean'       _    = flip Boolean Boolean' <$> arbitrary
generateCanonicalGenerator ds s t@(type1 :*: type2) size =
  do t1 <- generateCanonicalGenerator ds s type1 (decrease size)
     t2 <- generateCanonicalGenerator ds s type2 (decrease size)
     return $ Pair t1 t2 t
generateCanonicalGenerator ds s (Algebraic d) size =
  oneof (map ctrGen (constructors ds d))
  where
    ctrGen (TypeConstructor c types) =
      do ts <- mapM (\tau -> generateCanonicalGenerator ds s tau (decrease size)) types
         return $ Constructor c ts (Algebraic d)
generateCanonicalGenerator ds (is, bs, ts) (type1 :->: type2) size =
  do x  <- generateName ts
     t0 <- generateGeneratorSized ds (is, (x, type1) : filter ((/=x) . fst) bs, ts) type2 (decrease size)
     return $ Lambda x t0 (type1 :->: type2)
generateCanonicalGenerator ds s tau size = generateGeneratorSized ds s tau size

generateGeneratorSized :: DataDeclarations -> ProgramConfiguration -> (Type -> (Int -> Gen (Term Type)))
generateGeneratorSized _  _              Unit'    _    = return $ Unit Unit'
generateGeneratorSized ds s              Integer' 0    = generateGeneratorSized ds s Integer' 1
generateGeneratorSized _  _              Integer' 1    = flip Number  Integer' <$> arbitrary
generateGeneratorSized ds s@(is, bs, ts) Integer' size =
  frequency $ zip [1..]
    [ flip Number  Integer' <$> arbitrary
    , do cond  <- generateGeneratorSized ds s Boolean'  (decrease size)
         t1    <- generateGeneratorSized ds s Integer'  (decrease size)
         t2    <- generateGeneratorSized ds s Integer'  (decrease size)
         return $ If cond t1 t2 Integer'
    , do t1    <- generateGeneratorSized ds s Integer'  (decrease size)
         t2    <- generateGeneratorSized ds s Integer'  (decrease size)
         return $ Plus t1 t2 Integer'
    , do t1    <- generateGeneratorSized ds s Integer' (decrease size)
         type2 <- generateType is (map snd ts)
         t2    <- generateGeneratorSized ds s type2    (decrease size)
         return $ Fst (Pair t1 t2 $ Integer' :*: type2) Integer'
    , do type1 <- generateType is (map snd ts)
         t1    <- generateGeneratorSized ds s type1    (decrease size)
         t2    <- generateGeneratorSized ds s Integer' (decrease size)
         return $ Snd (Pair t1 t2 $ type1 :*: Integer') Integer'
    , do argType <- generateType is (map snd ts)
         f       <- generateGeneratorSized ds s (argType :->: Integer') (decrease size)
         arg     <- generateGeneratorSized ds s argType (decrease size)
         return $ Application f arg Integer'
    , do x     <- generateName ts
         type1 <- generateType is (map snd bs)
         t1    <- generateGeneratorSized ds s type1 (decrease size)
         t2    <- generateGeneratorSized ds (is, (x, type1) : filter ((/=x) . fst) bs, ts) Integer' (decrease size)
         return $ Let x t1 t2 Integer'
    , do x     <- generateName ts
         -- this is the trivially terminating recursive term, because x does not occur !
         t1    <- generateGeneratorSized ds (is, filter ((/=x) . fst) bs, ts) Integer' (decrease size)
         return $ Rec x t1 Integer'
         ]
    ++ ((\a -> (15, return a)) . flip Variable Integer' <$> [ n | (n, t) <- bs , t == Integer' ])
generateGeneratorSized ds s          Boolean' 0 = generateGeneratorSized ds s Boolean' 1
generateGeneratorSized _  _          Boolean' 1 = flip Boolean Boolean' <$> arbitrary
generateGeneratorSized ds s@(is, bs, ts) Boolean' size           =
  frequency $ zip [1..]
    [ flip Boolean  Boolean' <$> arbitrary
    , do cond  <- generateGeneratorSized ds s Boolean'  (decrease size)
         t1    <- generateGeneratorSized ds s Boolean'  (decrease size)
         t2    <- generateGeneratorSized ds s Boolean'  (decrease size)
         return $ If cond t1 t2 Boolean'
    , do t1    <- generateGeneratorSized ds s Integer'  (decrease size)
         t2    <- generateGeneratorSized ds s Integer'  (decrease size)
         return $ Leq t1 t2 Boolean'
    , do type1 <- generateType is (map snd ts) >>= nonFunction
         t1    <- generateGeneratorSized ds s type1 (decrease size)
         t2    <- generateGeneratorSized ds s type1 (decrease size)
         return $ Equal t1 t2 Boolean'
    , do t1    <- generateGeneratorSized ds s Boolean' (decrease size)
         type2 <- generateType is (map snd ts)
         t2    <- generateGeneratorSized ds s type2    (decrease size)
         return $ Fst (Pair t1 t2 $ Boolean' :*: type2) Boolean'
    , do type1 <- generateType is (map snd ts)
         t1    <- generateGeneratorSized ds s type1    (decrease size)
         t2    <- generateGeneratorSized ds s Boolean' (decrease size)
         return $ Snd (Pair t1 t2 $ type1 :*: Boolean') Boolean'
    , do argType <- generateType is (map snd ts)
         f       <- generateGeneratorSized ds s (argType :->: Boolean') (decrease size)
         arg     <- generateGeneratorSized ds s argType (decrease size)
         return $ Application f arg Boolean'
    , do x     <- generateName ts
         type1 <- generateType is (map snd bs)
         t1    <- generateGeneratorSized ds s type1 (decrease size)
         t2    <- generateGeneratorSized ds (is, (x, type1) : filter ((/=x) . fst) bs, ts) Boolean' (decrease size)
         return $ Let x t1 t2 Boolean'
    , do x     <- generateName ts
         -- this is the trivially terminating recursive term, because x does not occur !
         t1    <- generateGeneratorSized ds (is, filter ((/=x) . fst) bs, ts) Boolean' (decrease size)
         return $ Rec x t1 Boolean'
         ]
    ++ ((\a -> (15, return a)) . flip Variable Boolean' <$> [ n | (n, t) <- bs , t == Boolean' ])
generateGeneratorSized ds s@(is, _, _) (Variable' index) size   =
  generateGeneratorSized ds s (resolve index is) (decrease size)
-- Todo, generate more interesting things here!
generateGeneratorSized ds s t@(type1 :*: type2) size =
  do t1 <- generateGeneratorSized ds s type1 (decrease size)
     t2 <- generateGeneratorSized ds s type2 (decrease size)
     return $ Pair t1 t2 t
-- Todo, generate more interesting things here!
generateGeneratorSized ds (is, bs, ts) (type1 :->: type2) size =
  do x  <- generateName ts
     t0 <- generateGeneratorSized ds (is, (x, type1) : filter ((/=x) . fst) bs, ts) type2 (decrease size)
     return $ Lambda x t0 (type1 :->: type2)
generateGeneratorSized ds s (Algebraic d) size =
  oneof (map ctrGen (constructors ds d))
  where
    ctrGen (TypeConstructor c types) =
      do ts <- mapM (\tau -> generateGeneratorSized ds s tau (decrease size)) types
         return $ Constructor c ts (Algebraic d)

resolve :: Index -> LocalIndices -> Type
resolve i is =
  case lookup i is of
    Just s ->
      case s of
        (Variable' i') -> resolve i' is
        _              -> s
    Nothing -> Integer' -- (we actually have several choices here /!\.

nonFunction :: Type -> Gen Type
nonFunction (t1 :->: t2) =
  oneof [ nonFunction t1
        , nonFunction t2
        ]
nonFunction t = return t

generateName :: GlobalBindings -> Gen Name
generateName ts = elements (pure <$> ['a'..'z'])
                  `suchThat` (\a -> notElem a $ map fst ts)

generateType :: LocalIndices -> [Type] -> Gen Type
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
