
module GeneratorGenerator where

import Syntax

import Test.Tasty.QuickCheck
  (Gen, oneof, frequency, arbitrary, elements, suchThat, sized)

type LocalIndices         = [(Index, Type)]
type LocalBindings        = [(Name,  Type)]
type GlobalBindings       = [(Name,  Type)]
type ProgramConfiguration = (LocalIndices, LocalBindings, GlobalBindings)

half :: Int -> Int
half size = size `div` 2

third :: Int -> Int
third size = size `div` 3

generateGenerator :: DataDeclarations -> ProgramConfiguration -> (Type -> Gen (Term Type))
generateGenerator ds s t = sized (generateCanonicalGenerator ds s t)

generateCanonicalGenerator :: DataDeclarations -> ProgramConfiguration -> (Type -> (Int -> Gen (Term Type)))
generateCanonicalGenerator _ _ Integer'       _    = flip Number  Integer' <$> arbitrary
generateCanonicalGenerator _ _ Boolean'       _    = flip Boolean Boolean' <$> arbitrary
generateCanonicalGenerator ds s (Algebraic d) size =
  if size < 1
  then ctrGen (smallest (constructors ds d))
  else oneof (map ctrGen (constructors ds d))
  where
    ctrGen (TypeConstructor c types) =
      do ts <- mapM (\tau -> generateCanonicalGenerator ds s tau (size `div` length types)) types
         return $ Constructor c ts (Algebraic d)
    smallest [t] = t
    smallest (TypeConstructor c ts : TypeConstructor c' ts' : rest) =
      if length ts < length ts'
      then smallest (TypeConstructor c  ts  : rest)
      else smallest (TypeConstructor c' ts' : rest)
    smallest _ = error "todo : deal with void."
generateCanonicalGenerator ds (is, bs, ts) (type1 :->: type2) size =
  do x  <- generateName ts
     t0 <- generateTermGenerator ds (is, (x, type1) : filter ((/=x) . fst) bs, ts) type2 (half size)
     return $ Lambda x t0 (type1 :->: type2)
generateCanonicalGenerator ds s (Variable' _) size = generateCanonicalGenerator ds s Integer' size

generateTermGenerator :: DataDeclarations -> ProgramConfiguration -> (Type -> (Int -> Gen (Term Type)))
generateTermGenerator ds s              t        0    = generateCanonicalGenerator ds s t 0
generateTermGenerator ds s@(is, bs, ts) Integer' size =
  frequency $ zip [1..]
    [ flip Number  Integer' <$> arbitrary
    , do cond  <- generateTermGenerator ds s Boolean'  (third size)
         t1    <- generateTermGenerator ds s Integer'  (third size)
         t2    <- generateTermGenerator ds s Integer'  (third size)
         return $ If cond t1 t2 Integer'
    , do t1    <- generateTermGenerator ds s Integer'  (half size)
         t2    <- generateTermGenerator ds s Integer'  (half size)
         return $ Plus t1 t2 Integer'
    , do argType <- generateType is (map snd ts)
         f       <- generateTermGenerator ds s (argType :->: Integer') (half size)
         arg     <- generateTermGenerator ds s argType (half size)
         return $ Application f arg Integer'
    , do x     <- generateName ts
         type1 <- generateType is (map snd bs)
         t1    <- generateTermGenerator ds s type1 (half size)
         t2    <- generateTermGenerator ds (is, (x, type1) : filter ((/=x) . fst) bs, ts) Integer' (half size)
         return $ Let x t1 t2 Integer'
    , do x     <- generateName ts
         -- this is the trivially terminating recursive term, because x does not occur !
         t1    <- generateTermGenerator ds (is, filter ((/=x) . fst) bs, ts) Integer' (half size)
         return $ Rec x t1 Integer'
         ]
    ++ ((\a -> (15, return a)) . flip Variable Integer' <$> [ n | (n, t) <- bs , t == Integer' ])
generateTermGenerator ds s@(is, bs, ts) Boolean' size           =
  frequency $ zip [1..]
    [ flip Boolean  Boolean' <$> arbitrary
    , do cond  <- generateTermGenerator ds s Boolean'  (third size)
         t1    <- generateTermGenerator ds s Boolean'  (third size)
         t2    <- generateTermGenerator ds s Boolean'  (third size)
         return $ If cond t1 t2 Boolean'
    , do t1    <- generateTermGenerator ds s Integer'  (half size)
         t2    <- generateTermGenerator ds s Integer'  (half size)
         return $ Leq t1 t2 Boolean'
    , do type1 <- generateType is (map snd ts) >>= nonFunction
         t1    <- generateTermGenerator ds s type1 (half size)
         t2    <- generateTermGenerator ds s type1 (half size)
         return $ Equal t1 t2 Boolean'
    , do argType <- generateType is (map snd ts)
         f       <- generateTermGenerator ds s (argType :->: Boolean') (half size)
         arg     <- generateTermGenerator ds s argType                 (half size)
         return $ Application f arg Boolean'
    , do x     <- generateName ts
         type1 <- generateType is (map snd bs)
         t1    <- generateTermGenerator ds s type1 (half size)
         t2    <- generateTermGenerator ds (is, (x, type1) : filter ((/=x) . fst) bs, ts) Boolean' (half size)
         return $ Let x t1 t2 Boolean'
    , do x     <- generateName ts
         -- this is the trivially terminating recursive term, because x does not occur !
         t1    <- generateTermGenerator ds (is, filter ((/=x) . fst) bs, ts) Boolean' (size - 1)
         return $ Rec x t1 Boolean'
         ]
    ++ ((\a -> (15, return a)) . flip Variable Boolean' <$> [ n | (n, t) <- bs , t == Boolean' ])
generateTermGenerator ds s@(is, _, _) (Variable' index) size  =  generateTermGenerator ds s (resolve index is) size
generateTermGenerator ds (is, bs, ts) (type1 :->: type2) size =
  do x  <- generateName ts
     t0 <- generateTermGenerator ds (is, (x, type1) : filter ((/=x) . fst) bs, ts) type2 (size - 1)
     return $ Lambda x t0 (type1 :->: type2)
generateTermGenerator ds s (Algebraic d) size =
  oneof (map ctrGen (constructors ds d))
  where
    ctrGen (TypeConstructor c types) =
      do ts <- mapM (\tau -> generateTermGenerator ds s tau (size `div` length types)) types
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
    , do type1 <- generateType is bindingTypes
         type2 <- generateType is bindingTypes
         return $ type1 :->: type2
    ] ++ (return . Variable' . fst <$> is)
      ++ map return bindingTypes
