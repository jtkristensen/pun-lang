module GeneratorGeneratorTests where

import Syntax
import GeneratorGenerator
import TypeInference

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

newtype Primitive
    = Primitive (Term Type)
  deriving Show

instance Arbitrary Primitive where
  arbitrary =
    Primitive <$> oneof (generateGenerator [] ([], [], []) <$> [Integer', Boolean'])

newtype TerminatingTerm
    = TerminatingTerm (Term Type, Type)
  deriving Show

instance Arbitrary TerminatingTerm where
  arbitrary =
    do t    <- aType
       term <- generateGenerator mempty mempty t
       return $ TerminatingTerm (term, t)

newtype SubstType = SubstType (Substitution, Type, Term Type, Type)
  deriving Show

instance Arbitrary SubstType where
  arbitrary =
    do subs  <- generateSubstitution
       t     <- generateType subs []
       let canonT = refine subs t
       term  <- generateGenerator mempty (subs, [], []) canonT
       return $ SubstType (subs, t, term, canonT)

newtype AcyclicIndices = AcyclicIndices LocalIndices
  deriving Show

instance Arbitrary AcyclicIndices where
  arbitrary =
    do length'       <- choose (0, 100)
       indices       <- shuffle ([(index, Variable' index') | index <- [0..length'], index' <- [index + 1]])
       canonicalType <- elements [Integer', Boolean']
       return $ AcyclicIndices (indices ++ [(length' + 1, canonicalType)])

newtype AnalyseGenerator = AnalyseGenerator (Term Type, ([String], [String]))
  deriving Show

instance Arbitrary AnalyseGenerator where
  arbitrary =
    do t    <- aType
       term <- generateGenerator mempty mempty t
       return $ AnalyseGenerator (term, analyse term)

generateGeneratorTests :: TestTree
generateGeneratorTests =
  testGroup "`generateGenerator` tests :"
  [ testProperty "Pairs and Functions are not primitives" $
    \(Primitive value) ->
      case value of
        Pair    {} -> False
        Lambda  {} -> False
        _          -> True,
    testProperty "generateGenerator t has type Term t forall t." $ whenFail (print "hello") $
    \(TerminatingTerm (term, t)) ->
      let (t', _, cs) = infer term 0
          subs        = bindings cs
          typeOfT'    = annotation (refine subs <$> t')
      in
        equivalent t typeOfT',
    testProperty "Only valid types are generated from a substitution." $
    \(SubstType (subs, t, _, _)) ->
      all (`elem` (fst <$> subs)) (indices t),
    testProperty "generateGenerator generates appropriate type for generated substitution." $
    \(SubstType (_, _, term, canonT)) ->
      let (t', _, cs) = infer term 0
          subs'       = bindings cs
          typeOfT'    = annotation (refine subs' <$> t')
      in
        equivalent canonT typeOfT',
    testProperty "Resolve resolves 'chains' of variables" $
    \(AcyclicIndices indices) ->
      let solution = resolve 0 indices
      in
        (solution == Integer' || solution == Boolean'),
    testProperty "Analysing terms that have been generated" $
    \(AnalyseGenerator (_, (declaredNames, usedNames))) ->
      label ("analyse names: " ++ occurrence declaredNames usedNames) True
  ]

occurrence :: [String] -> [String] -> String
occurrence declaredNames usedNames
  | null declaredNames = "No names were declared."
  | null usedNames     = "There were declared names but no occurrences"
  | otherwise          = "There were occurrences of declared names."

combine :: ([String], [String]) -> ([String], [String]) -> ([String], [String])
combine (d1, u1) (d2, u2) = (d1 ++ d2, u1 ++ u2)

analyse :: Term Type -> ([String], [String])
analyse (Number        _ _)     = (mempty, mempty)
analyse (Boolean       _ _)     = (mempty, mempty)
analyse (Unit            _)     = (mempty, mempty)
analyse (Variable      n _)     = (mempty, return n)
analyse (If   cond t1 t2 _)     = combine (combine (analyse cond) (analyse t1)) (analyse t2)
analyse (Plus      t1 t2 _)     = combine (analyse t1) (analyse t2)
analyse (Leq       t1 t2 _)     = combine (analyse t1) (analyse t2)
analyse (Pair      t1 t2 _)     = combine (analyse t1) (analyse t2)
analyse (Fst       t     _)     = analyse t
analyse (Snd       t     _)     = analyse t
analyse (Lambda    n  t  _)     = combine (return n, mempty) (analyse t)
analyse (Application t1 t2 _)   = combine (analyse t1) (analyse t2)
analyse (Let     n t1 t2 _)     = combine (return n, mempty) (combine (analyse t1) (analyse t2))
analyse (Rec       n  t  _)     = combine (return n, mempty) (analyse t)
analyse (Leaf            _)     = (mempty, mempty)
analyse (Node    l k v r _)     = (analyse k `combine` analyse v) `combine`
                                  (analyse l `combine` analyse r)
analyse (Case t l (p, n) _)     = (analyse t `combine` analyse l) `combine`
                                  (analyse p `combine` analyse n)

-- Todo, we want to use more complicated types.
-- Todo, should this actually live in `PropertyChecker`?
-- Todo, Where do we get the index for Variable' from?
aType :: Gen Type
aType =
  oneof
    [ return Integer'
    , return Boolean'
    , ( :*:  ) <$> aType <*> aType
    , ( :->: ) <$> aType <*> aType
    -- What do we do about variable here ??
    ]

generateSubstitution :: Gen Substitution
generateSubstitution = sized (generateSizedSubstitution 0)

generateSizedSubstitution :: Index -> Int -> Gen Substitution
generateSizedSubstitution current size =
  if current < toInteger size
    then do
      t      <- oneof [ sized (newType current), newVariable current ]
      rest   <- generateSizedSubstitution (current + 1) size
      return $ (current, t) : rest
    else do
      t      <- sized (newType current)
      return [(current, t)]

newType :: Index -> Int -> Gen Type
newType i size
  | i == toInteger size = oneof [ return Integer', return Boolean' ]
newType i size =
  oneof
    [ return Integer'
    , return Boolean'
    , ( :*:  ) <$> newType (i + 1) size <*> newType (i + 2) size
    , ( :->: ) <$> newType (i + 1) size <*> newType (i + 2) size
    ]

newVariable :: Index -> Gen Type
newVariable i = return $ Variable' (i + 1)

unifiesWith :: Type -> Type -> Maybe Substitution
unifiesWith Integer' Integer' = return []
unifiesWith Boolean' Boolean' = return []
unifiesWith Unit'    Unit'    = return []
unifiesWith (Variable' a) (Variable' b) = return [(a, Variable' b)]
unifiesWith (Variable' a) t             =
  if a `elem` indices t then Nothing else return [(a, t)]
unifiesWith t             (Variable' b) =
  if b `elem` indices t then Nothing else return [(b, t)]
unifiesWith (t1 :*: t2)  (ta :*: tb) =
  do a1 <-          t1 `unifiesWith` ta
     b2 <- subst a1 t2 `unifiesWith` subst a1 tb
     return $ a1 <> b2
unifiesWith (t1 :->: t2)  (ta :->: tb) =
  do a1 <-          t1 `unifiesWith` ta
     b2 <- subst a1 t2 `unifiesWith` subst a1 tb
     return $ a1 <> b2
unifiesWith _ _ = Nothing

subst :: Substitution -> Type -> Type
subst _ Integer'      = Integer'
subst _ Boolean'      = Boolean'
subst _ Unit'         = Unit'
subst s (Variable' a) =
  case [ t | (b, t) <- s , a == b ] of
    [ ] -> Variable' a
    [t] -> t
    _   -> error "internal error about unification"
subst s (t1 :*:  t2) = subst s t1 :*:  subst s t2
subst s (t1 :->: t2) = subst s t1 :->: subst s t2
subst s (BST  t1 t2) = BST (subst s t1) (subst s t2)

equivalent :: Type -> Type -> Bool
equivalent t1 t2 =
  case t1 `unifiesWith` t2 of
    (Just s) -> subst s t1 == subst s t2
    _        -> False

-- Think about it {~_^}.
-- instance Eq Type where
--   (==) = equivalent

-- Also an option {^o^}!
-- unify :: Type -> Type -> Maybe Substitution
-- unify t1 t2 = t1 `unifiesWith` t2