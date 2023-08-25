module PropertyCheckerTests where

import Syntax
import PropertyChecker
import TypeInference
-- import Examples

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
-- import Test.Tasty.HUnit

-- Is there a more general test case, that relies on something that commutes
-- with `infer` (Joachim)?

-- Todo, do something better than the empty substitution in : generateGenerator <mempty>.

newtype Primitive
    = Primitive (Term Type)
  deriving Show

instance Arbitrary Primitive where
  arbitrary =
    Primitive <$> oneof (generateGenerator ([], []) <$> [Integer', Boolean'])

newtype TerminatingTerm
    = TerminatingTerm (Term Type, Type)
  deriving Show

instance Arbitrary TerminatingTerm where
  arbitrary =
    do t    <- aType
       term <- generateGenerator mempty t
       return $ TerminatingTerm (term, t)

newtype SubstType = SubstType (Substitution, Type, Term Type, Type)
  deriving Show

instance Arbitrary SubstType where
  arbitrary =
    do subs  <- generateSubstitution
       t     <- generateType subs
       let canonT = refine subs t
       term  <- generateGenerator (subs, []) canonT
       return $ SubstType (subs, t, term, canonT)

newtype AcyclicIndices = AcyclicIndices CurrentIndices
  deriving Show

instance Arbitrary AcyclicIndices where
  arbitrary = 
    do length'   <- choose (0, 100)
       let indices = [(index, Variable' index') | index <- [0..length'], index' <- [index + 1]]
       canonical <- elements [Integer', Boolean']
       return $ AcyclicIndices (indices ++ [(length' + 1, canonical)])

generateGenerator_tests :: TestTree
generateGenerator_tests =
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
      all (`elem` (fst <$> subs)) (indicies t),
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
        (solution == Integer' || solution == Boolean')
  ]

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
unifiesWith (Variable' a) (Variable' b) = return [(a, Variable' b)]
unifiesWith (Variable' a) t             =
  if a `elem` indicies t then Nothing else return [(a, t)]
unifiesWith t             (Variable' b) =
  if b `elem` indicies t then Nothing else return [(b, t)]
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
subst s (Variable' a) =
  case [ t | (b, t) <- s , a == b ] of
    [ ] -> Variable' a
    [t] -> t
    _   -> error "internal error about unification"
subst s (t1 :*:  t2) = subst s t1 :*:  subst s t2
subst s (t1 :->: t2) = subst s t1 :->: subst s t2

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
