module PropertyCheckerTests where

import Syntax
import PropertyChecker
import TypeInference
-- import Examples

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

-- Is there a more general test case, that relies on something that commutes
-- with `infer` (Joachim)?

-- Todo, do something better than the empty substitution in : generateGenerator <mempty>.

{-
Generated input that made the second test case fail:

Rec "a" (Rec "i" (Rec "e" (Variable "a" (Variable' 0)) (Variable' 0)) (Variable' 0)) (Variable' 0)
Had the type Variable' 0 but we expected the type Boolean'
-}

newtype TerminatingTerm = TerminatingTerm (Term Type, Type)

instance Arbitrary TerminatingTerm where
  arbitrary =
    do t    <- aType
       term <- generateGenerator mempty t
       return $ TerminatingTerm (term, t)

generateGenerator_tests :: TestTree
generateGenerator_tests =
  testGroup "`generateGenerator` tests :"
    [ testCase "Pairs and Functions are not primitives" $
      do subs           <- generate $ generateSubstitution
         generatedValue <- generate $ oneof $ generateGenerator (subs, []) <$> [Integer', Boolean']
         case generatedValue of
           (Pair    _ _ _) -> False
           (Lambda  _ _ _) -> False
           _               -> True
           @? (show generatedValue ++ " should have been a primitive {^_^}")
    -- Todo: "now, it is `generateGenerator s t`, and we need to say something in the paper about `s`.
    , testCase "generateGenerator t has type Term t forall t." $
      do TerminatingTerm (term, t) <- generate arbitrary
         let (t', _, cs) = infer term 0
         let subs        = bindings cs
         let typeOfT'    = annotation (refine subs <$> t')
         (equivalent t typeOfT')
           @? ( "the term " ++ show t' ++
                " had the type " ++ show typeOfT' ++
                " but we expected the type " ++ show t
              )
    -- Nice to have.
    -- , testCase "Only valid types are generated from a substitution." $
    --   do subs <- generate $ generateSubstitution
    --      t    <- generate $ generateType subs
    --      let t' = refine subs t
    --      (elem t' $ map snd subs)
    --        @? ( "the type " ++ show t ++ " was not present in the generated substitution" )
    , testCase "generateGenerator generates appropriate type for generated substitution." $
      do subs <- generate $ generateSubstitution
         t    <- generate $ generateType subs
         let canonT = refine subs t
         term <- generate $ generateGenerator (subs, []) canonT
         let (t', _, cs) = infer term 0
         let subs'       = bindings cs
         let typeOfT'    = annotation (refine subs' <$> t')
         (equivalent canonT typeOfT')
           @? ( "the term " ++ show t' ++
                " had the type " ++ show typeOfT' ++
                " but we expected the type " ++ show canonT
              )
    -- Todo: move this, create better test case
    , testCase "Resolve resolves 'chains' of variables" $
      resolve 2 [(0, Integer'), (2, Boolean'), (1, Variable' 2)] @?= Boolean'
    ]

-- Todo, we want to use more complicated types.
-- Todo, should this actually live in `PropertyChecker`?
-- Todo, Where do we get the index for Variable' from?
aType :: Gen Type
aType =
  oneof $
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
  if current < (toInteger size)
    then do
      t      <- oneof $ [ sized (newType current), newVariable current ]
      rest   <- generateSizedSubstitution (current + 1) size
      return $ (current, t) : rest
    else do
      t      <- sized (newType current)
      return $ [(current, t)]

newType :: Index -> Int -> Gen Type
newType i size
  | i == (toInteger size) = oneof $ [ return Integer', return Boolean' ]
newType i size =
  oneof $
    [ return Integer'
    , return Boolean'
    , ( :*:  ) <$> (newType (i + 1) size) <*> (newType (i + 2) size)
    , ( :->: ) <$> (newType (i + 1) size) <*> (newType (i + 2) size)
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
    [ ] -> (Variable' a)
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
