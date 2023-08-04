
module PropertyChecker where

import Syntax
import TypeInference

import Test.Tasty.QuickCheck

type Generator = Gen (Term Type)

generateGenerators :: Program Type -> [Generator]
generateGenerators _ = undefined

generateGenerator :: Type -> Gen (Term Type, Substitution)
generateGenerator Integer' = flip Number  Integer' <$> arbitrary
generateGenerator Boolean' = flip Boolean Boolean' <$> arbitrary
generateGenerator (Variable' index) = return $ Variable (show index) (annotation inferredTermType)
  where
    -- Joachim: Are you shure this is what you want to compute?
    (inferredTermType, _, _) = infer (Variable (show index) index) index
generateGenerator _ = undefined

-- Check takes the components of a property, and returns a generator for terms of type `Boolean'` that we can evaluate inside of QuickCheck.
check :: [(Name, Type)] -> Term Type -> Gen [(Name, Term Type)]
check _ _ = undefined
