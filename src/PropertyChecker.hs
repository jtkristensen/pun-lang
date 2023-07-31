
module PropertyChecker where

import Syntax
import TypeInference

import Test.Tasty.QuickCheck

type Generator = Gen (Term Type)

generateGenerators :: Program Type -> [Generator]
generateGenerators _ = undefined

generateGenerator :: Type -> Gen (Term Type)
generateGenerator Integer' = do
  generatedInt <- arbitrary
  return $ Number generatedInt Integer'
generateGenerator Boolean' = do
  generatedBool <- arbitrary
  return $ Boolean generatedBool Boolean'
generateGenerator (Variable' index) =
  case infer term index of
    (inferredTermType, _, _) ->
      return $ Variable varName (annotation inferredTermType)
  where
    varName = show index
    term    = Variable varName index
generateGenerator (type1 :*: type2) = do
  arbitraryTerm1 <- generateGenerator type1
  arbitraryTerm2 <- generateGenerator type2
  return $ Pair arbitraryTerm1 arbitraryTerm2 (type1 :*: type2)
generateGenerator (type1 :->: type2) = do
  arbitraryTerm1 <- generateGenerator type1
  arbitraryTerm2 <- generateGenerator type2
  return $ Application arbitraryTerm1 arbitraryTerm2 (type1 :->: type2)

-- Check takes the components of a property, and returns a generator for terms of type `Boolean'` that we can evaluate inside of QuickCheck.
check :: [(Name, Type)] -> Term Type -> Gen [(Name, Term Type)]
check _ _ = undefined
