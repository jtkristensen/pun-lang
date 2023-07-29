
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
    return $ (Number generatedInt Integer')
generateGenerator Boolean' = do
    generatedBool <- arbitrary
    return $ (Boolean generatedBool Boolean')
generateGenerator (Variable' index) =
  case infer term index of
    (inferredTermType, _, _) ->
      return $ (Variable varName (getType inferredTermType))
  where
    varName = show index
    term    = Variable varName index

getType :: Term Type -> Type
getType (Number      _ Integer') = Integer'
getType (Boolean     _ Boolean') = Boolean'
getType (Variable    _ a       ) = a
getType (If          _ _ _  tau) = tau
getType (Plus        _ _    tau) = tau
getType (Leq         _ _    tau) = tau
getType (Pair        _ _    tau) = tau
getType (Fst         _      tau) = tau
getType (Snd         _      tau) = tau
getType (Lambda      _ _    tau) = tau
getType (Application _ _    tau) = tau
getType (Let         _ _ _  tau) = tau
getType (Rec         _ _    tau) = tau
getType _                        = Integer'

-- Check takes the components of a property, and returns a generator for terms of type `Boolean'` that we can evaluate inside of QuickCheck.
check :: [(Name, Type)] -> Term Type -> Gen [(Name, Term Type)]
check _ _ = undefined
