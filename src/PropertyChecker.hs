
module PropertyChecker where

import Syntax
import TypeInference

import Test.Tasty.QuickCheck

type Generator = Gen (Term Type)

generateGenerators :: Program Type -> [Generator]
generateGenerators _ = undefined

getType :: Term Type -> Type
getType termType = 
    case termType of
        (Number  i Integer') -> Integer'
        (Boolean b Boolean') -> Boolean'
        _                    -> Integer'

generateGenerator :: Type -> Gen (Term Type)
generateGenerator Integer' = do
    generatedInt <- arbitrary
    return $ (Number generatedInt Integer')
generateGenerator Boolean' = do
    generatedBool <- arbitrary
    return $ (Boolean generatedBool Boolean')
generateGenerator (type1 symbol type2) = do
    arbitraryTerm1 <- generateGenerator type1
    arbitraryTerm2 <- generateGenerator type2
    case symbol of
        :*:  -> Pair arbitraryTerm1 arbitraryTerm2 (type1 :*: type2)
        :->: -> Application arbitraryTerm1 arbitraryTerm2 (type1 :->: type2)

-- Check takes the components of a property, and returns a generator for terms of type `Boolean'` that we can evaluate inside of QuickCheck.
check :: [(Name, Type)] -> Term Type -> Gen [(Name, Term Type)]
check _ _ = undefined