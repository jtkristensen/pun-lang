
module PropertyChecker where

import Syntax
import TypeInference

import Test.Tasty.QuickCheck

type Generator      = Gen (Term Type)
type CurrentIndices = Substitution

generateGenerator :: CurrentIndices -> (Type -> Generator)
generateGenerator _ Integer'           = flip Number  Integer' <$> arbitrary
generateGenerator _ Boolean'           = flip Boolean Boolean' <$> arbitrary
generateGenerator s (Variable' index)  = generateGenerator s (resolve index s)
-- generateGenerator s (type1 :*: type2)  = undefined
-- generateGenerator s (type1 :->: type2) = undefined
generateGenerator _ _ = undefined

-- Todo, give it a better name {^o^}!
resolve :: Index -> CurrentIndices -> Type
resolve i cs =
  case lookup i cs of
    Just s  -> s
    Nothing -> error $ "Unable to resolve index to type. Index "
      ++ show i ++ " was not among current indices"

generateType :: CurrentIndices -> Gen Type
generateType cs =
  oneof $ [ return Integer'
          , return Boolean'
          , do type1 <- generateType cs
               type2 <- generateType cs
               return $ type1 :*: type2
          , do type1 <- generateType cs
               type2 <- generateType cs
               return $ type1 :->: type2
          ] ++ (return . Variable' . fst <$> cs)

-- Check takes the components of a property, and returns a generator for terms of type `Boolean'` that we can evaluate inside of QuickCheck.
check :: [(Name, Type)] -> Term Type -> Gen [(Name, Term Type)]
check _ _ = undefined
