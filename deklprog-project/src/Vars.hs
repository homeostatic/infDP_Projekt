module Vars
  ( testVars,
    Vars (allVars),
    freshVars
  )
where

import Test.QuickCheck
import Base.Type        


-- {-

spec :: Int -> Property
spec n = (n >= 0) && (n <= 100) ==> freshVars !! (n * 26)  == VarName ("A" ++ str)
  where str = if n == 0 then "" else show (n - 1)

-- Run tests
testVars :: IO ()
testVars = quickCheck spec


-- -}

class Vars a where
  allVars :: a -> [VarName]

instance Vars Term where
  allVars (Var n) = [n]
  allVars (Comb _ t) = allNames t where
    allNames [] = []
    allNames (te:tes) = (allVars te) ++ allNames tes

instance Vars Rule where
  allVars (Rule t ts) = allVars t ++ allNames ts where
    allNames tes = concatMap allVars tes

instance Vars Prog where
  allVars (Prog rs) = concatMap allVars rs

instance Vars Goal where
  allVars (Goal ts) = concatMap allVars ts

-- allCombinations :: [a] -> [[a]] --Kopiert aus der einen Hausaufgabe (Weis jetzt nich mehr welche)
-- allCombinations [] = [[]]
-- allCombinations a = [] : combine [[]] a where
--   combine xall a =  dings ++ combine dings a  where
--     dings = concatMap (\ x -> zipWith (\ y z -> y:z) (repeat x) xall) a

-- lCondition :: [Char] -> Bool --Bedingung, dass der Name mit einem Großbuchstaben beginnt
-- lCondition [] = False
-- lCondition (x:xs) = elem x ['A'..'Z']

-- freshVars' :: [VarName]
-- freshVars' = [VarName (xs) | xs <- allCombinations(['0'..'9']++['A'..'Z']++['a'..'z']++['_']), lCondition xs] --testVars failed


-- freshVars'' :: [VarName]
-- freshVars'' = [VarName (x:xs) | x <- ['A'..'Z'], VarName xs <- [ xs | xs <- freshVars]]


--Variables begin with a capital letter, followed by any number of digits.
freshVars :: [VarName]
freshVars = [VarName [y] | y <- ['A'..'Z']]++concatMap (\n -> [VarName (x:show n) | x <- ['A'..'Z']]) [0..]
