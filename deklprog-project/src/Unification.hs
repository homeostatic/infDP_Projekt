module Unification
  ( testUnification,
  unify
  ) where


import Data.Maybe
import Subst
import Vars
import Base.Type


import Test.QuickCheck ( quickCheckAll, (==>), Property )

-- Does  a variable occur in a term?
occurs :: VarName -> Term -> Bool
occurs v t = v `elem` allVars t

-- Disagreementset -> zipWith - Funktion
ds :: Term -> Term -> Maybe(Term, Term) 
ds (Var v0) (Var v1) = if v0 == v1 then Nothing else Just( Var v0, Var v1)
ds t@(Comb c0 ts) (Var v1) = Just( Var v1, t)
ds (Var v0) t@(Comb c1 ts) = Just(Var v0, t) 
ds t0@(Comb c0 []) t1@(Comb c1 []) = if c0 == c1 then Nothing else Just(t0,t1)
ds t0@(Comb c0 ts0) t1@(Comb c1 ts1) = if ((c0 == c1) && length ts0 == length ts1) then let nds = (filter (/= Nothing) (map (\(x0, x1) -> ds x0 x1) (zip ts0 ts1))) in 
  (if nds == [] then Nothing else head nds)
  else Just(t0, t1)

-- Unifikation
unify :: Term -> Term -> Maybe Subst 
unify t0 t1 = unify' empty t0 t1 
  where 
    unify' s t0' t1' = case (ds (apply s t0') (apply s t1')) of 
      Nothing -> Just s 
      Just(Var x, t) | not(occurs x t) -> unify' (compose (single x t) s) t0' t1' 
      _ -> Nothing



-- Properties



-- Properties

-- The disagreement set of a term with itself is empty
prop_1 :: Term -> Bool
prop_1 t = isNothing (ds t t)
    
-- The disagreement set of two different terms is not empty
prop_2 :: Term -> Term -> Property
prop_2 t1 t2 = isJust (ds t1 t2) ==> t1 /= t2

-- If a variable v occurs in a term t (other than the variable itself),
-- then the unification of v and t should fail due to the occur check
prop_3 :: VarName -> Term -> Property
prop_3 v t = occurs v t && t /= Var v ==> isNothing (unify (Var v) t)

-- If two terms t1 and t2 are unifiable, then the disagreement set of the mgu
-- applied to t1 and t2 is empty
prop_4 :: Term -> Term -> Property
prop_4 t1 t2 =
  let mMgu = unify t1 t2
  in isJust mMgu ==> let mgu = fromJust mMgu
                     in isNothing (ds (apply mgu t1) (apply mgu t2))

return []

-- Run all tests
testUnification :: IO Bool
testUnification = $(quickCheckAll)