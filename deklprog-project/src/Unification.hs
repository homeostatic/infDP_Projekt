{-# LANGUAGE TemplateHaskell #-}

module Unification
  ( -- testUnification
  , -- unify
  ) where


import Data.Maybe

import Test.QuickCheck

--helper function
-- firstJust


-- die die Unstimmigkeitsmenge zweier Terme berechnet und sie als Paar zurückgibt.
-- 
-- Sollte die Unstimmigkeitsmenge leer sein, soll die Funktion stattdessen Nothing zurückgeben.
-- ds :: Term -> Term -> Maybe (Term, Term)
-- --Var mit Var
-- ds (Var t1) (Var t2)      | t1 == t2   = Nothing
--                           |otherwise   = Just (Var n1,Var n)
-- --Var mit Comb
-- ds (Comb c1 ts)@t1 (Var v2)            = Just (Var v2, t1)
-- ds (Var v1) (Comb c2 ts)@t2            = Just (Var v1, t2)
-- --Comb mit Comb
-- ds (Comb c0 [])@t0 (Comb c1 [])@t1    = if c0==c1 then Nothing else Just(t0,t1)
-- ds (Comb c0 ts0)@t0 (Comb c1 ts1)@t1  = if c0 == c1 and length(ts0) == length(ts1) 
--                                             then first(filter (\= Nothing (concatMap (\(x0, x1)->ds x0 x1)(zip ts0 ts1)))
--                                             else Just(t0, t1)




--allgemeinsten Unifikator für zwei Terme bestimmt, sofern die beiden Terme unifizierbar sind.
-- Ansonsten soll die Funktion Nothing zurückgeben.
-- --                        Just [(var, term)]
-- unify :: Term -> Term -> Maybe Subst
-- -- unify (Var v0) (Var v1) = Just Subst [(VarName v0, v1)]
-- -- unify t1 t2 = ds (apply t1 t2 
-- unify t0 t1 = unify' empty t0 t1
--   where
--     unify' s t0' t1' = case (ds (apply s t0') (apply s t1')) of
--       Nothing -> Just s
--       Just(Var x, t) | not(occurs x t) = unify' (compose (single Var x t) s) t0' t1'
--       _ -> Nothing 


  
-- Properties

{- Uncomment this to test the properties when all required functions are implemented

-- Does  a variable occur in a term?
occurs :: VarName -> Term -> Bool
occurs v t = v `elem` allVars t

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
-}



