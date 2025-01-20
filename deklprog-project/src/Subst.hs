{-# LANGUAGE TemplateHaskell #-}

module Subst
  ( Subst, -- don't export the constructor of the data type!
    domain,
    empty,
    single,
    -- compose,
    apply,
    -- restrictTo,
    testSubst,
    isEmpty,
    -- testSubst,
  )
where

import Base.Type
import Data.List (intercalate, nub, sort)
import Test.QuickCheck
import Vars
-- import Text.XHtml (base)



-- Restrict a substitution to a given set of variables
restrictTo :: Subst -> [VarName] -> Subst
restrictTo (Subst vts) vs = Subst [(x, t) | (x, t) <- vts, x `elem` vs]
-- Data type for substitutions
data Subst = Subst [(VarName, Term)]
  deriving (Show)

-- Generator for substitutions
instance Arbitrary Subst where
  -- We use the `suchThat` combinator to filter out substitutions that are not valid,
  -- i.e. whose domain contains the same variable more than once.
  arbitrary = Subst <$> (arbitrary `suchThat` ((\vts -> length vts == length (nub vts)) . map fst))


{- -- Pretty printing of substitutions
instance Pretty Subst where
  pretty (Subst vts) = '{' : intercalate ", " (map prettyVt vts) ++ "}"
    where
      prettyVt (x, t) = unwords [pretty (Var x), "->", pretty t] -}

-- All variables occuring in substitutions
instance Vars Subst where
  allVars (Subst vts) = nub (vs ++ concatMap allVars ts)
    where
      (vs, ts) = unzip vts
-- Properties

-----------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                      Aufgaben                                                                   --
-----------------------------------------------------------------------------------------------------------------------------------------------------

-- Definieren Sie eine Funktion domain :: Subst -> [VarName], die den Definitionsbereich einer Substitution zurückgibt, wobei wir domain
-- so definieren, dass nur die Variablen enthalten sind, die durch die Substitution verändert werden.
domain :: Subst -> [VarName]
domain (Subst vts) = dom vts where
  dom [] = []
  dom (s:ss)  | check (fst s) (snd s) = [fst s] ++ dom ss
              | otherwise             = dom ss
    where 
      check (VarName fromVarName) (Var (VarName toVarName)) = fromVarName /= toVarName -- Wenn sich nach der Subtitution nichts änder (gleic VarName) nicht zur Liste hinzufügen
      check _ _ = True  


empty :: Subst -- Erstellt eine leere Subtitution
empty = Subst []

single :: VarName -> Term -> Subst -- zum Erstellen einer Substitution, die lediglich eine einzelne Variable auf einen Term abbildet.
single varname term = if term == Var varname
                      then Subst []
                      else Subst [(varname, term)]

isEmpty :: Subst -> Bool -- zum Prüfen, ob der Definitionsbereich (domain) einer Substitution leer ist.
isEmpty subst = if domain subst == [] then True else False


{- apply :: Subst -> Term -> Term -- zum Anwenden einer Substitution auf ein Term.
apply (Subst vts) (Var varname) = applyRules vts varname
  where
    applyRules [] varname'  = Var varname'
    applyRules (x:xs) varname' | fst x == varname'  = snd x
                               | otherwise          = applyRules xs
apply (Subst vts) (Comb combName terms) = Comb combName (map (apply (Subst vts)) terms) -}
apply :: Subst -> Term -> Term -- zum Anwenden einer Substitution auf ein Term.
apply (Subst vts) (Var varname) = applyRules vts
  where
    applyRules [] = Var varname
    applyRules ((vn, t) : xs)
      | vn == varname = t
      | otherwise = applyRules xs
apply (Subst vts) (Comb combName terms) =
    Comb combName (map (apply (Subst vts)) terms)


-- combine two substitutions into a single new substitution
-- s = {X_1 ->t_1, X_2->t_2,...X_k->t_k}
-- t = {Y_1 ->u_1, Y_2->u_2,...Y_k->u_k}
--t o s := {X_i -> t(t_i) | X_i -> t_i \in s \land X_i \neq t(t_i)} \cup {Y_j -> u_j \in t | Y_j \notin dom(s)}
compose :: Subst -> Subst -> Subst
compose (Subst a) (Subst []) = Subst a
compose (Subst []) (Subst b) = Subst b
compose s1 s2 = Subst [(Vn1, apply s1 tn1) | (Vn1, tn1) <-  ] ++ [(Vn4, ts4) |  ]


-----------------------------------------------------------------------------------------------------------------------------------------------------
--                                                                      Tests                                                                   --
-----------------------------------------------------------------------------------------------------------------------------------------------------
-- TODO: Auskommentierte Tests müssen wieder aktiviert werden, wenn alle erforderlichen Funktionen implementiert sind!!


--{- Uncomment this to test the properties when all required functions are implemented

-- Applying the empty substitution to a term should not change the term
prop_1 :: Term -> Bool
prop_1 t = apply empty t == t

-- Applying a singleton substitution {X -> t} to X should return t
prop_2 :: VarName -> Term -> Bool
prop_2 x t = apply (single x t) (Var x) == t

-- Applying a composed substitution is equal to applying the two substitutions individually
prop_3 :: Term -> Subst -> Subst -> Bool
prop_3 t s1 s2 = apply (compose s1 s2) t == apply s1 (apply s2 t)

-- The domain of the empty substitution is empty
prop_4 :: Bool
prop_4 = null (domain empty)

-- The domain of a singleton substitution {X -> X} is empty
prop_5 :: VarName -> Bool
prop_5 x = null (domain (single x (Var x)))

-- The domain of a singleton substitution {X -> t} is [X]
prop_6 :: VarName -> Term -> Property
prop_6 x t = t /= Var x ==> domain (single x t) == [x]

-- -- The domain of a composed substitution is the union of the domains of the two substitutions
prop_7 :: Subst -> Subst -> Bool
prop_7 s1 s2 = all (`elem` (domain s1 ++ domain s2)) (domain (compose s1 s2))

-- -- The domain of a composed substitution does not contain variables that are mapped to themselves
prop_8 :: VarName -> VarName -> Property
prop_8 x1 x2 =
  x1
    /= x2
    ==> domain (compose (single x2 (Var x1)) (single x1 (Var x2)))
    == [x2]

-- The empty substitution does not contain any variables
prop_9 :: Bool
prop_9 = null (allVars empty)

-- The singleton substitution should not map a variable to itself
prop_10 :: VarName -> Bool
prop_10 x = null (allVars (single x (Var x)))

-- The variables occuring in a subsitution should be taken from both components of the individual substitutions
prop_11 :: VarName -> Term -> Property
prop_11 x t =
  t
    /= Var x
    ==> sort (nub (allVars (single x t)))
    == sort (nub (x : allVars t))

-- -- The variables occuring in a composed substitution are a subset of the variables occuring in the two substitutions
-- prop_12 :: Subst -> Subst -> Bool
-- prop_12 s1 s2 =
--   all (`elem` (allVars s1 ++ allVars s2)) (allVars (compose s1 s2))

-- -- The composed subsitution should contain the left substitution unless its variables are mapped by the right substitution
-- prop_13 :: VarName -> VarName -> Property
-- prop_13 x1 x2 =
--   x1
--     /= x2
--     ==> sort (allVars (compose (single x2 (Var x1)) (single x1 (Var x2))))
--     == sort [x1, x2]

-- The domain of a substitution is a subset of all its variables
prop_14 :: Subst -> Bool
prop_14 s = all (`elem` allVars s) (domain s)

-- Restricting the empty substitution to an arbitrary set of variables should return the empty substitution
prop_15 :: [VarName] -> Bool
prop_15 xs = null (domain (restrictTo empty xs))

-- The domain of a restricted substitution is a subset of the given set of variables
prop_16 :: [VarName] -> Subst -> Bool
prop_16 xs s = all (`elem` xs) (domain (restrictTo s xs))

-- The empty substitution is empty
prop_17 :: Subst -> Bool
prop_17 s = isEmpty empty

return []

-- Run all tests
testSubst :: IO Bool
testSubst = $(quickCheckAll)

-- -}

