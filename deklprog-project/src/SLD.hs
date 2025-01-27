module SLD
  ( SLDTree (..),
    -- sld,
    -- Strategy,
    -- dfs,
    -- bfs,
    -- solveWith,
  )
where

import Base.Type
import Subst
import Unification


-- Data type for an SLD tree
data SLDTree = SLDTree Goal [(Subst, SLDTree)] --Goal [Term] --Unifikation?
  deriving (Show)


--prolog arbeitet goals L->R ab
--versuch unifikation von g mit head von alle rules
    -- wenn unifikation == Nothing -> deadend  
    -- wenn unifikation == Just sub -> weitermachen mit new goal == apply sub body (of rule)

sld :: Prog -> Goal -> SLDTree
sld _ (Goal []) = SLDTree (Goal []) []  --"no further branches"

sld p@(Prog rules) (Goal (g:gs)) = SLDTree (Goal (g:gs)) (concatMap tryRule rules)
  where
    tryRule (Rule head body) = case unify g head of
      Nothing -> [] --unification failed
      Just sub -> [(sub, sld p (Goal (map (apply sub) body)))]
    

{--
Example program:
p(X, Z) :- q(X, Y), p(Y, Z).
p(X, X).
q(a, b).

Example Query:
-? p(S, b).

--}


-- Example Tree
{--
Prog:
prog = Prog [
    Rule (Comb "p" [Var (VarName "X"),Var (VarName "Z")]) 
      [Comb "q" [Var (VarName "X"),Var (VarName "Y")], Comb "p" [Var (VarName "Y"),Var (VarName "Z")]],
    Rule (Comb "p" [Var (VarName "X"),Var (VarName "X")]) [],
    Rule (Comb "q" [Comb "a" [],Comb "b" []]) []]

SLDTree
├─ Goal: [Comb "p" [Var (VarName "S"), Comb "b" []]]
│  ├─ Substitution: (compose (single (VarName "A") (Var (VarName "S"))) (single (VarName "B") (Comb "b" [])))
│  │  └─ SLDTree
│  │     ├─ Goal: [Comb "q" [Var (VarName "S"), Var (VarName "C")], Comb "p" [Var (VarName "C"), Comb "b" []]]
│  │     │  ├─ Substitution: (compose (single (VarName "S") (Comb "a" [])) (single (VarName "C") (Comb "b" [])))
│  │     │  │  └─ SLDTree
│  │     │  │     ├─ Goal: [Comb "p" [Comb "b" [], Comb "b" []]]
│  │     │  │     │  ├─ Substitution: (compose (single (VarName "D") (Comb "b" [])) (single (VarName "E") (Comb "b" [])))
│  │     │  │     │  │  └─ SLDTree
│  │     │  │     │  │     ├─ Goal: [Comb "q" [Comb "b" [], Var (VarName "F")], Comb "p" [Var (VarName "F"), Comb "b" []]]
│  │     │  │     │  │     └─ No further branches
│  │     │  │     │  ├─ Substitution: (single (VarName "D") (Comb "b" []))
│  │     │  │     │  │  └─ SLDTree
│  │     │  │     │  │     ├─ Goal: []
│  │     │  │     │  │     └─ No further branches
│  │     │  │     └─ No further branches
│  │     │  └─ No further branches
│  ├─ Substitution: (compose (single (VarName "A") (Comb "b" [])) (single (VarName "S") (Comb "b" [])))
│  │  └─ SLDTree
│  │     ├─ Goal: []
│  │     └─ No further branches

--}
