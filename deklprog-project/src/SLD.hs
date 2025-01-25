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

-- Data type for an SLD tree
data SLDTree = SLDTree Goal [(Subst, SLDTree)] --Goal [Term] --Unifikation?
  deriving (Show)


--prolog arbeitet goals L->R ab
--versuch unifikation von g mit alle rules
    -- wenn unifikation != Nothing -> weitermachen
    -- wenn unifikation == Nothing -> deadend  

sld :: Prog -> Goal -> SLDTree
sld _ (Goal []) = SLDTree (Goal []) []  --"no further branches"
--                                                        this might need to be a list of results, currently we only look at the first rule

sld (Prog (rl:rls)) (Goal (g:gs)) = SLDTree (Goal g:gs) [(sub, (sld (Prog (rl:rls)) (Goal (g':gs))))] where
                                        sub = unify g rl --need to handle (the "CASE" of?) unify returning 'Nothing'
                                        g' = apply sub g --updated goal applying the substitution  


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
