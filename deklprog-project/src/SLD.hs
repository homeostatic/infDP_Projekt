module SLD
  ( SLDTree(..), sld, Strategy, dfs, bfs, solveWith
  ) where
-- updated with musterlösungen

import Data.Maybe

import Base.Type

import Rename
import Subst
import Unification
import Vars

-- Data type for an SLD tree
data SLDTree = SLDTree Goal [(Subst, SLDTree)]
  deriving (Show)

-- Construct the SLD tree for a given program and goal
sld :: Strategy -> Prog -> Goal -> SLDTree
sld strat p = build empty
  where
  -- Found solution (empty goal)
  build _ g'@(Goal []) = SLDTree g' []
  -- Non-built-in predicates
  build s g'@(Goal (l:ls)) = SLDTree g'
    [ (mgu, build (compose mgu s) (Goal (map (apply mgu) (b ++ ls))))
    -- 1. Renaming of the program (fresh variants)
    | let Prog rs = p, Rule h b <- map (rename (allVars g' ++ allVars s)) rs
    -- 2. mgu of the rule heads and the first literal
    , mgu <- maybeToList (unify l h)
    ]

-- Alias type for search strategies
type Strategy = SLDTree -> [Subst]

-- Depth-first search in an SLD tree
dfs :: Strategy
dfs = dfs' empty
  where dfs' sigma (SLDTree (Goal []) _)  = [sigma]
        dfs' sigma (SLDTree _         ts) =
          [phi | (theta, t') <- ts, phi <- dfs' (compose theta sigma) t']

-- Breadth-first search in an SLD tree
bfs :: Strategy
bfs t = bfs' [(empty, t)]
  where bfs' [] = []
        bfs' ts = [sigma | (sigma, SLDTree (Goal []) _) <- ts] ++
          bfs' [ (compose theta sigma, t')
               | (sigma, SLDTree _ ts') <- ts
               , (theta, t') <- ts'
               ]

-- Solve a given goal with respect to a given program using a given strategy
solveWith :: Prog -> Goal -> Strategy -> [Subst]
solveWith p g strat = map (`restrictTo` allVars g) (strat (sld strat p g))

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

g0 = Goal [Comb "p" [Var (VarName "S"), Comb "b" []]]
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

SLDTree 
  (Goal [Comb "p" [Var (VarName "S"),Comb "b" []]]) 
          [(Subst [(VarName "S",Var (VarName "X")),(VarName "Z",Comb "b" [])],
              SLDTree
                  (Goal [Comb "q" [Var (VarName "X"),Var (VarName "Y")],Comb "p" [Var (VarName "Y"),Comb "b" []]])
                      [(Subst [(VarName "X",Comb "a" []),(VarName "Y",Comb "b" [])],
                        SLDTree (Goal []) [])]),
          (Subst [(VarName "S",Comb "b" []),(VarName "X",Comb "b" [])]
              ,SLDTree (Goal []) [])]


prog = Prog [Rule (Comb "p" [Var (VarName "X"),Var (VarName "Z")]) [Comb "q" [Var (VarName "X"),Var (VarName "Y")],Comb "p" [Var (VarName "Y"),Var (VarName "Z")]],Rule (Comb "p" [Var (VarName "X"),Var (VarName "X")]) [],Rule (Comb "q" [Comb "a" [],Comb "b" []]) []]
goal = Goal [Comb "p" [Var (VarName "S"),Comb "b" []]]

expected:
SLDTree (Goal [Comb "p" [Var (VarName "S"),Comb "b" []]]) 
        [((compose (single (VarName "A") (Var (VarName "S"))) (single (VarName "B") (Comb "b" [])))
                  ,SLDTree (Goal [Comb "q" [Var (VarName "S"),Var (VarName "C")],Comb "p" [Var (VarName "C"),Comb "b" []]])
                           [((compose (single (VarName "S") (Comb "a" [])) (single (VarName "C") (Comb "b" []))),
                                      SLDTree (Goal [Comb "p" [Comb "b" [],Comb "b" []]]) 
                                              [((compose (single (VarName "D") (Comb "b" [])) (single (VarName "E") (Comb "b" []))),
                                                        SLDTree (Goal [Comb "q" [Comb "b" [],Var (VarName "F")],Comb "p" [Var (VarName "F"),Comb "b" []]]) []),
                                                                (single (VarName "D") (Comb "b" []),
                                                        SLDTree (Goal []) [])])]),(
        (compose (single (VarName "A") (Comb "b" [])) (single (VarName "S") (Comb "b" [])))
                  ,SLDTree (Goal []) [])]

current:
SLDTree (Goal [Comb "p" [Var (VarName "S"),Comb "b" []]])
         [(Subst [(VarName "S",Var (VarName "X")),(VarName "Z",Comb "b" [])]
                ,SLDTree (Goal [Comb "q" [Var (VarName "X"),Var (VarName "Y")],Comb "p" [Var (VarName "Y"),Comb "b" []]])
                         [(Subst [(VarName "X",Comb "a" []),(VarName "Y",Comb "b" [])],
                                SLDTree (Goal []) [])]),
          (Subst [(VarName "S",Comb "b" []),(VarName "X",Comb "b" [])]
                ,SLDTree (Goal []) [])]

--}




-- -- depth-first search statergy
-- dfs :: Strategy
-- dfs (SLDTree goal [(subs, slds)] ) = []

-- -- breadth-first search statergy 
-- bfs :: Strategy

-- solveWith :: Prog -> Goal -> Strategy -> [Subst]
-- solveWith p@(Prog rules) g@(Goal terms) strat = strat (sld p g)