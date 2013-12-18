module SLIC.Front.LLifter.Equations (EqE(..), EqnSys, solveEqs) where

import Data.List as List
import Data.Set as Set
import Data.Map as Map

-- | EqV wraps known values, EqU wraps unknowns to be found
data EqE a b = 
    EqV a | EqU b
    deriving (Eq, Ord, Read, Show)
-- | The equation type
type Eqn a b = (a, Set b)
-- | The equation system type
type EqnSys a b = Map a (Set b)

-- | Get the right hand side of an equation
getRhs :: Eqn a b -> [b]
getRhs (_, s) = Set.elems s

-- | Split the rhs of an equation in two lists: known and unknown values
getRhsS :: Eqn a (EqE b c) -> ([EqE b d], [EqE e c])
getRhsS e = 
  let mark :: ([EqE a c], [EqE d b]) -> EqE a b ->  
              ([EqE a c], [EqE d b])
      mark (eqeV, eqeU) (EqV v) = ((EqV v):eqeV, eqeU)
      mark (eqeV, eqeU) (EqU u) = (eqeV, (EqU u):eqeU)
      l = getRhs e 
  in  List.foldl mark ([], []) l

-- | Take an equation `eqn' and an equation system in which *all unknowns 
--   of `eqn' are defined* and substitute the rhs of every equation in the
--   system for their lhs-s in the rhs of `eqn'.
--
--   All unknowns appearing in `eqn' but not defined in the equation
--   system are considered to have an empty rhs
subEqsFlLub ::  Ord a => Ord b => Ord c =>
  Eqn a (EqE b c) -> EqnSys c (EqE b c) -> Eqn a (EqE b c)
subEqsFlLub eqn@(n, _) eqnSys =
  let (rhsK, rhsU)    = getRhsS eqn
      unwrapU :: EqE j c -> c
      unwrapU (EqU u) = u
      unknowns        = List.map unwrapU rhsU
      f rhs n'        = Set.union 
                          (findWithDefault (Set.empty) n' eqnSys)
                          rhs
      rhs'            = List.foldl f (Set.empty) unknowns
      eqnSys'         = Map.filterWithKey 
                          (\n' _ -> not $ List.elem n' unknowns)
                          eqnSys
  in  if List.null rhsU then (n, Set.fromList rhsK)
      else subEqsFlLub (n, Set.union rhs' (Set.fromList rhsK)) eqnSys'

-- | Take an equation system and return the corresponding solved
--   equation system
solveEqs :: Ord a => Ord b =>  
            EqnSys a (EqE b a) -> EqnSys a (EqE b a)
solveEqs eqnSys = 
  Map.foldWithKey 
    (\n rhs eqnSys' -> 
        Map.insert n (snd (subEqsFlLub (n, rhs) eqnSys)) eqnSys')
    Map.empty eqnSys

