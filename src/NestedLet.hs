module NestedLet where

import KNormal (Exp(..), Var(..))
import Control.Monad.State
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map

expToNonNest e = if e == expToNonNest' e then e else expToNonNest (expToNonNest' e)

expToNonNest' :: Exp -> Exp
expToNonNest' (ELet x e1 e2) =
  let e1' = expToNonNest' e1 in
  let lets = extractLets e1' in
  let body = extractBody e1' in
  lets (ELet x body (expToNonNest' e2))
expToNonNest' (EIf e1 e2 e3) = (EIf (expToNonNest' e1) (expToNonNest' e2) (expToNonNest' e3))
expToNonNest' (ERec x ys e1 e2) = (ERec x ys (expToNonNest' e1) (expToNonNest' e2))
expToNonNest' (EApp e1 e2) = (EApp (expToNonNest' e1) (map expToNonNest' e2))
expToNonNest' e = e

extractLets :: Exp -> (Exp -> Exp)
extractLets (ELet x e1 e2) = (\e -> (ELet x e1 e))
extractLets _ = id

extractBody :: Exp -> Exp
extractBody (ELet x e1 e2) = e2
extractBody e = e
