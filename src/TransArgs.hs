module Declare where

import Parse
import Control.Monad.State

transArgs :: [Decl] -> [Decl]
transArgs = undefined



transArgs' :: Decl -> State (Map.Map String String) Decl
transArgs' 
