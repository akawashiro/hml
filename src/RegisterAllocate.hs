module RegisterAllocate where

import Declare
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe

allocate :: [Instruction] -> [Instruction]
allocate ists = evalState (allocate' ists) Map.empty

getRegisterName :: String -> State (Map.Map String String) String
getRegisterName r = do
  m <- get
  let s = Map.size m
  let v = if head r == '$' then r else ("$t"++show s)
  if Map.lookup r m == Nothing
  then put (Map.insert r v m) >> (return v)
  else return (fromJust (Map.lookup r m))

allocate' :: [Instruction] -> State (Map.Map String String) [Instruction]
allocate' [] = return []
allocate' (ist:ists) = case ist of
  IAdd r1 r2 r3 -> do
    r1' <- getRegisterName r1
    r2' <- getRegisterName r2
    r3' <- getRegisterName r3
    ists' <- allocate' ists
    return $ IAdd r1' r2' r3' : ists'
  IMul r1 r2 r3 -> do
    r1' <- getRegisterName r1
    r2' <- getRegisterName r2
    r3' <- getRegisterName r3
    ists' <- allocate' ists
    return $ IMul r1' r2' r3' : ists'
  IBeqz r1 f -> do
    r1' <- getRegisterName r1
    ists' <- allocate' ists
    return $ IBeqz r1' f : ists'
  IOriZ r1 v -> do
    r1' <- getRegisterName r1
    ists' <- allocate' ists
    return $ IOriZ r1' v : ists'
  IMove r1 r2 -> do
    r1' <- getRegisterName r1
    r2' <- getRegisterName r2
    ists' <- allocate' ists
    return $ IMove r1' r2' : ists'
  IPrint r1 -> do
    r1' <- getRegisterName r1
    ists' <- allocate' ists
    return $ IPrint r1' : ists'
  _ -> do
    ists' <- allocate' ists
    return $ ist : ists'
