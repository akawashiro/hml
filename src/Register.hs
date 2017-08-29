module Register where

import Declare
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

allocate :: [Instruction] -> [Instruction]
allocate ists = evalState (allocate' ists) (Map.empty,0)

getRegisterName :: String -> [Instruction] -> State (Map.Map String String,Int) String
getRegisterName r ists =
  if head r == '$'
  then return r
  else do
    (m,i) <- get
    let fr = firstFreeRegister (Map.toList m) (usingRegister ists)
    let v = head $ filter (\x -> Set.member x (usingRegister ists)) (map (\x -> "$t" ++ show x) [0..])
    maybe (maybe 
              (put (Map.insert r v m,i+1) >> return v)
              (\x -> put (Map.insert r x (deleteAtValue x m),i) >> return x) 
              fr)
          (\x -> return x) 
          (Map.lookup r m)

deleteAtValue :: Ord a => Eq b => b -> Map.Map a b -> Map.Map a b
deleteAtValue v m = Map.fromList (filter (\(x,y) -> y/=v) (Map.toList m))

firstFreeRegister :: [(String,String)] -> Set.Set String -> Maybe String
firstFreeRegister [] _ = Nothing
firstFreeRegister (r:rs) set = if Set.member (fst r) set || (snd r !! 1 /= 't')
                               then firstFreeRegister rs set
                               else Just (snd r)

followingInstruction :: [Instruction] -> [Instruction]
followingInstruction ists = evalState (followingInstruction' ists) Set.empty

type LabelSet = Set.Set String
followingInstruction' :: [Instruction] -> State LabelSet [Instruction]
followingInstruction' [] = return []
followingInstruction' (i:ists) = case i of
  ILabel l -> do
    s <- get
    ists' <- followingInstruction' ists
    if Set.member l s then return (i:ists') else return []
  IJump l -> do
    s <- get
    put $ Set.insert l s
    ists' <- followingInstruction' (tail ists)
    return $ (i:head ists:ists')
  _ -> do
    ists' <- followingInstruction' ists
    return $ i : ists'

usingRegister :: [Instruction] -> Set.Set String
usingRegister [] = Set.empty
usingRegister  (ist:ists) = case ist of
  IAdd r1 r2 r3 -> foldl f rest [r1,r2,r3]
  IMul r1 r2 r3 -> foldl f rest [r1,r2,r3]
  ISlt r1 r2 r3 -> foldl f rest [r1,r2,r3]
  IOriZ r1 v ->    foldl f rest [r1]
  IMove r1 r2 ->   foldl f rest [r1,r2]
  IBeqz r1 l ->    foldl f rest [r1]
  IJalr rs rt ->   foldl f rest [rs,rt]
  _ -> rest
  where rest = usingRegister ists
        f x y = Set.insert y x

allocate' :: [Instruction] -> State (Map.Map String String,Int) [Instruction]
allocate' [] = return []
allocate' (ist:ists) = case ist of
  IAdd r1 r2 r3 -> do
    r1' <- getRegisterName r1 fists
    r2' <- getRegisterName r2 fists
    r3' <- getRegisterName r3 fists
    ists' <- allocate' ists
    return $ IAdd r1' r2' r3' : ists'
  IMul r1 r2 r3 -> do
    r1' <- getRegisterName r1 fists
    r2' <- getRegisterName r2 fists
    r3' <- getRegisterName r3 fists
    ists' <- allocate' ists
    return $ IMul r1' r2' r3' : ists'
  ISlt r1 r2 r3 -> do
    r1' <- getRegisterName r1 fists
    r2' <- getRegisterName r2 fists
    r3' <- getRegisterName r3 fists
    ists' <- allocate' ists
    return $ ISlt r1' r2' r3' : ists'
  IBeqz r1 f -> do
    r1' <- getRegisterName r1 fists
    ists' <- allocate' ists
    return $ IBeqz r1' f : ists'
  IOriZ r1 v -> do
    r1' <- getRegisterName r1 fists
    ists' <- allocate' ists
    return $ IOriZ r1' v : ists'
  IMove r1 r2 -> do
    r1' <- getRegisterName r1 fists
    r2' <- getRegisterName r2 fists
    ists' <- allocate' ists
    return $ IMove r1' r2' : ists'
  IJalr rs rt -> do
    rs' <- getRegisterName rs fists
    rt' <- getRegisterName rt fists
    ists' <- allocate' ists
    return $ IJalr rs' rt' : ists'
  _ -> do
    ists' <- allocate' ists
    return $ ist : ists'
  where fists = followingInstruction (ist:ists)
