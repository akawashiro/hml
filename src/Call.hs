module Call where

import Declare
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe

processCall :: [Instruction] -> [Instruction]
processCall (ILabel l:ists) = ILabel l:evalState (processCall' ists) Map.empty
processCall ists = (concat $ map replaceRetWithPrint (ILabel "main":ists)) ++ [IOriZ "$v0" 10,ISys]

replaceRetWithPrint (IRet r) = [ISysStore,IMove "$a0" r,IOriZ "$v0" 1,ISys,ISysRestore,IOriZ "$v0" 10,ISys]
replaceRetWithPrint (ICall f as rd) = IStore : setMove (zip argsReg as) ++ [IJal f,IRestore,IMove rd "$v0"]
replaceRetWithPrint i        = [i]

setArgs :: [String] -> State (Map.Map String String) [Instruction]
setArgs as = do
  s <- get
  let ator = map (\(x,y) -> (x, "$t" ++ show y)) (zip as [0..])
  put (foldr (\(x,y) z -> Map.insert x y z) s ator)
  return $ map (\x -> IMove ("$t" ++ show x) ("$a" ++ show x)) [0..length as]

replaceRegister :: String -> State (Map.Map String String) String
replaceRegister r = do
  s <- get
  maybe (return r) return (Map.lookup r s)

argsReg = map (\x -> "$a" ++ show x) [0..]

setMove = map (\(x,y) ->  if isFunLabel y then ILa x y else IMove x y)

isFunLabel f = take 3 f == "fun"

processCall' :: [Instruction] -> State (Map.Map String String) [Instruction]
processCall' [] = return []
processCall' (ist:ists) = case ist of
  ILabel l -> do
    ists' <- processCall' ists
    return $ ILabel l : ists'
  IArgs as -> do
    set <- setArgs as
    ists' <- processCall' ists
    return $ set ++ ists'
  ICall f as rd -> do
    as' <- mapM replaceRegister as
    f' <- replaceRegister f
    ists' <- processCall' ists
    let jf = if isFunLabel f then IJal f else IJalr f' "$ra"
    let c = IStore : setMove (zip argsReg as') ++ [jf,IRestore,IMove rd "$v0"]
    return $ c ++ ists'
  IRet r -> do
    ists' <- processCall' ists
    return $ IMove "$v0" r : IJR "$ra" : ists'
  IAdd r1 r2 r3 -> do
    r1' <- replaceRegister r1
    r2' <- replaceRegister r2
    r3' <- replaceRegister r3
    ists' <- processCall' ists
    return $ IAdd r1' r2' r3' : ists'
  IMul r1 r2 r3 -> do
    r1' <- replaceRegister r1
    r2' <- replaceRegister r2
    r3' <- replaceRegister r3
    ists' <- processCall' ists
    return $ IMul r1' r2' r3' : ists'
  ISlt r1 r2 r3 -> do
    r1' <- replaceRegister r1
    r2' <- replaceRegister r2
    r3' <- replaceRegister r3
    ists' <- processCall' ists
    return $ ISlt r1' r2' r3' : ists'
  IBeqz r1 f -> do
    r1' <- replaceRegister r1
    ists' <- processCall' ists
    return $ IBeqz r1' f : ists'
  IOriZ r v -> do
    r' <- replaceRegister r
    ists' <- processCall' ists
    return $ IOriZ r' v : ists'
  IMove r1 r2 -> do
    r1' <- replaceRegister r1
    r2' <- replaceRegister r2
    ists' <- processCall' ists
    return $ IMove r1' r2' : ists'
  IJump l -> do
    ists' <- processCall' ists
    return $ IJump l : ists'
  _ -> do
    ists' <- processCall' ists
    return $ ist:ists'
