module Call where

import Declare
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe

processCall :: [Instruction] -> [Instruction]
processCall (ILabel l:ists) = ILabel l:evalState (processCall' ists) Map.empty
processCall ists = concat $ map replaceRetWithPrint (ILabel "main":ists)

replaceRetWithPrint (IRet r) = [IPrint r]
replaceRetWithPrint (ICall f as rd) = setMove (zip argsReg as) ++ [IJal f,IMove rd "$v0"]
replaceRetWithPrint i        = [i]

setArgs :: [String] -> State (Map.Map String String) ()
setArgs as = do
  s <- get
  let ator = map (\(x,y) -> (x, "$a" ++ show y)) (zip as [0..])
  put (foldr (\(x,y) z -> Map.insert x y z) s ator)

replaceRegister :: String -> State (Map.Map String String) String
replaceRegister r = do
  s <- get
  maybe (return r) return (Map.lookup r s)

argsReg = map (\x -> "$a" ++ show x) [0..]

setMove = map (uncurry IMove)

processCall' :: [Instruction] -> State (Map.Map String String) [Instruction]
processCall' [] = return []
processCall' (ist:ists) = case ist of
  ILabel l -> do
    ists' <- processCall' ists
    return $ ILabel l : ists'
  IArgs as -> do
    setArgs as
    ists' <- processCall' ists
    return ists'
  ICall f as rd ->
    return $ setMove (zip argsReg as) ++ [IJal f,IMove rd "$v0"]
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

