module Stack where

import Declare
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map

type RegMap = Map.Map String Int

numberOftReg = 10

processStack :: [Instruction] -> [Instruction]
processStack ists = processSpilledReg (spilledRegMap ists) (processStoreRestore (normalRegMap ists) (appendStack ists))

processSpilledReg :: RegMap -> [Instruction] -> [Instruction]
processSpilledReg regmap [] = []
processSpilledReg regmap (ist:ists) = case ist of
  IAdd rd rs rt -> (lwrs rs) ++ (lwrt rt) ++ [IAdd (rd' rd) (rs' rs) (rt' rt)] ++ (swrd rd) ++ rest
  IMul rd rs rt -> (lwrs rs) ++ (lwrt rt) ++ [IMul (rd' rd) (rs' rs) (rt' rt)] ++ (swrd rd) ++ rest
  IBeqz rs f    -> (lwrs rs) ++ [IBeqz (rs' rs) f] ++ rest
  IOriZ rd i    -> [IOriZ (rd' rd) i] ++ (swrd rd) ++ rest
  IMove rd rs   -> (lwrs rs) ++ [IMove (rd' rd) (rs' rs)] ++ (swrd rd) ++ rest
  IJR rs        -> (lwrs rs) ++ [IJR (rs' rs)] ++ rest
  _ -> ist:rest
  where rest = processSpilledReg regmap ists
        swrd r = if Map.lookup r regmap == Nothing then [] else [ISw "$v0" "$sp" (fromJust $ Map.lookup r regmap)]
        lwrs r = if Map.lookup r regmap == Nothing then [] else [ILw "$v0" "$sp" (fromJust $ Map.lookup r regmap)]
        lwrt r = if Map.lookup r regmap == Nothing then [] else [ILw "$v1" "$sp" (fromJust $ Map.lookup r regmap)]
        rd' r  = if Map.lookup r regmap == Nothing then r else "$v0"
        rs' r  = if Map.lookup r regmap == Nothing then r else "$v0"
        rt' r  = if Map.lookup r regmap == Nothing then r else "$v1"

processStoreRestore :: RegMap -> [Instruction] -> [Instruction]
processStoreRestore _ [] = []
processStoreRestore regmap (ist:ists) = case ist of
  IStore -> map (\(r,o) -> ISw r "$sp" o) l ++ s
  ISysStore -> (ISw "$a0" "$sp" (fromJust (Map.lookup "$a0" regmap))) : s
  IRestore -> map (\(r,o) -> ILw r "$sp" o) l ++ s
  ISysRestore -> (ILw "$a0" "$sp" (fromJust (Map.lookup "$a0" regmap))) : s
  _ -> ist : s
  where s = processStoreRestore regmap ists 
        l = Map.toList regmap

istListToRegMap :: [Instruction] -> RegMap
istListToRegMap ists = execState (istListToRegMap' ists) Map.empty

isSpilledReg :: String -> Bool
isSpilledReg s = s!!1=='t' && read (drop 2 s) > numberOftReg-1

spilledRegMap :: [Instruction] -> RegMap
spilledRegMap ists = Map.fromList (filter (\(r,o) -> isSpilledReg r) (Map.toList (istListToRegMap ists)))


normalRegMap :: [Instruction] -> RegMap
normalRegMap ists = Map.fromList (filter (\(r,o) -> not (isSpilledReg r)) (Map.toList (istListToRegMap ists)))

addRegToMap :: String -> State RegMap ()
addRegToMap r = do
  m <- get
  if Map.lookup r m == Nothing && take 2 r /= "$v"
  then put (Map.insert r ((Map.size m)*(4)) m) >> return ()
  else put m >> return ()

istListToRegMap' :: [Instruction] -> State (Map.Map String Int) ()
istListToRegMap' (ist:ists) = case ist of
  IAdd rd rs rt -> (mapM_ addRegToMap [rd,rs,rt]) >> r
  IMul rd rs rt -> mapM_ addRegToMap [rd,rs,rt] >> r
  IBeqz rs f -> addRegToMap rs >> r
  IOriZ rs i -> addRegToMap rs >> r
  IMove rd rs -> mapM_ addRegToMap [rd,rs] >> r
  IJR rs       -> addRegToMap rs >> r
  _ -> r
  where r = istListToRegMap' ists
istListToRegMap' [] = return ()

appendStack ists = head ists' : [IOriZ "$t0" (-1*s),IAdd "$sp" "$sp" "$t0"] ++ (tail ists')
  where s = Map.size (istListToRegMap ists) * 4
        -- ists' = reverse ((head (reverse ists)) : [IAdd "$sp" "$sp" "$t0",IOriZ "$t0" s] ++ (tail (reverse ists)))
        f (IJR r) = [IOriZ "$t0" s,IAdd "$sp" "$sp" "$t0",IJR r]
        f i@_ = [i]
        ists' = concat (map f ists)

