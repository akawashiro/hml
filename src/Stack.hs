module Stack where

import Declare
import Control.Monad.State
import qualified Data.Map as Map

type RegMap = Map.Map String Int

processStack :: [Instruction] -> [Instruction]
processStack ists = processStoreRestore (appendStack ists) (istListToRegMap ists)

processStoreRestore [] _ = []
processStoreRestore (ist:ists) regmap = case ist of
  IStore -> map (\(r,o) -> ISw r "$sp" o) l ++ s
  IRestore -> map (\(r,o) -> ILw r "$sp" o) l ++ s
  _ -> ist : s
  where s = processStoreRestore ists regmap
        l = Map.toList regmap

istListToRegMap :: [Instruction] -> RegMap
istListToRegMap ists = execState (istListToRegMap' ists) Map.empty

addRegToMap :: String -> State RegMap ()
addRegToMap r = do
  m <- get
  if Map.lookup r m == Nothing && take 2 r /= "$v"
  then put (Map.insert r ((Map.size m)*4) m) >> return ()
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

appendStack ists = head ists' : [IOriZ "$t0" s,IAdd "$sp" "$sp" "$t0"] ++ (tail ists')
  where s = Map.size (istListToRegMap ists) * 4
        ists' = reverse ((head (reverse ists)) : [IAdd "$sp" "$sp" "$t0",IOriZ "$t0" (-1*s)] ++ (tail (reverse ists)))

