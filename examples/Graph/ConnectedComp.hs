-- |
-- Module      : Graph.ConnectedComp
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
module Graph.ConnectedComp where

import           DynamicPipeline
import           Graph.ConnComp
import           Relude

-- brittany-disable-next-binding
type DPConnComp = Input (Channel (Edge :<+> ConnectedComponents :<+> Eof))
                :>> Generator (Channel (Edge :<+> ConnectedComponents :<+> Eof))
                :>> Output

input' :: FilePath
       -> Stage
            (WriteChannel Edge -> WriteChannel ConnectedComponents -> DP st ())
input' filePath = withInput @DPConnComp
  $ \edgeOut _ -> withDP $ unfoldFile filePath edgeOut (toEdge . decodeUtf8)

output' :: Stage (ReadChannel Edge -> ReadChannel ConnectedComponents -> DP st ())
output' = withOutput @DPConnComp $ \_ cc -> withDP $ forall cc print

generator' :: GeneratorStage DPConnComp ConnectedComponents Edge st
generator' =
  let gen = withGenerator @DPConnComp genAction
  in  mkGenerator gen filterTemplate

filterTemplate :: Filter DPConnComp ConnectedComponents Edge st
filterTemplate = actor actor1 |>> actor actor2

actor1 :: Edge
       -> ReadChannel Edge
       -> ReadChannel ConnectedComponents
       -> WriteChannel Edge
       -> WriteChannel ConnectedComponents
       -> StateT ConnectedComponents (DP st) ()
actor1 _ readEdge _ writeEdge _ = 
  forall readEdge $ \e -> get >>= doActor e
 where
  doActor v conn
    | toConnectedComp v `intersect` conn = modify (toConnectedComp v <>)
    | otherwise = push v writeEdge

actor2 :: Edge
       -> ReadChannel Edge
       -> ReadChannel ConnectedComponents
       -> WriteChannel Edge
       -> WriteChannel ConnectedComponents
       -> StateT ConnectedComponents (DP st) ()
actor2 _ _ readCC _ writeCC = do 
  forall' readCC pushMemory $ \e -> get >>= doActor e

 where
   pushMemory = get >>= flip push writeCC

   doActor cc conn
    | cc `intersect` conn = modify (cc <>)
    | otherwise = push cc writeCC


genAction :: Filter DPConnComp ConnectedComponents Edge st
          -> ReadChannel Edge
          -> ReadChannel ConnectedComponents
          -> WriteChannel Edge
          -> WriteChannel ConnectedComponents
          -> DP st ()
genAction filter' readEdge readCC _ writeCC = do
  results <- unfoldFilterForAll filter'
                                toConnectedComp
                                readEdge
                                (readCC .*. HNil)
  forall (hHead results) (`push` writeCC)


program :: FilePath -> IO ()
program file = runDP $ mkDP @DPConnComp (input' file) generator' output'
