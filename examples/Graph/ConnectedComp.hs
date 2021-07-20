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
type DPConnComp = Source (Channel (Edge :<+> ConnectedComponents :<+> Eof))
                :=> Generator (Channel (Edge :<+> ConnectedComponents :<+> Eof))
                :=> Sink

source' :: FilePath
        -> Stage
           (WriteChannel Edge -> WriteChannel ConnectedComponents -> IO ())
source' filePath = withSource @DPConnComp
  $ \edgeOut _ -> unfoldFile filePath edgeOut (toEdge . decodeUtf8)

sink' :: Stage (ReadChannel Edge -> ReadChannel ConnectedComponents -> IO ())
sink' = withSink @DPConnComp $ \_ cc -> foldM_ cc print

generator' :: GeneratorStage DPConnComp ConnectedComponents Edge
generator' =
  let gen = withGenerator @DPConnComp genAction
  in  mkGenerator gen filterTemplate

filterTemplate :: Filter DPConnComp ConnectedComponents Edge
filterTemplate = actor actor1 |>> actor actor2

actor1 :: IORef ConnectedComponents
       -> Edge
       -> ReadChannel Edge
       -> ReadChannel ConnectedComponents
       -> WriteChannel Edge
       -> WriteChannel ConnectedComponents
       -> IO ()
actor1 ref _ readEdge _ writeEdge _ = 
  foldM_ readEdge $ \e -> readIORef ref >>= doActor e
 where
  doActor v conn
    | toConnectedComp v `intersect` conn = writeIORef ref (toConnectedComp v <> conn)
    | otherwise = push v writeEdge

actor2 :: IORef ConnectedComponents
       -> Edge
       -> ReadChannel Edge
       -> ReadChannel ConnectedComponents
       -> WriteChannel Edge
       -> WriteChannel ConnectedComponents
       -> IO ()
actor2 ref _ _ readCC _ writeCC = do 
  foldWithM_ readCC pushMemory $ \e -> readIORef ref >>= doActor e

 where
   pushMemory = readIORef ref >>= flip push writeCC

   doActor cc conn
    | cc `intersect` conn = writeIORef ref (cc <> conn)
    | otherwise = push cc writeCC


genAction :: Filter DPConnComp ConnectedComponents Edge
          -> ReadChannel Edge
          -> ReadChannel ConnectedComponents
          -> WriteChannel Edge
          -> WriteChannel ConnectedComponents
          -> IO ()
genAction filter' readEdge readCC _ writeCC = do
  let unfoldFilter = mkUnfoldFilterForAll filter' toConnectedComp readEdge (readCC .*. HNil) 
  results <- unfoldF unfoldFilter
  foldM_ (hHead results) (`push` writeCC)

program :: FilePath -> IO ()
program file = runDP $ mkDP @DPConnComp (source' file) generator' sink'
