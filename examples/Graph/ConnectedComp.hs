module Graph.ConnectedComp where

import           Data.HList
import           DynamicPipeline
import           Graph.ConnComp
import           Relude

-- brittany-disable-next-binding
type DPConnComp = Input (Channel (Edge :<+> ConnectedComponents :<+> Eof))
                :>> Generator (Channel (Edge :<+> ConnectedComponents :<+> Eof))
                :>> Output

input' :: FilePath
       -> Stage
            (WriteChannel Edge -> WriteChannel ConnectedComponents -> IO ())
input' filePath = withInput @DPConnComp @IO
  $ \edgeOut _ -> unfoldFile filePath edgeOut (toEdge . decodeUtf8)

output' :: Stage (ReadChannel Edge -> ReadChannel ConnectedComponents -> IO ())
output' = withOutput @DPConnComp @IO $ \_ cc -> forall cc print

generator' :: GeneratorStage ConnectedComponents IO DPConnComp Edge
generator' =
  let gen =
        withGenerator @DPConnComp
          @(Filter ConnectedComponents IO DPConnComp Edge)
          @IO
          $ genAction
  in  mkGenerator gen filterTemplate

filterTemplate :: Filter ConnectedComponents IO DPConnComp Edge
filterTemplate = actor actor1 |>> actor actor2

actor1 :: Edge
       -> ReadChannel Edge
       -> ReadChannel ConnectedComponents
       -> WriteChannel Edge
       -> WriteChannel ConnectedComponents
       -> StateT ConnectedComponents IO ()
actor1 _ readEdge _ writeEdge _ = 
  forall readEdge $ \e -> get >>= doActor e
 where
  doActor v conn
    | toConnectedComp v `intersect` conn = modify (toConnectedComp v <>)
    | otherwise = liftIO $ push v writeEdge

actor2 :: Edge
       -> ReadChannel Edge
       -> ReadChannel ConnectedComponents
       -> WriteChannel Edge
       -> WriteChannel ConnectedComponents
       -> StateT ConnectedComponents IO ()
actor2 _ _ readCC _ writeCC = do 
  forall' readCC pushMemory $ \e -> get >>= doActor e

 where
   pushMemory = get >>= liftIO . flip push writeCC

   doActor cc conn
    | cc `intersect` conn = modify (cc <>)
    | otherwise = push cc writeCC


genAction :: Filter ConnectedComponents IO DPConnComp Edge
          -> ReadChannel Edge
          -> ReadChannel ConnectedComponents
          -> WriteChannel Edge
          -> WriteChannel ConnectedComponents
          -> IO ()
genAction filter' readEdge readCC _ writeCC = do
  results <- spawnFilterForAll filter'
                               toConnectedComp
                               (const $ pure ())
                               readEdge
                               (readCC .*. HNil)
  forall (hHead results) (`push` writeCC)


program :: FilePath -> IO ()
program file = runDP $ mkDP @DPConnComp (input' file) generator' output'
