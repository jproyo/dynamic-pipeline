module Misc.RepeatedDP where

import           Data.HList
import           DynamicPipeline
import           Relude

type DPExample = Input (Channel (Int :<+> Eof)) :>> Generator (Channel (Int :<+> Eof)) :>> Output

input' :: Stage (WriteChannel Int -> IO ())
input' = withInput @DPExample @IO $ \cout -> unfoldT ([1 .. 1000] <> [1 .. 1000]) cout identity

generator' :: GeneratorStage (Maybe Int) IO DPExample Int
generator' =
  let gen = withGenerator @DPExample @(Filter (Maybe Int) IO DPExample Int) @IO $ genAction
  in  mkGenerator gen filterTemp

genAction :: Filter (Maybe Int) IO DPExample Int
          -> ReadChannel Int
          -> WriteChannel Int
          -> IO ()
genAction filter' cin cout = 
  void $ spawnFilterForAll' filter' Just (`push` cout) cin HNil

filterTemp :: Filter (Maybe Int) IO DPExample Int
filterTemp = mkFilter actorRepeted

actorRepeted :: Int
             -> ReadChannel Int
             -> WriteChannel Int
             -> StateT (Maybe Int) IO ()
actorRepeted i rc wc = do
  liftIO $ forall rc $ \e -> if e /= i then push e wc else pure ()

output' :: Stage (ReadChannel Int -> IO ())
output' = withOutput @DPExample @IO $ flip forall print

program :: IO ()
program = runDP $ mkDP @DPExample input' generator' output'
