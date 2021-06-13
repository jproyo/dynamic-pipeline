-- |
-- Module      : Misc.RepeatedDP
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
module Misc.RepeatedDP where

import           DynamicPipeline
import           Relude

type DPExample = Input (Channel (Int :<+> Eof)) :>> Generator (Channel (Int :<+> Eof)) :>> Output

input' :: forall k (s :: k). Stage (WriteChannel Int -> DP s ())
input' = withInput @DPExample $ \cout -> unfoldT ([1 .. 1000] <> [1 .. 1000]) cout identity

generator' :: GeneratorStage DPExample (Maybe Int) Int s
generator' =
  let gen = withGenerator @DPExample genAction
  in  mkGenerator gen filterTemp

genAction :: Filter DPExample (Maybe Int) Int s 
          -> ReadChannel Int
          -> WriteChannel Int
          -> DP s ()
genAction filter' cin cout = 
  void $ spawnFilterForAll' filter' Just (`push` cout) cin HNil

filterTemp :: Filter DPExample (Maybe Int) Int s 
filterTemp = mkFilter actorRepeted

actorRepeted :: Int
             -> ReadChannel Int
             -> WriteChannel Int
             -> StateT (Maybe Int) (DP s) ()
actorRepeted i rc wc = do
  liftIO $ forall rc $ \e -> if e /= i then push e wc else pure ()

output' :: Stage (ReadChannel Int -> DP s ())
output' = withOutput @DPExample $ flip forall print

program :: IO ()
program = runDP $ mkDP @DPExample input' generator' output'
