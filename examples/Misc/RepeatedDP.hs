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

type DPExample = Source (Channel (Int :<+> Eof)) :=> Generator (Channel (Int :<+> Eof)) :=> Sink

source' :: Stage (WriteChannel Int -> DP s ())
source' = withSource @DPExample $ \cout -> unfoldT ([1 .. 1000] <> [1 .. 1000]) cout identity

generator' :: GeneratorStage DPExample (Maybe Int) Int s
generator' =
  let gen = withGenerator @DPExample genAction
  in  mkGenerator gen filterTemp

genAction :: Filter DPExample (Maybe Int) Int s 
          -> ReadChannel Int
          -> WriteChannel Int
          -> DP s ()
genAction filter' cin cout = 
  let unfoldFilter = mkUnfoldFilterForAll' (`push` cout) filter' Just cin HNil 
   in void $ unfoldF unfoldFilter

filterTemp :: Filter DPExample (Maybe Int) Int s 
filterTemp = mkFilter actorRepeted

actorRepeted :: Int
             -> ReadChannel Int
             -> WriteChannel Int
             -> StateT (Maybe Int) (DP s) ()
actorRepeted i rc wc = foldM rc $ \e -> if e /= i then push e wc else pure ()

sink' :: Stage (ReadChannel Int -> DP s ())
sink' = withSink @DPExample $ flip foldM print

program :: IO ()
program = runDP $ mkDP @DPExample source' generator' sink'
