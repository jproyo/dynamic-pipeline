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

source' :: Stage (WriteChannel Int -> IO ())
source' = withSource @DPExample $ \cout -> unfoldT ([1 .. 1000] <> [1 .. 1000]) cout identity

generator' :: GeneratorStage DPExample (Maybe Int) Int
generator' =
  let gen = withGenerator @DPExample genAction
  in  mkGenerator gen filterTemp

genAction :: Filter DPExample (Maybe Int) Int
          -> ReadChannel Int
          -> WriteChannel Int
          -> IO ()
genAction filter' cin cout =
  let unfoldFilter = mkUnfoldFilterForAll' (`push` cout) filter' Just cin HNil 
   in void $ unfoldF unfoldFilter

filterTemp :: Filter DPExample (Maybe Int) Int
filterTemp = mkFilter actorRepeted

actorRepeted :: IORef (Maybe Int)
             -> Int
             -> ReadChannel Int
             -> WriteChannel Int
             -> IO ()
actorRepeted _ i rc wc = foldM_ rc $ \e -> if e /= i then push e wc else pure ()

sink' :: Stage (ReadChannel Int -> IO ())
sink' = withSink @DPExample $ flip foldM_ print

program :: IO ()
program = runDP $ mkDP @DPExample source' generator' sink'
