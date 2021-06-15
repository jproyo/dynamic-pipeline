-- |
-- Module      : DynamicPipeline
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- __DynamicPipeline__ is a __/Type Safe/__ Dynamic and Parallel Streaming Library, which is an implementation of __Dynamic Pipeline Paradigm (DPP)__ 
-- proposed in this paper [DPP](https://biblioteca.sistedes.es/articulo/the-dynamic-pipeline-paradigm/).
-- The aim of this Library is to provide all the __Type level__ constructs to guide the user in building a /DPP/ flow to solve any algorithm that fits on 
-- this computational model. 
-- 
-- This implementation has been developed using /Type Level Programming/ techniques like @Type families@, @Defunctionalization@, @Existential Types@ and 
-- @Dynamic Record Tagged Types@ among others.
-- Using all this techniques, we provide a /High Level and Type Safe/ DynamicPipeline Library to build a Data Flow Algorithm avoiding as much as possible 
-- boilerplate code, but maintaining safety and robustness.
-- 
-- Example of Filtering Repeated elements of a Stream
-- 
-- @
-- import "DynamicPipeline"
--
-- type DPExample = 'Source' ('Channel' (Int ':<+>' 'Eof')) ':>>' 'Generator' ('Channel' (Int ':<+>' 'Eof')) ':>>' 'Sink'
-- 
-- source' :: 'Stage' ('WriteChannel' Int -> 'DP' s ())
-- source' = 'withSource' @DPExample $ \cout -> 'unfoldT' ([1 .. 1000] <> [1 .. 1000]) cout identity
-- 
-- generator' :: 'GeneratorStage' DPExample (Maybe Int) Int s
-- generator' =
--   let gen = 'withGenerator' @DPExample genAction
--    in 'mkGenerator' gen filterTemp
-- 
-- genAction :: 'Filter' DPExample (Maybe Int) Int s 
--           -> 'ReadChannel' Int
--           -> 'WriteChannel' Int
--           -> 'DP' s ()
-- genAction filter\' cin cout = 
--   void $ 'unfoldFilterForAll'' filter' Just (\`'push'` cout) cin 'HNil'
-- 
-- filterTemp :: 'Filter' DPExample (Maybe Int) Int s 
-- filterTemp = 'mkFilter' actorRepeted
-- 
-- actorRepeted :: Int
--              -> 'ReadChannel' Int
--              -> 'WriteChannel' Int
--              -> StateT (Maybe Int) ('DP' s) ()
-- actorRepeted i rc wc = do
--   liftIO $ 'foldM' rc $ \e -> if e /= i then 'push' e wc else pure ()
-- 
-- sink\' :: 'Stage' ('ReadChannel' Int -> 'DP' s ())
-- sink\' = 'withSink' @DPExample $ flip 'foldM' print
-- 
-- program :: IO ()
-- program = 'runDP' $ 'mkDP' @DPExample source\' generator\' sink\'
-- @
--
module DynamicPipeline 
    ( -- * DP Flow Grammar #grammar#
      -- $grammar
    
      -- * Types Flow definition
      Eof,
      Sink,
      Generator,
      Source,
      Channel,
      type (:>>)(..),
      type (:<+>)(..), 
      -- * Smart Constructors 
      DynamicPipeline,
      Filter,
      Actor,
      GeneratorStage,
      Stage,
      ValidDP,
      IsDP,
      DP, 
      UnFoldFilter, 
      withDP, 
      mkGenerator,
      mkFilter,
      single,
      actor,
      (|>>>),
      (|>>),
      withSource,
      withGenerator,
      withSink,
      mkDP,
      runDP,
      unfoldF,
      mkUnfoldFilter,
      mkUnfoldFilter',
      mkUnfoldFilterForAll,
      mkUnfoldFilterForAll',
      (.*.), HList(HNil), hHead, 
      -- * Channels
      ReadChannel,
      WriteChannel,
      foldM,
      foldM',
      push,
      pull,
      unfoldM,
      unfoldFile,
      unfoldT
    )
    where

import Data.HList ((.*.), HList(HNil), hHead)
import DynamicPipeline.Flow
import DynamicPipeline.Channel
import DynamicPipeline.Stage

-- $grammar
-- Something here