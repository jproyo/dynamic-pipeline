-- |
-- Module      : DynamicPipeline
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module DynamicPipeline 
    ( module DynamicPipeline.Stage
    , module DynamicPipeline.Channel
    , module DynamicPipeline.Flow
    , (.*.), HList(HNil), hHead
    )
    where

import Data.HList ((.*.), HList(HNil), hHead)
import DynamicPipeline.Flow
    ( Eof,
      Output,
      Generator,
      Input,
      Channel,
      type (:>>)(..),
      type (:<+>)(..),
    )
import DynamicPipeline.Channel
    ( ReadChannel,
      WriteChannel,
      fold,
      fold',
      push,
      pull,
      unfoldM,
      unfoldFile,
      unfoldT,
    )
import DynamicPipeline.Stage
    ( DynamicPipeline,
      Filter,
      Actor,
      GeneratorStage,
      Stage,
      ValidDP,
      IsDP,
      DP, 
      withDP, 
      mkGenerator,
      mkFilter,
      single,
      actor,
      (|>>>),
      (|>>),
      withInput,
      withGenerator,
      withOutput,
      mkDP,
      runDP,
      unfoldFilterForAll,
      unfoldFilterForAll',
      unfoldFilterWith)