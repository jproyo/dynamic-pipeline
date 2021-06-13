module DynamicPipeline 
    ( module DynamicPipeline.Stage
    , module DynamicPipeline.Channel
    , (.*.)
    )
    where

import Data.HList ((.*.))
import DynamicPipeline.Channel
    ( Eof,
      Output,
      Generator,
      Input,
      Channel,
      type (:>>)(..),
      type (:<+>)(..),
      ReadChannel,
      WriteChannel,
      forall,
      forall',
      push,
      pull,
      unfoldOnChannel,
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
      spawnFilterForAll,
      spawnFilterForAll',
      spawnFilterWith)