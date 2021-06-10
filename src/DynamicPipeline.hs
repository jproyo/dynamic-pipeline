module DynamicPipeline 
    ( module DynamicPipeline.Internal
    , (.*.)
    )
    where

import Data.HList ((.*.))
import DynamicPipeline.Internal
    ( DynamicPipeline,
      Filter,
      Actor,
      GeneratorStage,
      Stage,
      ValidDP,
      IsDP,
      Eof,
      Output,
      Generator,
      Input,
      Channel,
      type (:>>)(..),
      type (:<+>)(..),
      ReadChannel,
      WriteChannel,
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
      forall,
      forall',
      push,
      pull,
      unfoldOnChannel,
      unfoldFile,
      unfoldT,
      spawnFilterForAll,
      spawnFilterForAll',
      spawnFilterWith)