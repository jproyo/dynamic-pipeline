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
-- type DPExample = 'Source' ('Channel' (Int ':<+>' 'Eof')) ':=>' 'Generator' ('Channel' (Int ':<+>' 'Eof')) ':=>' 'Sink'
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
--   let unfoldFilter = 'mkUnfoldFilterForAll'' (\`'push'` cout) filter' Just cin 'HNil' 
--    in void $ 'unfoldF' unfoldFilter
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
      
      -- * Building 'DynamicPipeline' #dp#
      -- $dp

      -- ** Generator and Filter #genfilter#
      -- $generator
    
      -- * Types Flow definition
      Eof,
      Sink,
      Generator,
      Source,
      Channel,
      FeedbackChannel,
      type (:=>)(..),
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
      (.*.), HList(HNil,HCons), hHead, hTail,
      -- * Channels
      ReadChannel,
      WriteChannel,
      (|=>),
      (|=>|),
      (|>=>),
      (|>=>|),
      mapF_,
      map_,
      mapM_,
      mapMF_,
      foldM_,
      foldWithM_,
      push,
      pull,
      finish,
      unfoldM,
      unfoldFile,
      unfoldT
    )
    where

import Data.HList ((.*.), HList(HNil,HCons), hHead, hTail)
import DynamicPipeline.Flow
import DynamicPipeline.Channel
import DynamicPipeline.Stage

-- $grammar
-- The following is the Regular Grammar allowed to build a /DPP/ Flow definition:
-- 
-- @
-- __DP__       -> 'Source' __CHANS__ ':=>' 'Generator' __CHANS__ ':=>' 'Sink'
-- __DP__       -> 'Source' __CHANS ':=>' 'Generator' __CHANS__ ':=>' __FEEDBACK__ ':=>' 'Sink'
-- __CHANS__    -> 'Channel' __CH__
-- __FEEDBACK__ -> 'FeedbackChannel' __CH__
-- __CH__       -> 'Type' ':<+>' __CH__ | 'Eof'
-- @
--
-- Example: 
-- 
-- @ 'Source' ('Channel' (Int ':<+>' Int)) ':=>' 'Generator' ('Channel' (Int ':<+>' Int)) ':=>' 'Sink' @
--
-- Or with Feedback Channel to retrofit Streamming
--
-- @ 'Source' ('Channel' (Int ':<+>' Int)) ':=>' 'Generator' ('Channel' (Int ':<+>' Int)) ':=>' 'FeedbackChannel' ('String' ':<+>' 'Eof') ':=>' 'Sink' @
--
-- $dp
-- 'DynamicPipeline' Data type is the point where all the information is contained in order the library can run our /DP/ Algorithm.
-- 
-- This Data type contains three fundamental pieces: 'Source', 'Generator' and 'Sink'. But all these are dynamic based on the 
-- defined Flow.
-- One of the fundamental feature of this Library is to provide several combinators that deduce from the Definition Flow, what are the 
-- Function Signatures the user must fulfill according to his definition.
-- 
-- All these combinators work in the same manner which based on the flow definition present to the user at compile time what is the function that must 
-- be provided.
-- Lets see an example based on the "Misc.RepeatedDP", which basically filter out repeated elements in a stream.
--
-- >>> import Relude
-- >>> import DynamicPipeline
-- >>> type DPEx = Source (Channel (Int :<+> Eof)) :=> Generator (Channel (Int :<+> Eof)) :=> Sink
-- >>> :t withSource @DPEx
-- withSource @DPEx
--   :: forall k (st :: k).
--      (WriteChannel Int -> DP st ())
--      -> Stage (WriteChannel Int -> DP st ())
--
-- In @type DPEx = ..@ we are defining a Flow which contains a 'Source' that is going to have an 'Int' Channel that is going to feed the 'Generator'. 
-- Therefore the 'Source' should write on that channel and because of that we are being asked to provide a Function that @WriteChannel Int -> DP st ()@.
-- Remember that our Monadic context is always 'DP'.
-- 
-- Having that we can provide that function and have all the pieces together for 'Source'.
--
-- >>> let source' = withSource @DPEx $ \wc -> unfoldT ([1..10] <> [1..10] <> [1..10] <> [1..10]) wc identity
-- >>> :t source'
-- source' :: forall k (st :: k). Stage (WriteChannel Int -> DP st ())
--
-- So we are done. we provide that function.
-- Now we can do the same for 'Sink' which is the other opposite part of the Stream because 'Generator' is a little different as we can see in the documentation.
--
-- >>> let sink' = withSink @DPEx $ \rc -> foldM rc $ putStr . show
-- >>> :t sink'
-- sink' :: forall k (st :: k). Stage (ReadChannel Int -> DP st ())
--
-- Done with 'Sink'.
--
-- $generator
-- Now we reach to the last piece which needs more work to be done because it is the core of /DPP/ which dynamically adds Parallel computations between the 'Generator' Stage
-- and previous 'Filter's and 'Source'.
--
-- Fortunately we have the same combinator 'withGenerator' but it is not so straightforward what to put there. So, lets go step by step.
--
-- >>> :t withGenerator @DPEx
-- withGenerator @DPEx
--   :: forall k filter (st :: k).
--      (filter -> ReadChannel Int -> WriteChannel Int -> DP st ())
--      -> Stage
--           (filter -> ReadChannel Int -> WriteChannel Int -> DP st ())
--
-- At the first Glance it is asking for some similar function that is going to return our desired 'Stage' but there is some type parameter which is 
-- not so obvious __@filter@__.
-- Fortunately we have combinators for that as well that can save us a lot of time and effort.
--
-- /Note: We could have done a Generator with an Empty 'Filter' but we are not taking advantage of DPP in building a Pipeline Parallelization Computational Algorithm/
--
-- In the case of 'Filter' we have several combinators at our disposal.
-- 
-- * Use 'mkFilter' if your /DPP/ contains 1 actor per Filter
--
-- * Use '|>>' and '|>>>' if your /DPP/ contains more than 1 actor
--
-- In our example we are going to use 1 actor only that is going to discard repeated elements
-- >>> :t mkFilter @DPEx actor1
-- Variable not in scope:
--   actor1
--     :: filterParam
--        -> ReadChannel Int
--        -> WriteChannel Int
--        -> StateT filterState (DP st) ()
--
-- First lets fill in the gaps.
--
-- >>> let filter' = mkFilter @DPEx (\i rc wc -> foldM rc $ \e -> if e /= i then push e wc else pure ())
-- >>> :t filter'
-- filter' :: forall k filterState (st :: k). Filter DPEx filterState Int st
--
-- Basically we are checking if the element that we are reding from the Channel (Remember that we can have multiple 'Filter' on front writing to us),
-- is equal to the First Element that was read by the 'Generator' and on which this 'Filter' was instantiated with (a.k.a. @filterParam@). 
-- If the element is not equal we 'push' it to the next 'Filter' or 'Generator', otherwise we discarded.
--
--
-- >>> let gen' = mkGenerator (withGenerator @DPEx $ \f r w -> let unf = mkUnfoldFilterForAll' (`push` w) f Just r HNil in void $ unfoldF unf) filter'
-- >>> :t gen'
-- gen' :: forall k (st :: k). GeneratorStage DPEx (Maybe Int) Int st
--
-- Now we have everything in place we only need to call 'runDP' and 'mkDP' 
--
-- >>> runDP $ mkDP @DPEx source' gen' sink'
-- 12345678910
-- 


