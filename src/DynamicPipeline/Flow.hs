{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : DynamicPipeline.Flow
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module DynamicPipeline.Flow 
    ( Eof,
      Sink,
      Generator,
      Source,
      Channel,
      FeedbackChannel,
      type (:=>)(..),
      type (:<+>)(..),
      ChanIn,
      ChanInIn,
      ChanOut,
      ChanOutIn,
      ChansFilter,
      ChanWriteSource,
      ChanReadWriteGen,
      ChanReadOut,
      ChanRecord(..),
      InOutChan(..),
      MkCh(..),
      MkChans(..),
      ExpandGenToCh, 
      ExpandSinkToCh,
      ExpandSourceToCh,
      ExpandFilterToCh,
      inGenOut,
      toHList, 
      makeChans,
      makeChansF,
      getFilterChannels
    ) where

import           Control.Lens                             hiding ((<|))
import           Data.Foldable                            as F
import           Data.HList
import           DynamicPipeline.Channel
import           Relude                                   as R

-- | 'Source' contains the 'Source' Stage its Channels definitions in the DP definition Flow.
-- 
-- @ a ~ 'Channel' @
data Source (a :: Type)

-- | 'Generator' contains the 'Generator' Stage its Channels definitions in the DP definition Flow.
-- 
-- @ a ~ 'Channel' @
data Generator (a :: Type)

-- | 'Sink' contains the 'Sink' Stage end of Flow of DP definition.
data Sink

-- |'Eof' is the __End of Channel__ mark in the DP Definition Flow
data Eof

-- |'Channel' is the Container Type of /Open Union Type/ which is going to be defined with ':<+>'.
--
-- @ a ~ (Type ':<+>' Type ':<+>' ... ':<+>' Eof) @
data Channel (a :: Type)

-- |'FeedbackChannel' is the Container Type of /Open Union Type/ which is going to be defined with ':<+>' and indicates that this
-- | Channel is for feedback to Source
--
-- @ a ~ (Type ':<+>' Type ':<+>' ... ':<+>' Eof) @
data FeedbackChannel (a :: Type)

-- | This is the Type level function of the /Open Union Type/ for Channels. 
-- 
-- Channels forms an /Open Union Type/ in each stage because according to __DPP__ we can have multiple /In/ and /Out/ Channels 
-- in a Single Stage. 
--
-- 'Eof' should be the last Channel of the /Open Union Type/ to indicate termination of the Grammar.
--
-- @ chann1 ~ Type @
--
-- @ chann2 ~ Type @
data chann1 :<+> chann2 = chann1 :<+> chann2
 deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :<+>

-- | This is the Type level function of the /Open Union Type/ for Stages. 
-- 
-- This should have the form:
--
-- @ 'Source' ('Channel' ..) ':=>' 'Generator' ('Channel' ..) ':=>' 'Sink' @
data a :=> b = a :=> b
  deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :=>

-- Internal Data Types for expanding function based on Channel definitions
{-# WARNING ChanIn "INTERNAL USE" #-}
data ChanIn (a :: Type)
{-# WARNING ChanOut "INTERNAL USE" #-}
data ChanOut (a :: Type)
{-# WARNING ChanOutIn "INTERNAL USE" #-}
data ChanOutIn (a :: Type) (b :: Type)
{-# WARNING ChanInIn "INTERNAL USE" #-}
data ChanInIn (a :: Type) (b :: Type)
{-# WARNING ChansFilter "INTERNAL USE" #-}
data ChansFilter (a :: Type)
{-# WARNING ChanWriteSource "INTERNAL USE" #-}
data ChanWriteSource (a :: Type)
{-# WARNING ChanReadWriteGen "INTERNAL USE" #-}
data ChanReadWriteGen (a :: Type)
{-# WARNING ChanReadWriteF "INTERNAL USE" #-}
data ChanReadWriteF (a :: Type)
{-# WARNING ChanReadOut "INTERNAL USE" #-}
data ChanReadOut (a :: Type)

-- Type encoding for Building Chans. Only for internal use in the Associated Type Family and combinators of MkCh and MkChans
-- For accessing Dynamic Indexed Records of Channels
{-# WARNING inLabel "INTERNAL USE" #-}
inLabel :: Label "source"
inLabel = Label

{-# WARNING genLabel "INTERNAL USE" #-}
genLabel :: Label "generator"
genLabel = Label

{-# WARNING outLabel "INTERNAL USE" #-}
outLabel :: Label "sink"
outLabel = Label

{-# WARNING inChLabel "INTERNAL USE" #-}
inChLabel :: Label "in-ch"
inChLabel = Label

{-# WARNING outChLabel "INTERNAL USE" #-}
outChLabel :: Label "out-ch"
outChLabel = Label

-- Associated Type Family: Building Source and Sink Channels
{-# WARNING MkCh "INTERNAL USE" #-}
class MkCh (a :: Type) where
  type HChI a :: [Type]
  type HChO a :: [Type]
  mkCh :: Proxy a -> IO (HList (HChI a), HList (HChO a))

instance MkCh more => MkCh (a :<+> more) where
  type HChI (a :<+> more) = ReadChannel a ': HChI more
  type HChO (a :<+> more) = WriteChannel a ': HChO more
  mkCh _ = do
    (o, i) <- newChannel @a
    (il, ol) <- mkCh (Proxy @more)
    return (i .*. il, o .*. ol)

instance MkCh Eof where
  type HChI Eof = '[]
  type HChO Eof = '[]
  mkCh _ = return (HNil, HNil)

-- Type Family Defunctionalization to Expand Source, Generator and Sinks to its own HList Channel types.
{-# WARNING ExpandToHList "INTERNAL USE" #-}
type family ExpandToHList (a :: Type) (param :: Type) :: [Type]
type instance ExpandToHList (ChanWriteSource ( Source (Channel inToGen)
                                          :=> Generator (Channel genToOut)
                                          :=> Sink )
                            ) _ = HChO inToGen
type instance ExpandToHList (ChanWriteSource ( Source (Channel inToGen)
                                          :=> Generator (Channel genToOut)
                                          :=> FeedbackChannel toSource
                                          :=> Sink )
                            ) _ = HAppendListR (HChI toSource) (HChO inToGen)

type instance ExpandToHList (ChanReadWriteGen ( Source (Channel inToGen)
                                            :=> Generator (Channel genToOut)
                                            :=> Sink)
                            ) filter = filter ': HAppendListR (HChI inToGen) (HChO genToOut)
type instance ExpandToHList (ChanReadWriteGen ( Source (Channel inToGen)
                                            :=> Generator (Channel genToOut)
                                            :=> FeedbackChannel toSource
                                            :=> Sink)
                            ) filter = filter ': HAppendListR (HAppendListR (HChI inToGen) (HChO genToOut)) (HChO toSource)
type instance ExpandToHList (ChanReadOut ( Source (Channel inToGen)
                                       :=> Generator (Channel genToOut)
                                       :=> Sink )
                            ) filter = HChI genToOut
type instance ExpandToHList (ChanReadOut ( Source (Channel inToGen)
                                       :=> Generator (Channel genToOut)
                                       :=> FeedbackChannel toSource
                                       :=> Sink )
                            ) filter = HChI genToOut



{-# WARNING ExpandToHListF "INTERNAL USE" #-}
type family ExpandToHListF (a :: Type) (param :: Type) (state :: Type) :: [Type]
type instance ExpandToHListF (ChanReadWriteF ( Source (Channel inToGen)
                                            :=> Generator (Channel genToOut)
                                            :=> Sink)
                            ) param state = IORef state ': param ': HAppendListR (HChI inToGen) (HChO genToOut)
type instance ExpandToHListF (ChanReadWriteF ( Source (Channel inToGen)
                                            :=> Generator (Channel genToOut)
                                            :=> FeedbackChannel toSource
                                            :=> Sink)
                            ) param state = IORef state ': param ': HAppendListR (HAppendListR (HChI inToGen) (HChO genToOut)) (HChO toSource)

{-# WARNING ExpandSourceToCh "INTERNAL USE" #-}
type ExpandSourceToCh a = ExpandToHList (ChanWriteSource a) Void
{-# WARNING ExpandGenToCh "INTERNAL USE" #-}
type ExpandGenToCh a filter = ExpandToHList (ChanReadWriteGen a) filter
{-# WARNING ExpandFilterToCh "INTERNAL USE" #-}
type ExpandFilterToCh a param state = ExpandToHListF a param state
{-# WARNING ExpandSinkToCh "INTERNAL USE" #-}
type ExpandSinkToCh a = ExpandToHList (ChanReadOut a) Void

data InOutChan lr lw = InOutChan
  { _iocReadChans  :: HList lr
  , _iocWriteChans :: HList lw
  }

toHList :: HAppendList lr lw => InOutChan lr lw -> HList (HAppendListR lr lw)
toHList InOutChan{..} = _iocReadChans `hAppend` _iocWriteChans

data ChanRecord slr slw glr glw silr silw = ChanRecord
  { _crSource :: InOutChan slr slw
  , _crGenerator :: InOutChan glr glw
  , _crSink :: InOutChan silr silw
  }

-- Class for building Channels base on a DP Definition on `a` Type
{-# WARNING MkChans "INTERNAL USE" #-}
class MkChans (a :: Type) where
  type HChan a :: Type
  mkChans :: Proxy a -> IO (HChan a)

-- Instance for Building Channels for all the Chain Source :=> Generator :=> Sink
instance ( MkCh inToGen
         , MkCh genToOut)
    => MkChans (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> Sink) where

  type HChan (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> Sink)
      = ChanRecord '[] (HChO inToGen) (HChI inToGen) (HChO genToOut) (HChI genToOut) '[]

  mkChans _ =  do
    (rs, ws) <- mkCh (Proxy @inToGen)
    (rg, wg) <- mkCh (Proxy @genToOut)
    return $ ChanRecord
      { _crSource = InOutChan HNil ws
      , _crGenerator = InOutChan rs wg
      , _crSink = InOutChan rg HNil
      }

instance ( MkCh inToGen
         , MkCh genToOut
         , MkCh toSource
         , HAppendList (HChO genToOut) (HChO toSource))
    => MkChans (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> FeedbackChannel toSource :=> Sink) where

  type HChan (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> FeedbackChannel toSource :=> Sink)
    = ChanRecord (HChI toSource) (HChO inToGen) 
                 (HChI inToGen) (HAppendListR (HChO genToOut) (HChO toSource)) 
                 (HChI genToOut) '[]

  mkChans _ =  do
    (rf, wf) <- mkCh (Proxy @toSource)
    (rs, ws) <- mkCh (Proxy @inToGen)
    (rg, wg) <- mkCh (Proxy @genToOut)
    return $ ChanRecord
      { _crSource = InOutChan rf ws
      , _crGenerator = InOutChan rs (wg `hAppend` wf)
      , _crSink = InOutChan rg HNil
      }

-- Instance for Building Only Channels for Filters on each Generator action
instance MkCh inToGen
    => MkChans (ChansFilter (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> Sink)) where

  type HChan (ChansFilter (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> Sink))
      = InOutChan (HChI inToGen) (HChO inToGen)

  mkChans _ =  uncurry InOutChan <$> mkCh (Proxy @inToGen)

instance ( MkCh inToGen
         )
    => MkChans (ChansFilter (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> FeedbackChannel toSource :=> Sink)) where

  type HChan (ChansFilter (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> FeedbackChannel toSource :=> Sink))
    = InOutChan (HChI inToGen) (HChO inToGen)

  mkChans _ =  uncurry InOutChan <$> mkCh (Proxy @inToGen)


{-# WARNING makeChans "INTERNAL USE" #-}
{-# INLINE makeChans #-}
makeChans :: forall (a :: Type) slr slw glr glw silr silw. (MkChans a, HChan a ~ ChanRecord slr slw glr glw silr silw) => IO (ChanRecord slr slw glr glw silr silw)
makeChans = mkChans (Proxy @a)

{-# WARNING makeChansF "INTERNAL USE" #-}
{-# INLINE makeChansF #-}
makeChansF :: forall (a :: Type) flr flw. (MkChans a, HChan a ~ InOutChan flr flw) => IO (InOutChan flr flw)
makeChansF = mkChans (Proxy @a)

-- Ugly Dynamic Indexed Record Viewer to generate specific list of channels
{-# WARNING sourceChans "INTERNAL USE" #-}
{-# INLINE sourceChans #-}
sourceChans :: HAppendList slr slw => ChanRecord slr slw _ _ _ _ -> HList (HAppendListR slr slw)
sourceChans = toHList . _crSource

{-# WARNING generatorChans "INTERNAL USE" #-}
{-# INLINE generatorChans #-}
generatorChans :: HAppendList glr glw => ChanRecord _ _ glr glw _ _ -> HList (HAppendListR glr glw)
generatorChans = toHList . _crGenerator


{-# WARNING sinkChans "INTERNAL USE" #-}
{-# INLINE sinkChans #-}
sinkChans :: HAppendList silr silw => ChanRecord _ _ _ _ silr silw -> HList (HAppendListR silr silw)
sinkChans = toHList . _crSink

{-# WARNING inGenOut "INTERNAL USE" #-}
{-# INLINE inGenOut #-}
inGenOut :: (HAppendList slr slw, HAppendList glr glw, HAppendList silr silw) => ChanRecord slr slw glr glw silr silw -> (HList (HAppendListR slr slw), HList (HAppendListR glr glw), HList (HAppendListR silr silw))
inGenOut ch = (sourceChans ch, generatorChans ch, sinkChans ch)

{-# WARNING getFilterChannels "INTERNAL USE" #-}
{-# INLINE getFilterChannels #-}
getFilterChannels :: InOutChan lr lw -> (HList lr, HList lw)
getFilterChannels InOutChan{..} = (_iocReadChans, _iocWriteChans)

