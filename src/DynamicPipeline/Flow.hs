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
      type (:>>)(..),
      type (:<+>)(..),
      ChanIn,
      ChanOut,
      ChanOutIn,
      ChansFilter,
      ChanWriteSource,
      ChanReadWriteGen,
      ChanReadOut,
      MkCh(..),
      MkChans(..),
      ExpandGenToCh, 
      ExpandSinkToCh,
      ExpandSourceToCh,
      ExpandFilterToCh,
      AllChans,
      FilterChans,
      inGenOut,
      makeChans,
      getFilterChannels
    ) where

import           Control.Lens                             hiding ((<|))
import           Data.Foldable                            as F
import           Data.HList
import           Data.HList.Labelable
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
-- @ 'Source' ('Channel' ..) ':>>' 'Generator' ('Channel' ..) ':>>' 'Sink' @
data a :>> b = a :>> b
  deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :>>

-- Internal Data Types for expanding function based on Channel definitions
data ChanIn (a :: Type)
data ChanOut (a :: Type)
data ChanOutIn (a :: Type) (b :: Type)
data ChansFilter (a :: Type)
data ChanWriteSource (a :: Type)
data ChanReadWriteGen (a :: Type)
data ChanReadOut (a :: Type)

-- Type encoding for Building Chans. Only for internal use in the Associated Type Family and combinators of MkCh and MkChans
-- For accessing Dynamic Indexed Records of Channels
inLabel :: Label "Source"
inLabel = Label

genLabel :: Label "generator"
genLabel = Label

outLabel :: Label "Sink"
outLabel = Label

inChLabel :: Label "in-ch"
inChLabel = Label

outChLabel :: Label "out-ch"
outChLabel = Label

-- Associated Type Family: Building Source and Sink Channels
class MkCh (a :: Type) where
  type HChI a :: [Type]
  type HChO a :: [Type]
  mkCh :: Proxy a -> IO (HList (HChI a), HList (HChO a))

instance MkCh more => MkCh (a :<+> more) where
  type HChI (a :<+> more) = WriteChannel a ': HChI more
  type HChO (a :<+> more) = ReadChannel a ': HChO more
  mkCh _ = do
    (i, o) <- newChannel @a
    (il, ol) <- mkCh (Proxy @more)
    return (i .*. il, o .*. ol)

instance MkCh Eof where
  type HChI Eof = '[]
  type HChO Eof = '[]
  mkCh _ = return (HNil, HNil)

-- Type Family Defunctionalization to Expand Source, Generator and Sinks to its own HList Channel types.
type family ExpandToHList (a :: Type) (param :: Type) :: [Type]
type instance ExpandToHList (ChanWriteSource ( Source (Channel inToGen)
                                          :>> Generator (Channel genToOut)
                                          :>> Sink )
                            ) _ = HChI inToGen

type instance ExpandToHList (ChanReadWriteGen ( Source (Channel inToGen)
                                            :>> Generator (Channel genToOut)
                                            :>> Sink)
                            ) filter = filter ': HAppendListR (HChO inToGen) (HChI genToOut)

type instance ExpandToHList (ChanReadOut ( Source (Channel inToGen)
                                       :>> Generator (Channel genToOut)
                                       :>> Sink )
                            ) filter = HChO genToOut

type ExpandSourceToCh a = ExpandToHList (ChanWriteSource a) Void
type ExpandGenToCh a filter = ExpandToHList (ChanReadWriteGen a) filter
type ExpandFilterToCh a param = ExpandGenToCh a param
type ExpandSinkToCh a = ExpandToHList (ChanReadOut a) Void

-- Class for building Channels base on a DP Definition on `a` Type
class MkChans (a :: Type) where
  type HChan a :: Type
  mkChans :: Proxy a -> IO (HChan a)

-- Instance for Building Channels for all the Chain Source :>> Generator :>> Sink
instance ( MkCh inToGen
         , MkCh genToOut)
    => MkChans (Source (Channel inToGen) :>> Generator (Channel genToOut) :>> Sink) where

  type HChan (Source (Channel inToGen) :>> Generator (Channel genToOut) :>> Sink)
    = Record '[ Tagged "Source" (Record '[ Tagged "in-ch" (HList (HChI inToGen))
                                        , Tagged "out-ch" (HList (HChO inToGen))
                                        ]
                        )
       , Tagged "generator" (Record '[ Tagged "in-ch" (HList (HChI genToOut))
                                     , Tagged "out-ch" (HList (HChO genToOut))
                                     ]
                            )
       , Tagged "Sink" (Record '[ Tagged "in-ch" (HList (HChI genToOut))])
       ]

  mkChans _ =  do
    (ii, io) <- mkCh (Proxy @inToGen)
    (gi, go) <- mkCh (Proxy @genToOut)
    (oi, _)  <- mkCh (Proxy @genToOut)
    return $ (inLabel .=. (inChLabel .=. ii .*. outChLabel .=. io .*. emptyRecord))
              .*.
              (genLabel .=. (inChLabel .=. gi .*. outChLabel .=. go .*. emptyRecord))
              .*.
              (outLabel .=. (inChLabel .=. oi .*. emptyRecord))
              .*.
              emptyRecord

-- Instance for Building Only Channels for Filters on each Generator action
instance MkCh inToGen
    => MkChans (ChansFilter (Source (Channel inToGen) :>> Generator (Channel genToOut) :>> Sink)) where

  type HChan (ChansFilter (Source (Channel inToGen) :>> Generator (Channel genToOut) :>> Sink))
    = Record '[ Tagged "in-ch" (HList (HChO inToGen))
              , Tagged "out-ch" (HList (HChI inToGen))
              ]

  mkChans _ =  do
    (writes', reads') <- mkCh (Proxy @inToGen)
    return $ mkRecord (inChLabel .=. reads' .*. outChLabel .=. writes' .*. HNil)


{-# INLINE makeChans #-}
makeChans :: forall (a :: Type). MkChans a => IO (HChan a)
makeChans = mkChans (Proxy @a)

-- Ugly Dynamic Indexed Record Viewer to generate specific list of channels
{-# INLINE sourceChans #-}
sourceChans :: ( LabeledOpticF (LabelableTy r1) (Const t1)
              , LabeledOpticP (LabelableTy r1) (->)
              , LabeledOpticTo (LabelableTy r1) "in-ch" (->)
              , LabeledOpticF (LabelableTy r2) (Const t1)
              , LabeledOpticP (LabelableTy r2) (->)
              , LabeledOpticTo (LabelableTy r2) "Source" (->)
              , Labelable "in-ch" r1 s t2 t1 t1
              , Labelable "Source" r2 t3 t3 (r1 s) (r1 t2))
           => r2 t3 -> t1
sourceChans = let inl  = hLens' inLabel
                  inch = hLens' inChLabel
               in view (inl . inch)

{-# INLINE generatorChans #-}
generatorChans :: ( LabeledOpticF (LabelableTy r1) (Const (HList l1))
                  , LabeledOpticP (LabelableTy r1) (->)
                  , LabeledOpticTo (LabelableTy r1) "out-ch" (->)
                  , LabeledOpticF (LabelableTy r2) (Const (HList l2))
                  , LabeledOpticP (LabelableTy r2) (->)
                  , LabeledOpticTo (LabelableTy r2) "in-ch" (->)
                  , LabeledOpticF (LabelableTy r3) (Const (HList l2))
                  , LabeledOpticP (LabelableTy r3) (->)
                  , LabeledOpticTo (LabelableTy r3) "generator" (->)
                  , LabeledOpticF (LabelableTy r3) (Const (HList l1))
                  , LabeledOpticTo (LabelableTy r3) "Source" (->)
                  , HAppendList l1 l2
                  , Labelable "generator" r3 t1 t1 (r2 s1) (r2 t2)
                  , Labelable "in-ch" r2 s1 t2 (HList l2) (HList l2)
                  , Labelable "Source" r3 t1 t1 (r1 s2) (r1 t3)
                  , Labelable "out-ch" r1 s2 t3 (HList l1) (HList l1))
               => r3 t1 -> HList (HAppendListR l1 l2)
generatorChans ch = let inl  = hLens' inLabel
                        genl  = hLens' genLabel
                        inch = hLens' inChLabel
                        outch = hLens' outChLabel
                        outsIn = view (inl . outch) ch
                        insGen = view (genl . inch) ch
                     in outsIn `hAppendList` insGen

{-# INLINE sinkChans #-}
sinkChans :: ( LabeledOpticF (LabelableTy r1) (Const t1)
               , LabeledOpticP (LabelableTy r1) (->)
               , LabeledOpticTo (LabelableTy r1) "out-ch" (->)
               , LabeledOpticF (LabelableTy r2) (Const t1)
               , LabeledOpticP (LabelableTy r2) (->)
               , LabeledOpticTo (LabelableTy r2) "generator" (->)
               , Labelable "generator" r2 t2 t2 (r1 s) (r1 t3)
               , Labelable "out-ch" r1 s t3 t1 t1)
            => r2 t2 -> t1
sinkChans = let genl  = hLens' genLabel
                outch = hLens' outChLabel
             in view (genl . outch)


type AllChans r2 r3 l1 r4 l2 t2 s t1 s2 t5 l3 l4 = (LabeledOpticTo (LabelableTy r2) "in-ch" (->),
            LabeledOpticF (LabelableTy r3) (Const (HList l3)),
            LabeledOpticP (LabelableTy r3) (->),
            LabeledOpticTo (LabelableTy r3) "Source" (->),
            LabeledOpticF (LabelableTy r2) (Const (HList l1)),
            LabeledOpticP (LabelableTy r2) (->),
            LabeledOpticTo (LabelableTy r2) "out-ch" (->),
            LabeledOpticTo (LabelableTy r4) "in-ch" (->),
            LabeledOpticF (LabelableTy r3) (Const (HList l2)),
            LabeledOpticTo (LabelableTy r3) "generator" (->),
            LabeledOpticF (LabelableTy r3) (Const (HList l1)),
            LabeledOpticF (LabelableTy r2) (Const (HList l3)),
            LabeledOpticF (LabelableTy r4) (Const (HList l4)),
            LabeledOpticP (LabelableTy r4) (->),
            LabeledOpticTo (LabelableTy r4) "out-ch" (->),
            LabeledOpticF (LabelableTy r3) (Const (HList l4)),
            LabeledOpticF (LabelableTy r4) (Const (HList l2)),
            HAppendList l1 l2, Labelable "generator" r3 t2 t2 (r4 s) (r4 t1),
            Labelable "in-ch" r2 s2 t5 (HList l3) (HList l3),
            Labelable "in-ch" r4 s t1 (HList l2) (HList l2),
            Labelable "Source" r3 t2 t2 (r2 s2) (r2 t5),
            Labelable "out-ch" r2 s2 t5 (HList l1) (HList l1),
            Labelable "out-ch" r4 s t1 (HList l4) (HList l4))

{-# INLINE inGenOut #-}
inGenOut :: AllChans r2 r3 l1 r4 l2 t2 s t1 s2 t5 l3 l4 => r3 t2 -> (HList l3, HList (HAppendListR l1 l2), HList l4)
inGenOut ch = (sourceChans ch, generatorChans ch, sinkChans ch)


type FilterChans r b t a = (LabeledOpticF (LabelableTy r) (Const b),
                            LabeledOpticTo (LabelableTy r) "out-ch" (->),
                            Labelable "out-ch" r t t b b,
                            LabeledOpticF (LabelableTy r) (Const a),
                            LabeledOpticP (LabelableTy r) (->),
                            LabeledOpticTo (LabelableTy r) "in-ch" (->),
                            Labelable "in-ch" r t t a a)

{-# INLINE getFilterChannels #-}
getFilterChannels :: FilterChans r b t a => r t -> (a, b)
getFilterChannels ch =
   let inch = hLens' inChLabel
       outch = hLens' outChLabel
       reads' = ch^.inch
       writes' = ch^.outch
    in (reads', writes')
