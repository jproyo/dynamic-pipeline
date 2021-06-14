-- |
-- Module      : DynamicPipeline.Channel
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module DynamicPipeline.Channel where

import qualified Control.Concurrent                       as CC
import           Control.Concurrent.Chan.Unagi.NoBlocking
import           Control.Lens                             hiding ((<|))
import           Data.ByteString                          as B
import           Data.Comp.Algebra                        (CoalgM, anaM)
import           Data.Foldable                            as F
import           Data.HList
import           Data.HList.Labelable
import           Relude                                   as R

-- Definitions
newtype WriteChannel a = WriteChannel { unWrite :: InChan (Maybe a) }
newtype ReadChannel a = ReadChannel { unRead :: OutChan (Maybe a) }

{-# INLINE forall #-}
forall :: MonadIO m => ReadChannel a -> (a -> m ()) -> m ()
forall = loop'
  where
    loop' c io = maybe (pure ()) (\e -> io e >> loop' c io) =<< liftIO (pull c)

{-# INLINE forall' #-}
forall' :: MonadIO m => ReadChannel a -> m () -> (a -> m ()) -> m ()
forall' = loop'
  where
    loop' c onNothing io = maybe onNothing (\e -> io e >> loop' c onNothing io) =<< liftIO (pull c)

{-# NOINLINE newChannel #-}
newChannel :: forall a. IO (WriteChannel a, ReadChannel a)
newChannel = bimap WriteChannel ReadChannel <$> newChan

{-# INLINE end #-}
end :: WriteChannel a -> IO ()
end = flip writeChan Nothing . unWrite

{-# INLINE push #-}
push :: MonadIO m => a -> WriteChannel a -> m ()
push a c = liftIO $ writeChan (unWrite c) (Just a)

{-# INLINE pull #-}
pull :: MonadIO m => ReadChannel a -> m (Maybe a)
pull = liftIO . readChan (CC.threadDelay 100) . unRead

data InputFeedCoalgM m a b = Done
                           | Computation
                             { _cSeed   :: m a
                             , _cStop   :: m Bool
                             , _cOnElem :: a -> m ()
                             }

inputFilterCoalgM :: MonadIO m => CoalgM m Maybe (InputFeedCoalgM m a b)
inputFilterCoalgM Done = return Nothing
inputFilterCoalgM c@Computation{..} = ifM _cStop
                                (return $ Just Done)
                                ( _cSeed >>= _cOnElem >> return (Just c) )

{-# INLINE unfoldM #-}
unfoldM :: forall m a b. MonadIO m => m a -> (a -> b) -> m Bool -> WriteChannel b -> m ()
unfoldM seed fn stopIfM writeChannel  =
  let onElem = flip push writeChannel . fn
   in anaM inputFilterCoalgM (Computation seed stopIfM onElem) >> pure ()

{-# INLINE unfoldFile #-}
unfoldFile :: MonadIO m => FilePath -> WriteChannel b -> (ByteString -> b) -> m ()
unfoldFile file writeChannel fn = liftIO $
    R.withFile file ReadMode $ \h ->
      unfoldM (B.hGetLine h) fn (R.hIsEOF h) writeChannel

{-# INLINE unfoldT #-}
unfoldT :: (MonadIO m, Foldable t) => t a -> WriteChannel b -> (a -> b) -> m ()
unfoldT ts writeChannel fn = forM_ ts (flip push writeChannel . fn)


-- This types are for building DP Declaration

-- |'Input' contains the 'Input' Stage definition with its Channels in the DP definition Flow
-- | 'a' has the for
data Input (a :: Type)
-- |'Eof' is the __End of Channel__ mark in the DP Definition Flow
data Generator (a :: Type)
-- |'Eof' is the __End of Channel__ mark in the DP Definition Flow
data Output
-- |'Eof' is the __End of Channel__ mark in the DP Definition Flow
data Eof

-- This is for connecting different kind of channels
data chann1 :<+> chann2 = chann1 :<+> chann2
 deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :<+>

-- This is for connecting Input with Generator and Output
data a :>> b = a :>> b
  deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :>>

-- Internal Data Types for expanding function based on Channel definitions
data ChanIn (a :: Type)
data ChanOut (a :: Type)
data ChanOutIn (a :: Type) (b :: Type)
data Channel (a :: Type)
data ChansFilter (a :: Type)
data ChanWriteInput (a :: Type)
data ChanReadWriteGen (a :: Type)
data ChanReadOut (a :: Type)

-- Type encoding for Building Chans. Only for internal use in the Associated Type Family and combinators of MkCh and MkChans
-- For accessing Dynamic Indexed Records of Channels
inLabel :: Label "input"
inLabel = Label

genLabel :: Label "generator"
genLabel = Label

outLabel :: Label "output"
outLabel = Label

inChLabel :: Label "in-ch"
inChLabel = Label

outChLabel :: Label "out-ch"
outChLabel = Label

-- Associated Type Family: Building Input and Output Channels
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

-- Type Family Defunctionalization to Expand Input, Generator and Outputs to its own HList Channel types.
type family ExpandToHList (a :: Type) (param :: Type) :: [Type]
type instance ExpandToHList (ChanWriteInput ( Input (Channel inToGen)
                                          :>> Generator (Channel genToOut)
                                          :>> Output )
                            ) _ = HChI inToGen

type instance ExpandToHList (ChanReadWriteGen ( Input (Channel inToGen)
                                            :>> Generator (Channel genToOut)
                                            :>> Output)
                            ) filter = filter ': HAppendListR (HChO inToGen) (HChI genToOut)

type instance ExpandToHList (ChanReadOut ( Input (Channel inToGen)
                                       :>> Generator (Channel genToOut)
                                       :>> Output )
                            ) filter = HChO genToOut

type ExpandInputToCh a = ExpandToHList (ChanWriteInput a) Void
type ExpandGenToCh a filter = ExpandToHList (ChanReadWriteGen a) filter
type ExpandFilterToCh a param = ExpandGenToCh a param
type ExpandOutputToCh a = ExpandToHList (ChanReadOut a) Void

-- Class for building Channels base on a DP Definition on `a` Type
class MkChans (a :: Type) where
  type HChan a :: Type
  mkChans :: Proxy a -> IO (HChan a)

-- Instance for Building Channels for all the Chain Input :>> Generator :>> Output
instance ( MkCh inToGen
         , MkCh genToOut)
    => MkChans (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) where

  type HChan (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output)
    = Record '[ Tagged "input" (Record '[ Tagged "in-ch" (HList (HChI inToGen))
                                        , Tagged "out-ch" (HList (HChO inToGen))
                                        ]
                        )
       , Tagged "generator" (Record '[ Tagged "in-ch" (HList (HChI genToOut))
                                     , Tagged "out-ch" (HList (HChO genToOut))
                                     ]
                            )
       , Tagged "output" (Record '[ Tagged "in-ch" (HList (HChI genToOut))])
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
    => MkChans (ChansFilter (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output)) where

  type HChan (ChansFilter (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output))
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
{-# INLINE inputChans #-}
inputChans :: ( LabeledOpticF (LabelableTy r1) (Const t1)
              , LabeledOpticP (LabelableTy r1) (->)
              , LabeledOpticTo (LabelableTy r1) "in-ch" (->)
              , LabeledOpticF (LabelableTy r2) (Const t1)
              , LabeledOpticP (LabelableTy r2) (->)
              , LabeledOpticTo (LabelableTy r2) "input" (->)
              , Labelable "in-ch" r1 s t2 t1 t1
              , Labelable "input" r2 t3 t3 (r1 s) (r1 t2))
           => r2 t3 -> t1
inputChans = let inl  = hLens' inLabel
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
                  , LabeledOpticTo (LabelableTy r3) "input" (->)
                  , HAppendList l1 l2
                  , Labelable "generator" r3 t1 t1 (r2 s1) (r2 t2)
                  , Labelable "in-ch" r2 s1 t2 (HList l2) (HList l2)
                  , Labelable "input" r3 t1 t1 (r1 s2) (r1 t3)
                  , Labelable "out-ch" r1 s2 t3 (HList l1) (HList l1))
               => r3 t1 -> HList (HAppendListR l1 l2)
generatorChans ch = let inl  = hLens' inLabel
                        genl  = hLens' genLabel
                        inch = hLens' inChLabel
                        outch = hLens' outChLabel
                        outsIn = view (inl . outch) ch
                        insGen = view (genl . inch) ch
                     in outsIn `hAppendList` insGen

{-# INLINE outputChans #-}
outputChans :: ( LabeledOpticF (LabelableTy r1) (Const t1)
               , LabeledOpticP (LabelableTy r1) (->)
               , LabeledOpticTo (LabelableTy r1) "out-ch" (->)
               , LabeledOpticF (LabelableTy r2) (Const t1)
               , LabeledOpticP (LabelableTy r2) (->)
               , LabeledOpticTo (LabelableTy r2) "generator" (->)
               , Labelable "generator" r2 t2 t2 (r1 s) (r1 t3)
               , Labelable "out-ch" r1 s t3 t1 t1)
            => r2 t2 -> t1
outputChans = let genl  = hLens' genLabel
                  outch = hLens' outChLabel
               in view (genl . outch)


type AllChans r2 r3 l1 r4 l2 t2 s t1 s2 t5 l3 l4 = (LabeledOpticTo (LabelableTy r2) "in-ch" (->),
            LabeledOpticF (LabelableTy r3) (Const (HList l3)),
            LabeledOpticP (LabelableTy r3) (->),
            LabeledOpticTo (LabelableTy r3) "input" (->),
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
            Labelable "input" r3 t2 t2 (r2 s2) (r2 t5),
            Labelable "out-ch" r2 s2 t5 (HList l1) (HList l1),
            Labelable "out-ch" r4 s t1 (HList l4) (HList l4))

{-# INLINE inGenOut #-}
inGenOut :: AllChans r2 r3 l1 r4 l2 t2 s t1 s2 t5 l3 l4 => r3 t2 -> (HList l3, HList (HAppendListR l1 l2), HList l4)
inGenOut ch = (inputChans ch, generatorChans ch, outputChans ch)


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
