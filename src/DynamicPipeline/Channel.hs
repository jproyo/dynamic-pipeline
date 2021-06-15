{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : DynamicPipeline.Channel
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module DynamicPipeline.Channel 
  ( ReadChannel,
    WriteChannel,
    DynamicPipeline.Channel.foldM,
    foldM',
    push,
    pull,
    unfoldM,
    unfoldFile,
    unfoldT, 
    newChannel,
    end
  ) where

import qualified Control.Concurrent                       as CC
import           Control.Concurrent.Chan.Unagi.NoBlocking
import           Control.Lens                             hiding ((<|))
import           Data.ByteString                          as B
import           Data.Comp.Algebra                        (CoalgM, anaM)
import           Data.Foldable                            as F
import           Data.HList
import           Relude                                   as R


-- | 'WriteChannel' can only write values into some Channel Queue
-- 
-- [@a@]: Type that this Channel can write
newtype WriteChannel a = WriteChannel { unWrite :: InChan (Maybe a) }

-- | 'ReadChannel' can only read values of a previously written Channel. It is connected to a 'WriteChannel' but hidden for the user 
-- 
-- [@a@]: Type that this Channel can read
newtype ReadChannel a = ReadChannel { unRead :: OutChan (Maybe a) }

-- | 'foldM' is a /Catamorphism/ for consuming a 'ReadChannel' and do some Monadic @m@ computation with each element
{-# INLINE foldM #-}
foldM :: MonadIO m 
      => ReadChannel a -- ^'ReadChannel'
      -> (a -> m ()) -- ^Computation to do with read element
      -> m ()
foldM = flip foldM' (pure ())

-- | Idem 'foldM' but allows pass a monadic computation to perform at the end of the Channel
{-# INLINE foldM' #-}
foldM' :: MonadIO m 
       => ReadChannel a -- ^'ReadChannel'
       -> m () -- ^Computation to do at the end of the channel
       -> (a -> m ()) -- ^Computation to do with read element
       -> m ()
foldM' = loop'
  where
    loop' c onNothing io = 
      maybe onNothing (\e -> io e >> loop' c onNothing io) =<< liftIO (pull c)

-- | Push element @a@ into 'WriteChannel'
{-# INLINE push #-}
push :: MonadIO m => a -> WriteChannel a -> m ()
push a c = liftIO $ writeChan (unWrite c) (Just a)

-- | Pull element @Maybe a@ from 'ReadChannel'
{-# INLINE pull #-}
pull :: MonadIO m => ReadChannel a -> m (Maybe a)
pull = liftIO . readChan (CC.threadDelay 100) . unRead

-- | Coalgebra with Monadic computation to Feed some 'WriteChannel'
--
-- [@m@]: Monadic computation wrapping Coalgebra
--
-- [@a@]: Element get from some Source and to be write in some Channel
--
{-# WARNING SourceFeedCoalgM "INTERNAL USE" #-}
data SourceFeedCoalgM m a = Done -- ^ Termination 'Term' of the Coalgebra
                          | Computation -- ^ Continuation 'Term' of the Coalgebra
                             { _cSeed   :: m a -- ^ Computation that Seeds the /Anamorphism/ 
                             , _cStop   :: m Bool -- ^ Stop signal
                             , _cOnElem :: a -> m () -- ^ Computation on Read Element
                             }

-- | 'SourceFeedCoalgM' in terms of 'CoalgM' 
{-# WARNING sourceFeedCoalgM "INTERNAL USE" #-}
sourceFeedCoalgM :: MonadIO m => CoalgM m Maybe (SourceFeedCoalgM m a)
sourceFeedCoalgM Done = return Nothing
sourceFeedCoalgM c@Computation{..} = ifM _cStop 
                                          (return $ Just Done) 
                                          ( _cSeed >>= _cOnElem >> return (Just c) )

-- | unfold from a Monadic seed @m a@ to a 'WriteChannel'
{-# INLINE unfoldM #-}
unfoldM :: forall m a b. MonadIO m 
        => m a -- ^Monadic Seed 
        -> (a -> b) -- ^Map input from seed to something to be written in Channel
        -> m Bool -- ^When stop unfolding
        -> WriteChannel b -- ^'WriteChannel' to write input seed elements
        -> m ()
unfoldM seed fn stopIfM writeChannel  =
  let onElem = flip push writeChannel . fn
   in anaM sourceFeedCoalgM (Computation seed stopIfM onElem) >> pure ()

-- | Using 'unfoldM', unfold from file
{-# INLINE unfoldFile #-}
unfoldFile :: MonadIO m 
           => FilePath -- ^Seed 'FilePath' to read from
           -> WriteChannel b -- ^'WriteChannel' to write File contents
           -> (ByteString -> b) -- ^Transform 'ByteString' read from File to something meaningful for your App
           -> m ()
unfoldFile file writeChannel fn = liftIO $
    R.withFile file ReadMode $ \h ->
      unfoldM (B.hGetLine h) fn (R.hIsEOF h) writeChannel

-- | Idem 'unfoldM' but for 'Foldable', for example a List @[a]@. Useful for testing purpose
{-# INLINE unfoldT #-}
unfoldT :: (MonadIO m, Foldable t) => t a -> WriteChannel b -> (a -> b) -> m ()
unfoldT ts writeChannel fn = forM_ ts (flip push writeChannel . fn)

{-# WARNING newChannel "INTERNAL USE" #-}
{-# NOINLINE newChannel #-}
newChannel :: forall a. IO (WriteChannel a, ReadChannel a)
newChannel = bimap WriteChannel ReadChannel <$> newChan

{-# WARNING end "INTERNAL USE" #-}
{-# INLINE end #-}
end :: WriteChannel a -> IO ()
end = flip writeChan Nothing . unWrite

