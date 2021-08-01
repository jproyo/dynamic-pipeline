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
  ( ReadChannel
  , WriteChannel
  , (|=>)
  , (|=>|)
  , (|>=>)
  , (|>=>|)
  , mapF_
  , map_
  , mapM_
  , mapMF_
  , foldM_
  , foldWithM_
  , push
  , pull
  , unfoldM
  , unfoldFile
  , unfoldT
  , newChannel
  , end
  , finish
  ) where

import qualified Control.Concurrent                                as CC
import           Control.Concurrent.Chan.Unagi.NoBlocking
import           Control.Lens                                                                                  hiding ( (<|)
                                                                                                                      )
import           Data.ByteString                                   as B
import           Data.Foldable                                     as F
                                                                                                               hiding ( mapM_
                                                                                                                      )
import           Data.HList                                                                                    hiding ( foldM_
                                                                                                                      , mapM_
                                                                                                                      )
import           GHC.IO.Handle                                     as H
import           Relude                                            as R
                                                                                                               hiding ( mapM_
                                                                                                                      )


-- | 'WriteChannel' can only write values into some Channel Queue
-- 
-- [@a@]: Type that this Channel can write
newtype WriteChannel a = WriteChannel { unWrite :: InChan (Maybe a) }

-- | 'ReadChannel' can only read values of a previously written Channel. It is connected to a 'WriteChannel' but hidden for the user 
-- 
-- [@a@]: Type that this Channel can read
newtype ReadChannel a = ReadChannel { unRead :: OutChan (Maybe a) }

-- | 'map_' is a /Natural Transformation/ from consumer 'ReadChannel' to some producer 'WriteChannel' applying a transformation with function @f@
{-# INLINE map_ #-}
map_ :: MonadIO m
     => ReadChannel a -- ^'ReadChannel'
     -> WriteChannel b -- ^'ReadChannel'
     -> (a -> b) -- ^Monadic Transformation to do with read element
     -> m ()
map_ rc wc f = foldM_ rc $ flip push wc . f

-- | Same as 'map_' but with 'id' combinator
(|=>) :: MonadIO m => ReadChannel a -> WriteChannel a -> m ()
(|=>) rc wc = map_ rc wc id

infixl 5 |=>

-- | Same as 'map_' but mark Eof Channel after all processing
{-# INLINE mapF_ #-}
mapF_ :: MonadIO m
      => ReadChannel a -- ^'ReadChannel'
      -> WriteChannel b -- ^'ReadChannel'
      -> (a -> b) -- ^Monadic Transformation to do with read element
      -> m ()
mapF_ rc wc f = map_ rc wc f >> finish wc

-- | Alias 'mapF_'
(|=>|) :: MonadIO m => ReadChannel a -> WriteChannel b -> (a -> b) -> m ()
(|=>|) = mapF_

infixl 5 |=>|


-- | Same as 'map_' But applying a Monadic mapping
{-# INLINE mapM_ #-}
mapM_ :: MonadIO m
      => ReadChannel a -- ^'ReadChannel'
      -> WriteChannel b -- ^'ReadChannel'
      -> (a -> m (Maybe b)) -- ^Monadic Transformation to do with read element
      -> m ()
mapM_ rc wc f = foldM_ rc $ maybe (pure ()) (`push` wc) <=< f

-- | Alias 'mapM_'
(|>=>) :: MonadIO m => ReadChannel a -> WriteChannel b -> (a -> m (Maybe b)) -> m ()
(|>=>) = mapM_

infixr 5 |>=>

-- | Same as 'mapM_' but mark Eof Channel after all processing
{-# INLINE mapMF_ #-}
mapMF_ :: MonadIO m
       => ReadChannel a -- ^'ReadChannel'
       -> WriteChannel b -- ^'ReadChannel'
       -> (a -> m (Maybe b)) -- ^Monadic Transformation to do with read element
       -> m ()
mapMF_ rc wc f = mapM_ rc wc f >> finish wc

-- | Alias 'mapMF_'
(|>=>|) :: MonadIO m => ReadChannel a -> WriteChannel b -> (a -> m (Maybe b)) -> m ()
(|>=>|) = mapMF_

infixr 5 |>=>|


-- | 'foldM_' is a /Catamorphism/ for consuming a 'ReadChannel' and do some Monadic @m@ computation with each element
{-# INLINE foldM_ #-}
foldM_ :: MonadIO m
       => ReadChannel a -- ^'ReadChannel'
       -> (a -> m ()) -- ^Computation to do with read element
       -> m ()
foldM_ = flip foldWithM_ (pure ())

-- | Idem 'foldM_' but allows pass a monadic computation to perform at the end of the Channel
{-# INLINE foldWithM_ #-}
foldWithM_ :: MonadIO m
           => ReadChannel a -- ^'ReadChannel'
           -> m () -- ^Computation to do at the end of the channel
           -> (a -> m ()) -- ^Computation to do with read element
           -> m ()
foldWithM_ = loop'
  where loop' c onNothing io = maybe onNothing (\e -> io e >> loop' c onNothing io) =<< liftIO (pull c)

-- | Push element @a@ into 'WriteChannel'
{-# INLINE push #-}
push :: MonadIO m => a -> WriteChannel a -> m ()
push a c = liftIO $ writeChan (unWrite c) (Just a)

-- | Pull element @Maybe a@ from 'ReadChannel'
{-# INLINE pull #-}
pull :: MonadIO m => ReadChannel a -> m (Maybe a)
pull = liftIO . readChan CC.yield . unRead

-- | Finalize Channel to indicate EOF mark and allow progress on following consumers
finish :: MonadIO m => WriteChannel a -> m ()
finish = liftIO . end


-- | Coalgebra with Monadic computation to Feed some 'WriteChannel'
--
-- [@m@]: Monadic computation wrapping Coalgebra
--
-- [@a@]: Element get from some Source and to be write in some Channel
--
-- | unfold from a Monadic seed @m a@ to a 'WriteChannel'
{-# INLINE unfoldM #-}
unfoldM :: forall m a b
         . MonadIO m
        => m a -- ^Monadic Seed 
        -> (a -> b) -- ^Map input from seed to something to be written in Channel
        -> m Bool -- ^When stop unfolding
        -> WriteChannel b -- ^'WriteChannel' to write input seed elements
        -> m ()
unfoldM = loop'
 where
  loop' seed fn stopIfM writeChannel =
    ifM stopIfM (finish writeChannel) (seed >>= flip push writeChannel . fn >> loop' seed fn stopIfM writeChannel)
-- | Using 'unfoldM', unfold from file
{-# INLINE unfoldFile #-}
unfoldFile :: MonadIO m
           => FilePath -- ^Seed 'FilePath' to read from
           -> WriteChannel b -- ^'WriteChannel' to write File contents
           -> (ByteString -> b) -- ^Transform 'ByteString' read from File to something meaningful for your App
           -> m ()
unfoldFile file writeChannel fn =
  liftIO $ R.withFile file ReadMode $ \h -> unfoldM (B.hGetLine h) fn (H.hIsEOF h) writeChannel

-- | Idem 'unfoldM' but for 'Foldable', for example a List @[a]@. Useful for testing purpose
{-# INLINE unfoldT #-}
unfoldT :: (MonadIO m, Foldable t) => t a -> WriteChannel b -> (a -> b) -> m ()
unfoldT ts writeChannel fn = forM_ ts (flip push writeChannel . fn) >> finish writeChannel

{-# WARNING newChannel "INTERNAL USE" #-}
{-# NOINLINE newChannel #-}
newChannel :: forall a . IO (WriteChannel a, ReadChannel a)
newChannel = bimap WriteChannel ReadChannel <$> newChan

{-# WARNING end "INTERNAL USE" #-}
{-# INLINE end #-}
end :: WriteChannel a -> IO ()
end = flip writeChan Nothing . unWrite


