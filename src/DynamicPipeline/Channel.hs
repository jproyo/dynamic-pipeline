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

-- Definitions
newtype WriteChannel a = WriteChannel { unWrite :: InChan (Maybe a) }
newtype ReadChannel a = ReadChannel { unRead :: OutChan (Maybe a) }

{-# INLINE foldM #-}
foldM :: MonadIO m => ReadChannel a -> (a -> m ()) -> m ()
foldM = loop'
  where
    loop' c io = 
      maybe (pure ()) (\e -> io e >> loop' c io) =<< liftIO (pull c)

{-# INLINE foldM' #-}
foldM' :: MonadIO m => ReadChannel a -> m () -> (a -> m ()) -> m ()
foldM' = loop'
  where
    loop' c onNothing io = 
      maybe onNothing (\e -> io e >> loop' c onNothing io) =<< liftIO (pull c)

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
