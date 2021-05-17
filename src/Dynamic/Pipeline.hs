{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE PolyKinds      #-}
module Dynamic.Pipeline where

--import qualified Control.Concurrent            as CC
--import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi.NoBlocking                                                      hiding ( Stream
                                                                                                                      )
--import           Data.ByteString               as B
import           Relude                                                                                        hiding ( map
                                                                                                                      , mapM
                                                                                                                      , traverse
                                                                                                                      )
--import qualified Relude                        as R

type Edge = (,)

newtype Channel a = Channel (InChan (Maybe a), OutChan (Maybe a))

data Chan list = Chan

newtype Code inChans outChans (eff :: * -> *) = Code (Chan inChans -> Chan outChans -> eff ())
newtype Stage inChans outChans (eff :: * -> *) = Stage { runStage :: Code inChans outChans eff }
newtype Filter inChans outChans (eff :: * -> *) = Filter { actors :: [Stage inChans outChans eff]}

type Input = Stage
type Generator ins outs eff = Filter ins outs eff
type Output = Stage

runDp :: Input ins outs IO -> Generator outs outs2 IO -> Output outs2 outs2 IO -> IO ()
runDp = undefined

data IC = ICq
  { inChannel1 :: Channel (Int, Int)
  , inChannel2 :: Channel [Int]
  }

data C = C
  { outChannel1 :: Channel (Int, Int)
  , outChannel2 :: Channel [Int]
  }

input :: Code IC C IO
input = Code $ \_ _ -> print "Input"

stageInput :: Stage IC C IO
stageInput = Stage input

output :: Code C C IO
output = Code $ \_ _ -> print "Output"

stageOutput :: Stage C C IO
stageOutput = Stage output

stageGenerator :: Generator C C IO
stageGenerator = filter'

filter' :: Filter C C IO
filter' = undefined

{-
λx.x> :t runDp stageInput stageGenerator stageOutput
runDp stageInput stageGenerator stageOutput :: IO ()
-}

-- {-# INLINE end' #-}
-- end' :: Channel a -> IO ()
-- end' = flip writeChan Nothing . fst

-- {-# INLINE endIn #-}
-- endIn :: Stage a b f -> IO ()
-- endIn = end' . inChannel

-- {-# INLINE endOut #-}
-- endOut :: Stage a b -> IO ()
-- endOut = end' . outChannel

-- {-# INLINE push' #-}
-- push' :: a -> Channel a -> IO ()
-- push' e = flip writeChan (Just e) . fst

-- {-# INLINE pushOut #-}
-- pushOut :: b -> Stage a b -> IO ()
-- pushOut e = push' e . outChannel

-- {-# INLINE pushIn #-}
-- pushIn :: a -> Stage a b -> IO ()
-- pushIn e = push' e . inChannel

-- {-# INLINE pull' #-}
-- pull' :: Channel a -> IO (Maybe a)
-- pull' = readChan (CC.threadDelay 100) . snd

-- {-# INLINE pullIn #-}
-- pullIn :: Stage a b -> IO (Maybe a)
-- pullIn = pull' . inChannel

-- {-# INLINE pullOut #-}
-- pullOut :: Stage a b -> IO (Maybe b)
-- pullOut = pull' . outChannel

-- {-# INLINE foldrS #-}
-- foldrS :: (Stage a b -> a -> IO (Stage a b)) -> Stage a b -> IO (Stage a b)
-- foldrS = loop
--   where loop fio c = maybe (return c) (loop fio <=< fio c) =<< pullIn c

-- {-# INLINE (|>>) #-}
-- (|>>) :: Stage a b -> (a -> IO c) -> IO (Stage c b)
-- (|>>) inp f = do
--   newC' <- newChan
--   newO' <- newChan
--   end' newO'
--   Stage newC' newO' <$> async (loop newC')
--  where
--   loop newCh = pullIn inp >>= loopUntilDone newCh (loopE newCh) loop

--   loopE ch a = flip push' ch =<< f a

-- loopUntilDone :: Channel b
--               -> (a -> IO ())
--               -> (Channel b -> IO ())
--               -> Maybe a
--               -> IO ()
-- loopUntilDone ch f loop = maybe (end' ch) ((>> loop ch) . f)

-- -- Generate Stage base on a seed function `f`
-- {-# INLINE unfoldM #-}
-- unfoldM :: IO a -> IO Bool -> IO (Stage a b)
-- unfoldM f stop = do
--   newCh  <- newChan
--   newCh' <- newChan
--   end' newCh'
--   Stage newCh newCh' <$> async (loop newCh)
--  where
--   loop newCh = ifM stop (end' newCh) (f >>= (`push'` newCh) >> loop newCh)

-- {-# INLINE mapM #-}
-- mapM :: (b -> IO c) -> Stage a b -> IO ()
-- mapM f inCh = async loop >>= wait
--   where loop = maybe (pure ()) (\a -> f a >> loop) =<< pullOut inCh

-- {-# INLINE foldMap #-}
-- foldMap :: Monoid m => (b -> m) -> Stage a b -> IO m
-- foldMap m s = async (loop mempty) >>= wait
--   where loop xs = maybe (pure xs) (loop . mappend xs . m) =<< pullOut s

-- {-# INLINE newStage #-}
-- newStage :: IO (Async ()) -> IO (Stage a b)
-- newStage as = Stage <$> newChan <*> newChan <*> as

-- {-# INLINE fromText #-}
-- fromText :: Text -> IO (Stage ByteString b)
-- fromText bs = newStage (async $ pure ()) >>= \s ->
--   R.mapM_ (`pushIn` s) (R.map R.encodeUtf8 $ R.lines bs) >> endIn s >> return s
