{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module DP.Internal where

import Unsafe.Coerce
import qualified Control.Concurrent            as CC
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

{-
<- defineDP $ do 
    shareChanIn <- createChan @Int
    input $ do 
      otherChanIn <- createChan @(Int, Int)
      outputChan <- createChan @[Int]
      onAction $ do 

-}
data chann1 :<+> chann2 = chann1 :<+> chann2
 deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :<+>

data a :|= b = a :|= b
  deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :|=

data ChanIn (a :: *)
data ChanOut (a :: *)

-- Associated Type Classes
class Monad m => HasChannel a m where
  type Chans a m :: *

instance Monad m => HasChannel (ChanOut a) m where
  type Chans (ChanOut a) m = Channel a -> m ()

-- instance Monad m => HasChannel (ChanIn a) m where
--   type Chans (ChanIn a) m = Channel a

-- instance Monad m => HasChannel (ChanIn a :<+> more) m where
--   type Chans (ChanIn a :<+> more) m = Channel a -> Chans more m

instance Monad m => HasChannel (ChanIn a) m where
   type Chans (ChanIn a) m = Maybe a

instance Monad m => HasChannel (ChanIn a :<+> more) m where
  type Chans (ChanIn a :<+> more) m = Maybe a -> Chans more m

instance Monad m => HasChannel (ChanOut a :<+> more) m where
  type Chans (ChanOut a :<+> more) m = Channel a -> Chans more m

instance Monad m => HasChannel (input :|= output) m where
  type Chans (input :|= output) m = Chans input m -> Chans output m

-- class CreateChannels a where
--   type CChans a :: *
-- --  toF :: 

-- instance Monad m => CreateChannels (ChanIn a) m where
--   type CChans (ChanIn a) m = Channel a

-- instance Monad m => CreateChannels (ChanIn a :<+> more) m where
--   type CChans (ChanIn a :<+> more) m = Channel a -> Chans more m


-- Defunctionalization
data ExecF a where
  ExecF :: HasChannel a m => Proxy a -> Chans a m -> ExecF (Chans a m)

class Eval l t | l -> t where
  eval :: l -> t

instance forall a b. (a ~ b) => Eval (ExecF a) b where
  eval (ExecF _ f) = f

-- Declaration of Channels
type X = ChanIn Int :<+> ChanIn (Int,Int) :|= ChanOut Int :<+> ChanOut (Int,Int)

myfn :: Maybe Int -> Maybe (Int, Int) -> Channel Int -> Channel (Int, Int) -> IO ()
myfn a b _ _ = do
  print a
  print b
  return ()

-- Expanded Channels by the compiler asking for the function required according to what the user declare according to X type
--some' :: ExecF (Channel Int -> Channel (Int, Int) -> Channel Int -> Channel (Int, Int) -> IO ())
--some' :: ExecF (Int -> (Int, Int) -> Channel Int -> Channel (Int, Int) -> IO ())
some' :: ExecF
  (Maybe Int
   -> Maybe (Int, Int) -> Channel Int -> Channel (Int, Int) -> IO ())
some' = ExecF (Proxy @X) myfn

x :: IO ()
x = do 
  a <- Channel <$> newChan @(Maybe Int)
  b <- Channel <$> newChan @(Maybe (Int,Int))
  push' 1 a
  push' 2 a
  push' (3,4) b
  push' (5,6) b
  a' <- pull' a
  b' <- pull' b
  eval some' a' b' a b

-- data DP inp gen outp where
--   DP ::Stage inp inp IO -> Stage gen gen IO -> Stage outp outp IO -> DP inp gen outp

newtype Code inChans outChans (eff :: * -> *) = Code (Proxy inChans -> Proxy outChans -> eff ())
newtype Stage inChans outChans (eff :: * -> *) = Stage { runStage :: Code inChans outChans eff }
newtype Filter inChans outChans (eff :: * -> *) = Filter { actors :: [Stage inChans outChans eff]}

type Input = Stage
type Generator ins outs eff = Filter ins outs eff
type Output = Stage

runDp :: Input ins outs IO -> Generator outs outs2 IO -> Output outs2 outs2 IO -> IO ()
runDp = undefined

data IC = IC
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

{-# INLINE push' #-}
push' :: a -> Channel a -> IO ()
push' e = flip writeChan (Just e) . fst . unsafeCoerce

-- {-# INLINE pushOut #-}
-- pushOut :: b -> Stage a b -> IO ()
-- pushOut e = push' e . outChannel

-- {-# INLINE pushIn #-}
-- pushIn :: a -> Stage a b -> IO ()
-- pushIn e = push' e . inChannel

{-# INLINE pull' #-}
pull' :: Channel a -> IO (Maybe a)
pull' = readChan (CC.threadDelay 100) . snd . unsafeCoerce

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
