{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
module DP.Internal where

import Unsafe.Coerce
import qualified Control.Concurrent            as CC
import           Control.Concurrent.Chan.Unagi.NoBlocking                                                      hiding ( Stream
                                                                                                                      )
import           Relude                                                                                        hiding ( map
                                                                                                                      , mapM
                                                                                                                      , traverse
                                                                                                                      )
import GHC.TypeLits
import Data.HList
import System.IO.Unsafe


type Edge = (,)

newtype InChannel a = InChan (Maybe a)
newtype OutChannel a = OutChan (Maybe a)

data chann1 :<+> chann2 = chann1 :<+> chann2
 deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :<+>

data a :>> b = a :>> b
  deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :>>

data ChanIn (a :: Type)
data ChanOut (a :: Type)
data ChanOutIn (a :: Type) (b :: Type)
data Channel (a :: Type)
data Input (a :: Type)
data Generator (a :: Type)
data Output

-- Inductive Type Family
type family WithInput (a :: Type) (m :: Type -> Type) :: Type where
  WithInput (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) m                         
                                             = WithInput (ChanIn inToGen) m
  WithInput (ChanIn (a :<+> more)) m         = InChannel a -> WithInput (ChanIn more) m
  WithInput (ChanIn a) m                     = InChannel a -> m ()
  WithInput (ChanOutIn (a :<+> more) ins) m  = OutChannel a -> WithInput (ChanOutIn more ins) m
  WithInput (ChanOutIn a ins) m              = OutChannel a -> WithInput (ChanIn ins) m 
  WithInput a _                              = TypeError
                                                  ( 'Text "Wrong Semantic for Building DP Program"
                                                    ':$$: 'Text "in the type '"
                                                    ':<>: 'ShowType a
                                                    ':<>: 'Text "'"
                                                    ':$$: 'Text "Language Grammar:"
                                                    ':$$: 'Text "DP    = Input CHANS :>> Generator CHANS :>> Output"
                                                    ':$$: 'Text "CHANS = Channel CH"
                                                    ':$$: 'Text "CH    = Type | Type :<+> CH"
                                                    ':$$: 'Text "Example: 'Input (Channel (Int :<+> Int)) :>> Generator (Channel (Int :<+> Int)) :>> Output'"
                                                  )

type family WithGenerator (a :: Type) (m :: Type -> Type) :: Type where
  WithGenerator (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) m                         
                                                 = WithGenerator (ChanOutIn inToGen genToOut) m
  WithGenerator (ChanIn (a :<+> more)) m         = InChannel a -> WithGenerator (ChanIn more) m
  WithGenerator (ChanIn a) m                     = InChannel a -> m ()
  WithGenerator (ChanOutIn (a :<+> more) ins) m  = OutChannel a -> WithGenerator (ChanOutIn more ins) m
  WithGenerator (ChanOutIn a ins) m              = OutChannel a -> WithGenerator (ChanIn ins) m 
  WithGenerator a _                              = TypeError
                                                ( 'Text "Wrong Semantic for Building DP Program"
                                                  ':$$: 'Text "in the type '"
                                                  ':<>: 'ShowType a
                                                  ':<>: 'Text "'"
                                                  ':$$: 'Text "Language Grammar:"
                                                  ':$$: 'Text "DP    = Input CHANS :>> Generator CHANS :>> Output"
                                                  ':$$: 'Text "CHANS = Channel CH"
                                                  ':$$: 'Text "CH    = Type | Type :<+> CH"
                                                  ':$$: 'Text "Example: 'Input (Channel (Int :<+> Int)) :>> Generator (Channel (Int :<+> Int)) :>> Output'"
                                                )

type family WithOutput (a :: Type) (m :: Type -> Type) :: Type where
  WithOutput (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) m                         
                                               = WithOutput (ChanOut genToOut) m
  WithOutput (ChanOut (a :<+> more)) m         = OutChannel a -> WithOutput (ChanOut more) m
  WithOutput (ChanOut a) m                     = OutChannel a -> m ()
  WithOutput a _                              = TypeError
                                                  ( 'Text "Wrong Semantic for Building DP Program"
                                                    ':$$: 'Text "in the type '"
                                                    ':<>: 'ShowType a
                                                    ':<>: 'Text "'"
                                                    ':$$: 'Text "Language Grammar:"
                                                    ':$$: 'Text "DP    = Input CHANS :>> Generator CHANS :>> Output"
                                                    ':$$: 'Text "CHANS = Channel CH"
                                                    ':$$: 'Text "CH    = Type | Type :<+> CH"
                                                    ':$$: 'Text "Example: 'Input (Channel (Int :<+> Int)) :>> Generator (Channel (Int :<+> Int)) :>> Output'"
                                                  )


-- Associated Type Family
-- class MkChans (a :: Type) where
--   type HChan a :: [Type]
--   mkChans :: Proxy a -> HList (HChan a)

-- instance MkChans more => MkChans (ChanIn a :|= more) where
--   type HChan (ChanIn a :|= more) = InChannel a ': HChan more
--   mkChans _ = let c = unsafePerformIO (Channel @a <$> newChan) 
--                in c `HCons` mkChans (Proxy @more) 

-- instance MkChans more => MkChans (ChanOut a :|= more) where
--   type HChan (ChanOut a :|= more) = OutChannel a ': HChan more
--   mkChans _ = let c = unsafePerformIO (Channel @a <$> newChan) 
--                in c `HCons` mkChans (Proxy @more)


-- instance MkChans more => MkChans (ChanIn a :<+> more) where
--   type HChan (ChanIn a :<+> more) = InChannel a ': HChan more
--   mkChans _ = let c = unsafePerformIO (Channel @a <$> newChan) 
--                in c `HCons` mkChans (Proxy @more) 

-- instance MkChans more => MkChans (ChanOut a :<+> more) where
--   type HChan (ChanOut a :<+> more) = OutChannel a ': HChan more
--   mkChans _ = let c = unsafePerformIO (Channel @a <$> newChan) 
--                in c `HCons` mkChans (Proxy @more)

--makeChans :: HList '[Channel a] -> (HList 

-- Defunctionalization
data Stage a where
  Stage :: forall a m (k :: Type -> (Type -> Type) -> Type). Monad m => Proxy a -> Proxy k -> a -> Stage a

mkStage :: forall a m (k :: Type -> (Type -> Type) -> Type). Monad m => Proxy a -> Proxy k -> a -> Stage a
mkStage = Stage @a @m @k

mkStage' :: forall a m (k :: Type -> (Type -> Type) -> Type). Monad m => a -> Stage a
mkStage' = Stage @a @m (Proxy @a) (Proxy @k)

class EvalC l t | l -> t where
  eval :: l -> t

instance forall a b. (a ~ b) => EvalC (Stage a) b where
  eval (Stage _ _ f) = f

-- something :: (ValidDP (EvalDP a)) => Int
-- something = undefined

-- x :: Int
-- x = something @DP  

type DPExample = Input (Channel Int) :>> Generator (Channel Int) :>> Output

input :: Stage (InChannel Int -> IO ())
input = mkStage' @(WithInput DPExample IO) @IO $ \cout -> forM_ [1..100] (`push'` cout) >> end' cout

generator :: Stage (OutChannel Int -> InChannel Int -> IO ())
generator = mkStage' @(WithGenerator DPExample IO) @IO $ \cin cout -> consumeAll cin $ maybe (end' cout) (flip push' cout . (+1))

output :: Stage (OutChannel Int -> IO ())
output = mkStage' @(WithOutput DPExample IO)  @IO $ \cin -> consumeAll cin print

-- chanInput :: HList '[OutChannel Int]
-- chanInput = mkChans (Proxy @InputC)
-- chanGen :: HList '[InChannel Int, OutChannel Int]
-- chanGen = mkChans (Proxy @GeneratorC)
-- chanOutput :: HList '[InChannel Int]
-- chanOutput = mkChans (Proxy @OutputC)

-- filter :: Filter (NonEmpty (Actor a)) (..... ) 
-- filter = mkFilter' @FilterC

-- type Actor = MonadState s m => Chans a -> m ()

-- newtype Filter a = Filter { unFilter :: NonEmpty (Actor a)}

-- prog' :: IO ()
-- prog' = do 
--   let cIns = chanInput
--   let cGen = chanGen
--   void $ hUncurry (eval input) cIns
--   outGen <- Channel <$> newChan
--   consumeAll cIns $ hUncurry (flip (eval gen) cGen)
--   consumeAll outGen $ eval output

-- prog :: IO ()
-- prog = do
--   outInput <- Channel <$> newChan
--   void $ eval input outInput
--   outGen <- Channel <$> newChan
--   consumeAll outInput $ flip (eval gen) outGen
--   consumeAll outGen $ eval output

consumeAll :: OutChannel a -> (Maybe a -> IO ()) -> IO ()
consumeAll c io = do 
  e <- pull' c
  io e
  maybe (pure ()) (const $ consumeAll c io) e

fn :: Int -> String -> Int -> IO ()
fn a b c = do
  print a 
  print b 
  print c

partiallyapply :: IO ()
partiallyapply = do 
  let x = 1 `HCons` "Hello" `HCons` 2 `HCons` HNil
  hUncurry fn x


-- data DP inp gen outp where
--   DP ::Stage inp inp IO -> Stage gen gen IO -> Stage outp outp IO -> DP inp gen outp

-- newtype Code inChans outChans (eff :: * -> *) = Code (Proxy inChans -> Proxy outChans -> eff ())
-- newtype Stage inChans outChans (eff :: * -> *) = Stage { runStage :: Code inChans outChans eff }
-- newtype Filter inChans outChans (eff :: * -> *) = Filter { actors :: [Stage inChans outChans eff]}

-- type Input = Stage
-- type Generator ins outs eff = Filter ins outs eff
-- type Output = Stage

-- runDp :: Input ins outs IO -> Generator outs outs2 IO -> Output outs2 outs2 IO -> IO ()
-- runDp = undefined

-- data IC = IC
--   { inChannel1 :: Channel (Int, Int)
--   , inChannel2 :: Channel [Int]
--   }

-- data C = C
--   { outChannel1 :: Channel (Int, Int)
--   , outChannel2 :: Channel [Int]
--   }

-- input :: Code IC C IO
-- input = Code $ \_ _ -> print "Input"

-- stageInput :: Stage IC C IO
-- stageInput = Stage input

-- output :: Code C C IO
-- output = Code $ \_ _ -> print "Output"

-- stageOutput :: Stage C C IO
-- stageOutput = Stage output

-- stageGenerator :: Generator C C IO
-- stageGenerator = filter'

-- filter' :: Filter C C IO
-- filter' = undefined

{-
λx.x> :t runDp stageInput stageGenerator stageOutput
runDp stageInput stageGenerator stageOutput :: IO ()
-}

{-# NOINLINE newChannel #-}
newChannel :: forall a. (InChan (Maybe a), OutChan (Maybe a))
newChannel = unsafePerformIO newChan

{-# INLINE end' #-}
end' :: InChannel a -> IO ()
end' = flip writeChan Nothing . unsafeCoerce

-- {-# INLINE endIn #-}
-- endIn :: Stage a b f -> IO ()
-- endIn = end' . inChannel

-- {-# INLINE endOut #-}
-- endOut :: Stage a b -> IO ()
-- endOut = end' . outChannel

{-# INLINE push' #-}
push' :: a -> InChannel a -> IO ()
push' e = flip writeChan (Just e) . unsafeCoerce

-- {-# INLINE pushOut #-}
-- pushOut :: b -> Stage a b -> IO ()
-- pushOut e = push' e . outChannel

-- {-# INLINE pushIn #-}
-- pushIn :: a -> Stage a b -> IO ()
-- pushIn e = push' e . inChannel

{-# INLINE pull' #-}
pull' :: OutChannel a -> IO (Maybe a)
pull' = readChan (CC.threadDelay 100) . unsafeCoerce

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
