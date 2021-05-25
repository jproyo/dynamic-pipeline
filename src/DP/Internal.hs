{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

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

newtype Channel a = Channel (InChan (Maybe a), OutChan (Maybe a))

data chann1 :<+> chann2 = chann1 :<+> chann2
 deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :<+>

data a :|= b = a :|= b
  deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :|=

data a :>> b = a :>> b
  deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :>>

data ChanIn (a :: Type)
data ChanOut (a :: Type)
data EOF
data Input (a :: Type)
data Generator (a :: Type)
data Output (a :: Type)

-- Inductive Type Family
type family Chans (a :: Type) (m :: Type -> Type) :: Type where
  Chans EOF m = m ()
  Chans (ChanOut a) m = Channel a
  Chans (ChanIn a) m = Maybe a
  Chans (ChanOut a :<+> ChanIn b :|= other) m = TypeError
                                                  ( 'Text "Cannot combine Input Channel with Output Channel to form a big channel"
                                                    ':$$: 'Text "in the type '"
                                                    ':<>: 'ShowType (ChanIn a :<+> ChanOut b)
                                                    ':<>: 'Text "'"
                                                    ':$$: 'Text "You can only combine to form a big channel Inputs or Outputs."
                                                    ':$$: 'Text "Example: ChanIn Int :<+> ChanIn String ... OR ChanOut Int :<+> ChanOut String ..."
                                                  )
  Chans (ChanIn a :<+> ChanOut b :|= other) m = TypeError
                                                  ( 'Text "Cannot combine Input Channel with Output Channel to form a big channel"
                                                    ':$$: 'Text "in the type '"
                                                    ':<>: 'ShowType (ChanIn a :<+> ChanOut b)
                                                    ':<>: 'Text "'"
                                                    ':$$: 'Text "You can only combine to form a big channel Inputs or Outputs."
                                                    ':$$: 'Text "Example: ChanIn Int :<+> ChanIn String ... OR ChanOut Int :<+> ChanOut String ..."
                                                  )
  Chans (ChanIn a :<+> more) m = Maybe a -> Chans more m
  Chans (ChanOut a :<+> more) m = Channel a -> Chans more m
  Chans (ChanIn a :|= ChanIn b) m = TypeError
                                      ( 'Text "Input Channel cannot be Connected to other Input Channel"
                                        ':$$: 'Text "in the type '"
                                        ':<>: 'ShowType (ChanIn a :|= ChanIn b)
                                        ':<>: 'Text "'"
                                        ':$$: 'Text "Input must be connected to Output only."
                                        ':$$: 'Text "Example: ChanIn Int :<+> ChanIn String :|= ChanOut Int :<+> ChanOut String"
                                      )
  Chans (ChanOut a :|= ChanOut b) m = TypeError
                                      ( 'Text "Output Channel cannot be Connected to other Output Channel"
                                        ':$$: 'Text "in the type '"
                                        ':<>: 'ShowType (ChanIn a :|= ChanIn b)
                                        ':<>: 'Text "'"
                                        ':$$: 'Text "Output must be connected to EOF only."
                                        ':$$: 'Text "Example: ChanOut Int :<+> ChanOut String :|= EOF"
                                      )
  Chans (input :|= output) m = Chans input m -> Chans output m

type family EvalTerm (a :: Type) :: Bool where
  EvalTerm (x :|= EOF) = 'True
  EvalTerm (x :|= y)   = EvalTerm y
  EvalTerm (x :<+> y)  = EvalTerm y
  EvalTerm x           = 'False

type family RequireEOF (a :: Bool) :: Constraint where
  RequireEOF 'True = ()
  RequireEOF 'False = TypeError
                        ( 'Text "EOF Termination is Required for building Channel chain"
                          ':$$: 'Text "Example: ChanIn Int :|= ChanOut String :|= EOF"
                          ':$$: 'Text "Example: ChanIn Int :<+> ChanIn String :|= ChanOut Int :<+> ChanOut String :|= EOF"
                        )
                        
type family TyEq (a :: k) (b :: k) :: Bool where
  TyEq a a = 'True
  TyEq a b = 'False

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And x     y     = 'False

type family EvalDP (a :: k) :: Bool where
  EvalDP (Input ins :>> (Generator gen :>> Output outs))       = And (EvalDP (ins :>> gen)) (EvalDP (gen :>> outs))
  EvalDP ((ChanIn a :<+> more) :>> more')                      = EvalDP (more :>> more')
  EvalDP ((ChanIn a :|= more) :>> more')                       = EvalDP (more :>> more')
  EvalDP ((ChanOut a :<+> more) :>> (ChanIn a' :<+> more'))    = And (TyEq a a') (EvalDP (more :>> more'))
  EvalDP ((ChanOut a :|= more) :>> (ChanIn a' :|= more'))      = And (TyEq a a') (EvalDP (more :>> more'))
  EvalDP (EOF :>> EOF)                                         = 'True
  EvalDP ((ChanOut a :<+> more) :>> (ChanIn a' :|= more'))     = 'False
  EvalDP ((ChanOut a :|= more)  :>> (ChanIn a' :<+> more'))    = 'False
  EvalDP (EOF :>> (ChanOut a :<+> more))                       = 'True
  EvalDP (EOF :>> (ChanOut a :|= more))                        = 'True


type family ValidDP (a :: Bool) :: Constraint where
  ValidDP 'True = ()
  ValidDP 'False = TypeError
                        ( 'Text "Invalid Dynamic Pipeline Chain"
                          ':$$: 'Text "Dynamic Pipeline should match Output Channel Type from previous Stage with Input Channel Types of next Stage."
                          ':$$: 'Text "Valid Example:"
                          ':$$: 'Text "`  type InputC       = ChanOut Int :|= EOF`"
                          ':$$: 'Text "`  type GeneratorC   = ChanIn Int :|= ChanOut Int :|= EOF`"
                          ':$$: 'Text "`  type OutputC      = ChanIn Int :|= EOF`"
                          ':$$: 'Text "`  type DP = Input InputC :>> Generator GeneratorC :>> Output OutputC`"
                          ':$$: 'Text "---------------------------------------------------------------------"
                          ':$$: 'Text "Invalid Example:"
                          ':$$: 'Text "`  type InputC       = ChanOut String :|= EOF`"
                          ':$$: 'Text "`  type GeneratorC   = ChanIn Int :|= ChanOut Int :|= EOF`"
                          ':$$: 'Text "`  type OutputC      = ChanIn Int :|= EOF`"
                          ':$$: 'Text "`  type DP = Input InputC :>> Generator GeneratorC :>> Output OutputC`"
                        )

-- Associated Type Family
class MkChans (a :: Type) where
  type HChan a :: [Type]
  mkChans :: Proxy a -> HList (HChan a)

instance MkChans EOF where
  type HChan EOF = '[]
  mkChans _ = HNil

instance MkChans more => MkChans (ChanIn a :|= more) where
  type HChan (ChanIn a :|= more) = Channel a ': HChan more
  mkChans _ = let c = unsafePerformIO (Channel @a <$> newChan) 
               in c `HCons` mkChans (Proxy @more)

instance MkChans more => MkChans (ChanOut a :|= more) where
  type HChan (ChanOut a :|= more) = Channel a ': HChan more
  mkChans _ = let c = unsafePerformIO (Channel @a <$> newChan) 
               in c `HCons` mkChans (Proxy @more) 

instance MkChans more => MkChans (ChanIn a :<+> more) where
  type HChan (ChanIn a :<+> more) = Channel a ': HChan more
  mkChans _ = let c = unsafePerformIO (Channel @a <$> newChan) 
               in c `HCons` mkChans (Proxy @more) 

instance MkChans more => MkChans (ChanOut a :<+> more) where
  type HChan (ChanOut a :<+> more) = Channel a ': HChan more
  mkChans _ = let c = unsafePerformIO (Channel @a <$> newChan) 
               in c `HCons` mkChans (Proxy @more)


-- Defunctionalization
data Stage a where
  Stage :: (RequireEOF (EvalTerm a), Monad m) => Proxy a -> Chans a m -> Stage (Chans a m)

mkStage :: forall a m. (RequireEOF (EvalTerm a), Monad m) => Proxy a -> Chans a m -> Stage (Chans a m)
mkStage = Stage @a @m

mkStage' :: forall a m. (RequireEOF (EvalTerm a), Monad m) => Chans a m -> Stage (Chans a m)
mkStage' = Stage @a @m (Proxy @a)

class Eval l t | l -> t where
  eval :: l -> t

instance forall a b. (a ~ b) => Eval (Stage a) b where
  eval (Stage _ f) = f

type InputC       = ChanOut Int :|= EOF
type GeneratorC   = ChanIn Int :|= ChanOut Int :|= EOF
type OutputC      = ChanIn Int :|= EOF

type DP = Input InputC :>> Generator GeneratorC :>> Output OutputC


{-
  (a -> b) -> (b -> c)   ....
  INPUT   => FILTER1 -> GENERATOR  -> OUTPUT 

do 
  fdsajflksfjalksdfa  -> x 
  fjda -> x
  for 
    y 


    Filter (b, d):
      actor1 : lee del canal (a,b) -> [a, b, d]
      actor2 [a, b, d] : lee del canal 2 [a_1, ..., a_n] --- [a, b, d] U [a_1, ..., a_n] ---> canal 2

-}

something :: (ValidDP (EvalDP a)) => Int
something = undefined

x :: Int
x = something @DP  

input :: Stage (Channel Int -> IO ())
input = mkStage' @InputC @IO $ \cout -> forM_ [1..100] (`push'` cout) >> end' cout

chanInput :: HList '[Channel Int]
chanInput = mkChans (Proxy @InputC)

gen :: Stage (Maybe Int -> Channel Int -> IO ())
gen = mkStage' @GeneratorC @IO $ \me cout -> maybe (end' cout) (flip push' cout . (+1)) me

chanGen :: HList '[Channel Int, Channel Int]
chanGen = mkChans (Proxy @GeneratorC)

output :: Stage (Maybe Int -> IO ())
output = mkStage' @OutputC @IO print

chanOutput :: HList '[Channel Int]
chanOutput = mkChans (Proxy @OutputC)

type Actor = Stage

newtype Filter a = Filter { unFilter :: NonEmpty (Actor a)}

prog :: IO ()
prog = do 
  outInput <- Channel <$> newChan
  void $ eval input outInput
  outGen <- Channel <$> newChan
  consumeAll outInput $ flip (eval gen) outGen
  consumeAll outGen $ eval output

consumeAll :: Channel a -> (Maybe a -> IO ()) -> IO ()
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

{-# INLINE end' #-}
end' :: Channel a -> IO ()
end' = flip writeChan Nothing . fst . unsafeCoerce

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
