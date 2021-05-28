{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
module DP.Internal where

import qualified Control.Concurrent            as CC
import           Control.Concurrent.Chan.Unagi.NoBlocking                                                      hiding ( Stream
                                                                                                                      )
import           Relude                                                                                        hiding ( map
                                                                                                                      , mapM
                                                                                                                      , traverse
                                                                                                                      )
import GHC.TypeLits
import Data.HList
import Data.HList.Labelable
import System.IO.Unsafe
import Control.Lens


type Edge = (,)

newtype WriteChannel a = WriteChannel { unWrite :: InChan (Maybe a) }
newtype ReadChannel a = ReadChannel { unRead :: OutChan (Maybe a) }

data chann1 :|= chann2 = chann1 :|= chann2
 deriving (Typeable, Eq, Show, Functor, Traversable, Foldable, Bounded)
infixr 5 :|=

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
  WithInput (ChanIn (a :<+> more)) m         = WriteChannel a -> WithInput (ChanIn more) m
  WithInput (ChanIn a) m                     = WriteChannel a -> m ()
  WithInput (ChanOutIn (a :<+> more) ins) m  = ReadChannel a -> WithInput (ChanOutIn more ins) m
  WithInput (ChanOutIn a ins) m              = ReadChannel a -> WithInput (ChanIn ins) m 
  WithInput a _                              = TypeError
                                                  ( 'Text "Invalid Semantic for Building DP Program"
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
  WithGenerator (ChanIn (a :<+> more)) m         = WriteChannel a -> WithGenerator (ChanIn more) m
  WithGenerator (ChanIn a) m                     = WriteChannel a -> m ()
  WithGenerator (ChanOutIn (a :<+> more) ins) m  = ReadChannel a -> WithGenerator (ChanOutIn more ins) m
  WithGenerator (ChanOutIn a ins) m              = ReadChannel a -> WithGenerator (ChanIn ins) m 
  WithGenerator a _                              = TypeError
                                                ( 'Text "Invalid Semantic for Building DP Program"
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
  WithOutput (ChanOut (a :<+> more)) m         = ReadChannel a -> WithOutput (ChanOut more) m
  WithOutput (ChanOut a) m                     = ReadChannel a -> m ()
  WithOutput a _                              = TypeError
                                                  ( 'Text "Invalid Semantic for Building DP Program"
                                                    ':$$: 'Text "in the type '"
                                                    ':<>: 'ShowType a
                                                    ':<>: 'Text "'"
                                                    ':$$: 'Text "Language Grammar:"
                                                    ':$$: 'Text "DP    = Input CHANS :>> Generator CHANS :>> Output"
                                                    ':$$: 'Text "CHANS = Channel CH"
                                                    ':$$: 'Text "CH    = Type | Type :<+> CH"
                                                    ':$$: 'Text "Example: 'Input (Channel (Int :<+> Int)) :>> Generator (Channel (Int :<+> Int)) :>> Output'"
                                                  )

-- type family TyEq (a :: k) (b :: k) :: Bool where
--   TyEq a a = 'True
--   TyEq a b = 'False

-- type family EvalInput (a :: k) :: Bool where
--   EvalInput (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output)
--                                              = EvalInput (ChanIn inToGen)
--   EvalInput (ChanIn (a :<+> more))           = EvalInput (ChanIn more)
--   EvalInput (ChanIn a)                       = 'True
--   EvalInput (ChanOutIn (a :<+> more) ins)    = EvalInput (ChanOutIn more ins)
--   EvalInput (ChanOutIn a ins)                = EvalInput (ChanIn ins)
--   EvalInput x                                = 'False

-- type family EvalGenerator (a :: k) :: Bool where
--   EvalGenerator (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output)
--                                                  = EvalGenerator (ChanIn inToGen)
--   EvalGenerator (ChanIn (a :<+> more))           = EvalGenerator (ChanIn more)
--   EvalGenerator (ChanIn a)                       = 'True
--   EvalGenerator (ChanOutIn (a :<+> more) ins)    = EvalGenerator (ChanOutIn more ins)
--   EvalGenerator (ChanOutIn a ins)                = EvalGenerator (ChanIn ins)
--   EvalGenerator x                                = 'False

-- type family EvalOutput (a :: k) :: Bool where
--   EvalOutput (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output)
--                                               = EvalOutput (ChanOut inToGen)
--   EvalOutput (ChanOut (a :<+> more))          = EvalOutput (ChanOut more)
--   EvalOutput (ChanOut a)                      = 'True
--   EvalOutput x                                = 'False


type family ValidDP (a :: Bool) :: Constraint where
  ValidDP 'True = ()
  ValidDP 'False = TypeError
                    ( 'Text "Invalid Semantic for Building DP Program"
                      ':$$: 'Text "Language Grammar:"
                      ':$$: 'Text "DP    = Input CHANS :>> Generator CHANS :>> Output"
                      ':$$: 'Text "CHANS = Channel CH"
                      ':$$: 'Text "CH    = Type | Type :<+> CH"
                      ':$$: 'Text "Example: 'Input (Channel (Int :<+> Int)) :>> Generator (Channel (Int :<+> Int)) :>> Output'"
                    )

data EndNode
data In (a :: Type)
data Gen (a :: Type)
data Out (a :: Type)

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


-- Associated Type Family
class MkCh (a :: Type) where
  type HChI a :: [Type]
  type HChO a :: [Type]
  mkCh :: Proxy a -> (HList (HChI a), HList (HChO a))

instance MkCh (In (ChanIn more)) => MkCh (In (ChanIn (a :<+> more))) where
  type HChI (In (ChanIn (a :<+> more))) = WriteChannel a ': HChI (In (ChanIn more))
  type HChO (In (ChanIn (a :<+> more))) = ReadChannel a ': HChO (In (ChanIn more))
  mkCh _ = let (i, o) = newChannel @a 
               (il, ol) = mkCh (Proxy @(In (ChanIn more)))
            in (i `HCons` il, o `HCons` ol)

instance MkCh (Out a) where
  type HChI (Out a) = '[]
  type HChO (Out a) = '[]
  mkCh _ = (HNil, HNil)

instance MkCh (Gen (ChanOutIn fromIn more)) => MkCh (Gen (ChanOutIn fromIn (a :<+> more))) where
  type HChI (Gen (ChanOutIn fromIn (a :<+> more))) = WriteChannel a ': HChI (Gen (ChanOutIn fromIn more))
  type HChO (Gen (ChanOutIn fromIn (a :<+> more))) = ReadChannel a ': HChO (Gen (ChanOutIn fromIn more))
  mkCh _ = let (i, o) = newChannel @a 
               (il, ol) = mkCh (Proxy @(Gen (ChanOutIn fromIn more)))
            in (i `HCons` il, o `HCons` ol)

instance MkCh (In (ChanIn EndNode)) where
  type HChI (In (ChanIn EndNode)) = '[]
  type HChO (In (ChanIn EndNode)) = '[]
  mkCh _ = (HNil, HNil) 

instance MkCh (Gen (ChanOutIn fromIn EndNode)) where
  type HChI (Gen (ChanOutIn fromIn EndNode)) = '[]
  type HChO (Gen (ChanOutIn fromIn EndNode)) = '[]
  mkCh _ = (HNil, HNil)

class MkChans (a :: Type) where
  type HChan a :: [Type]
  mkChans :: Proxy a -> Record (HChan a)
  
instance ( MkCh (In (ChanIn (inToGen :<+> EndNode)))
         , MkCh (Gen (ChanOutIn inToGen (genToOut :<+> EndNode)))
         , MkCh (Out (ChanIn (genToOut :<+> EndNode))))
    => MkChans (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) where
 
  type HChan (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) 
    = '[ Tagged "input" (Record '[ Tagged "in-ch" (HList (HChI (In (ChanIn (inToGen :<+> EndNode)))))
                                 , Tagged "out-ch" (HList (HChO (In (ChanIn (inToGen :<+> EndNode)))))
                                 ]
                        )
       , Tagged "generator" (Record '[ Tagged "in-ch" (HList (HChI (Gen (ChanOutIn inToGen (genToOut :<+> EndNode)))))
                                     , Tagged "out-ch" (HList (HChO (Gen (ChanOutIn inToGen (genToOut :<+> EndNode)))))
                                     ]
                            )
       , Tagged "output" (Record '[ Tagged "in-ch" (HList (HChI (Out (ChanIn (genToOut :<+> EndNode)))))])
       ]
    
  mkChans _ = let (ii, io) = mkCh (Proxy @(In (ChanIn (inToGen :<+> EndNode))))
                  (gi, go) = mkCh (Proxy @(Gen (ChanOutIn inToGen (genToOut :<+> EndNode))))
                  (oi, _) = mkCh (Proxy @(Out (ChanIn (genToOut :<+> EndNode))))
               in (inLabel .=. (inChLabel .=. ii .*. outChLabel .=. io .*. emptyRecord))
                   .*. 
                   (genLabel .=. (inChLabel .=. gi .*. outChLabel .=. go .*. emptyRecord))
                   .*. 
                   (outLabel .=. (inChLabel .=. oi .*. emptyRecord))
                   .*.
                   emptyRecord

makeChans :: forall (a :: Type). MkChans a => Record (HChan a)
makeChans = mkChans (Proxy @a)

inputChans :: (LabeledOpticF (LabelableTy r1) (Const t1),
  LabeledOpticP (LabelableTy r1) (->),
  LabeledOpticTo (LabelableTy r1) "in-ch" (->),
  LabeledOpticF (LabelableTy r2) (Const t1),
  LabeledOpticP (LabelableTy r2) (->),
  LabeledOpticTo (LabelableTy r2) "input" (->),
  Labelable "in-ch" r1 s t2 t1 t1,
  Labelable "input" r2 t3 t3 (r1 s) (r1 t2)) =>
  r2 t3 -> t1
inputChans ch = let inl  = hLens' inLabel
                    inch = hLens' inChLabel
                 in view (inl . inch) ch

generatorChans :: (LabeledOpticF (LabelableTy r1) (Const (HList l1)),
  LabeledOpticP (LabelableTy r1) (->),
  LabeledOpticTo (LabelableTy r1) "out-ch" (->),
  LabeledOpticF (LabelableTy r2) (Const (HList l2)),
  LabeledOpticP (LabelableTy r2) (->),
  LabeledOpticTo (LabelableTy r2) "in-ch" (->),
  LabeledOpticF (LabelableTy r3) (Const (HList l2)),
  LabeledOpticP (LabelableTy r3) (->),
  LabeledOpticTo (LabelableTy r3) "generator" (->),
  LabeledOpticF (LabelableTy r3) (Const (HList l1)),
  LabeledOpticTo (LabelableTy r3) "input" (->), HAppendList l1 l2,
  Labelable "generator" r3 t1 t1 (r2 s1) (r2 t2),
  Labelable "in-ch" r2 s1 t2 (HList l2) (HList l2),
  Labelable "input" r3 t1 t1 (r1 s2) (r1 t3),
  Labelable "out-ch" r1 s2 t3 (HList l1) (HList l1)) =>
  r3 t1 -> HList (HAppendListR l1 l2)
generatorChans ch = let inl  = hLens' inLabel
                        genl  = hLens' genLabel
                        inch = hLens' inChLabel
                        outch = hLens' outChLabel
                        outsIn = view (inl . outch) ch
                        insGen = view (genl . inch) ch
                     in outsIn `hAppendList` insGen

outputChans :: (LabeledOpticF (LabelableTy r1) (Const t1),
  LabeledOpticP (LabelableTy r1) (->),
  LabeledOpticTo (LabelableTy r1) "out-ch" (->),
  LabeledOpticF (LabelableTy r2) (Const t1),
  LabeledOpticP (LabelableTy r2) (->),
  LabeledOpticTo (LabelableTy r2) "generator" (->),
  Labelable "generator" r2 t2 t2 (r1 s) (r1 t3),
  Labelable "out-ch" r1 s t3 t1 t1) =>
  r2 t2 -> t1
outputChans ch = let genl  = hLens' genLabel
                     outch = hLens' outChLabel
                  in view (genl . outch) ch

-- Defunctionalization
data Stage a where
  Stage :: forall a (k :: Type -> (Type -> Type) -> Type). Proxy a -> Proxy k -> a -> Stage a

mkStage :: forall a (k :: Type -> (Type -> Type) -> Type). Proxy a -> Proxy k -> a -> Stage a
mkStage = Stage @a @k

mkStage' :: forall a (k :: Type -> (Type -> Type) -> Type). a -> Stage a
mkStage' = Stage @a (Proxy @a) (Proxy @k)

class EvalC l t | l -> t where
  eval :: l -> t

instance forall a b. (a ~ b) => EvalC (Stage a) b where
  eval (Stage _ _ f) = f
  
withInput :: forall (a :: Type) (m :: Type -> Type). WithInput a m -> Stage (WithInput a m)
withInput = mkStage' @(WithInput a m)

withGenerator :: forall (a :: Type) (m :: Type -> Type). WithGenerator a m -> Stage (WithGenerator a m)
withGenerator = mkStage' @(WithGenerator a m)

withOutput :: forall (a :: Type) (m :: Type -> Type). WithOutput a m -> Stage (WithOutput a m)
withOutput = mkStage' @(WithOutput a m)



---------------------------------------------------------------------------------------------------


type DPExample = Input (Channel Int) :>> Generator (Channel Int) :>> Output

input :: Stage (WriteChannel Int -> IO ())
input = withInput @DPExample @IO $ \cout -> forM_ [1..100] (`push'` cout) >> end' cout

generator :: Stage (ReadChannel Int -> WriteChannel Int -> IO ())
generator = mkStage' @(WithGenerator DPExample IO) $ \cin cout -> consumeAll cin $ maybe (end' cout) (flip push' cout . (+1))

output :: Stage (ReadChannel Int -> IO ())
output = mkStage' @(WithOutput DPExample IO) $ \cin -> consumeAll cin print

chanInput :: HList '[WriteChannel Int]
chanInput = inputChans $ makeChans @DPExample

chanGen :: HList '[ReadChannel Int, WriteChannel Int]
chanGen = generatorChans $ makeChans @DPExample

chanOut :: HList '[ReadChannel Int]
chanOut = outputChans $ makeChans @DPExample

-- filter :: Filter (NonEmpty (Actor a)) (..... ) 
-- filter = mkFilter' @FilterC

-- type Actor = MonadState s m => Chans a -> m ()

-- newtype Filter a = Filter { unFilter :: NonEmpty (Actor a)}

prog' :: IO ()
prog' = do 
  let cIns = chanInput
  let cGen = chanGen
  let cOut = chanOut
  hUncurry (eval input) cIns >> hUncurry (eval generator) cGen >> hUncurry (eval output) cOut

consumeAll :: ReadChannel a -> (Maybe a -> IO ()) -> IO ()
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
--   { WriteChannel1 :: Channel (Int, Int)
--   , WriteChannel2 :: Channel [Int]
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
newChannel :: forall a. (WriteChannel a, ReadChannel a)
newChannel = bimap WriteChannel ReadChannel $ unsafePerformIO newChan

{-# INLINE end' #-}
end' :: WriteChannel a -> IO ()
end' = flip writeChan Nothing . unWrite

-- {-# INLINE endIn #-}
-- endIn :: Stage a b f -> IO ()
-- endIn = end' . inChannel

-- {-# INLINE endOut #-}
-- endOut :: Stage a b -> IO ()
-- endOut = end' . outChannel

{-# INLINE push' #-}
push' :: a -> WriteChannel a -> IO ()
push' a c = writeChan (unWrite c) (Just a) 

-- {-# INLINE pushOut #-}
-- pushOut :: b -> Stage a b -> IO ()
-- pushOut e = push' e . outChannel

-- {-# INLINE pushIn #-}
-- pushIn :: a -> Stage a b -> IO ()
-- pushIn e = push' e . inChannel

{-# INLINE pull' #-}
pull' :: ReadChannel a -> IO (Maybe a)
pull' = readChan (CC.threadDelay 100) . unRead

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
