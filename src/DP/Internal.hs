{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-unused-foralls #-}
module DP.Internal where

import qualified Control.Concurrent            as CC
import Control.Concurrent.Async
import Control.Concurrent.Chan.Unagi.NoBlocking                                                      
import Relude as R
import GHC.TypeLits
import Data.List.NonEmpty
import Data.HList
import Data.HList.Labelable
import System.IO.Unsafe
import Control.Lens hiding ((<|))

newtype WriteChannel a = WriteChannel { unWrite :: InChan (Maybe a) }
newtype ReadChannel a = ReadChannel { unRead :: OutChan (Maybe a) }

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


type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And a b         = 'False

type family IsDP (a :: k) :: Bool where
  IsDP (Input (Channel inToGen)
        :>> Generator (Channel genToOut)
        :>> Output)
                                            = And (IsDP (Input (Channel inToGen))) (IsDP (Generator (Channel genToOut)))
  IsDP (Input (Channel (a :<+> more)))      = IsDP (Input (Channel more))
  IsDP (Input (Channel a))                  = 'True
  IsDP (Generator (Channel (a :<+> more)))  = IsDP (Generator (Channel more))
  IsDP (Generator (Channel a))              = 'True
  IsDP x                                    = 'False


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

type family WithFilter (a :: Type) (param :: Type) (m :: Type -> Type) :: Type where
  WithFilter (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) param m                         
                                                    = param -> WithFilter (ChanOutIn inToGen genToOut) param m
  WithFilter (ChanIn (a :<+> more)) param m         = WriteChannel a -> WithFilter (ChanIn more) param m
  WithFilter (ChanIn a) param m                     = WriteChannel a -> m ()
  WithFilter (ChanOutIn (a :<+> more) ins) param m  = ReadChannel a -> WithFilter (ChanOutIn more ins) param m
  WithFilter (ChanOutIn a ins) param m              = ReadChannel a -> WithFilter (ChanIn ins) param m 
  WithFilter a _ _                                  = TypeError
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

-- Type encoding for Building Chans. Only for internal use in the Associated Type Family and combinators of MkCh and MkChans
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

type family ExpandInputToCh (a :: Type) :: [Type] where
  ExpandInputToCh (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) = HChI (In (ChanIn (inToGen :<+> EndNode)))

type family ExpandGenToCh (a :: Type) :: [Type] where
  ExpandGenToCh (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) = HAppendListR (HChO (In (ChanIn (inToGen :<+> EndNode)))) 
                                                                                                     (HChI (Gen (ChanOutIn inToGen (genToOut :<+> EndNode))))

type family ExpandFilterToCh (a :: Type) (param :: Type) :: [Type] where
  ExpandFilterToCh (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) param = param ': HAppendListR (HChO (In (ChanIn (inToGen :<+> EndNode)))) 
                                                                                                     (HChI (Gen (ChanOutIn inToGen (genToOut :<+> EndNode))))

type family ExpandOutputToCh (a :: Type) :: [Type] where
  ExpandOutputToCh (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) = HChO (Gen (ChanOutIn inToGen (genToOut :<+> EndNode)))

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

inGenOut :: AllChans r2 r3 l1 r4 l2 t2 s t1 s2 t5 l3 l4 => r3 t2 -> (HList l3, HList (HAppendListR l1 l2), HList l4)
inGenOut ch = (inputChans ch, generatorChans ch, outputChans ch)

-- Defunctionalization
data Stage a where
  Stage :: Proxy a -> a -> Stage a

mkStage :: forall a. Proxy a -> a -> Stage a
mkStage = Stage @a

mkStage' :: forall a. a -> Stage a
mkStage' = Stage (Proxy @a)

class EvalC l t | l -> t where
  run :: l -> t

instance forall a b. (a ~ b) => EvalC (Stage a) b where
  run (Stage _ f) = f

runStage :: forall (n :: HNat) f (xs :: [*]) r. (HCurry' n f xs r, ArityFwd f n, ArityRev f n) => f -> HList xs -> r
runStage = hUncurry

runStage' :: HCurry' n f xs r => Proxy n -> f -> HList xs -> r
runStage' = hUncurry'

newtype Actor s m a param = Actor {  unActor :: MonadState s m => Stage (WithFilter a param m) }

newtype Filter s m a param = Filter { unFilter :: NonEmpty (Actor s m a param) }
  deriving Generic

instance Wrapped (Filter s m a param)

mkFilter :: forall s m a param. WithFilter a param m -> Filter s m a param
mkFilter = Filter . single

single :: forall s m a param. WithFilter a param m -> NonEmpty (Actor s m a param)
single = one . actor 

actor :: forall s m a param. WithFilter a param m -> Actor s m a param
actor = Actor . mkStage' @(WithFilter a param m)

(|>>) :: forall s m a param. WithFilter a param m -> Filter s m a param -> Filter s m a param
(|>>) a f = f & _Wrapped' %~ (\p -> actor a <| p)
infixr 5 |>>

runActor :: forall a param m xs c s. ( ArityRev (WithFilter a param m) (HLength (ExpandFilterToCh a param))
                                     , ArityFwd (WithFilter a param m) (HLength (ExpandFilterToCh a param))
                                     , HCurry' (HLength (ExpandFilterToCh a param)) (WithFilter a param m) xs c, MonadState s m) 
          => Actor s m a param -> HList xs -> c
runActor ac = runStage . run $ unActor ac

runFilter :: ( ArityRev (WithFilter a param m1) (HLength (ExpandFilterToCh a param))
             , ArityFwd (WithFilter a param m1) (HLength (ExpandFilterToCh a param))
             , HCurry' (HLength (ExpandFilterToCh a param)) (WithFilter a param m1) xs (StateT s1 m2 b), MonadState s2 m1, Monad m2) 
           => HList xs -> Filter s2 m1 a param -> s1 -> m2 ()
runFilter clist f s = flip evalStateT s . mapM_ (`runActor` clist) . unFilter $ f 

-- >>> let (cIns, cGen, cOut) = inGenOut $ makeChans @DPExample
-- >>> let p = (1::Int) `HCons` cGen
-- >>> runFilter p b (1::Int)
-- Couldn't match expected type ‘HList xs0’
--             with actual type ‘Filter Int (StateT Int IO) DPExample Int’
-- Couldn't match expected type ‘Filter s20 m10 a0 param0’
--             with actual type ‘HList '[Int, ReadChannel Int, WriteChannel Int]’
b :: Filter Int (StateT Int IO) DPExample Int
b =  (\i _ _ -> print i) |>> mkFilter (\i _ _ -> print (i+1))

withInput :: forall (a :: Type) (m :: Type -> Type). WithInput a m -> Stage (WithInput a m)
withInput = mkStage' @(WithInput a m)

withGenerator :: forall (a :: Type) (m :: Type -> Type). WithGenerator a m -> Stage (WithGenerator a m)
withGenerator = mkStage' @(WithGenerator a m)

withOutput :: forall (a :: Type) (m :: Type -> Type). WithOutput a m -> Stage (WithOutput a m)
withOutput = mkStage' @(WithOutput a m)

data DynamicPipeline a = ValidDP (IsDP a) => DynamicPipeline 
  { onInput     :: Stage (WithInput a IO)
  , onGenerator :: Stage (WithGenerator a IO)
  , onOutput    :: Stage (WithOutput a IO)
  }

runDP :: forall a r2 r3 l1 r4 l2 t2 s t1 s2 t5 l3 l4. 
                                  ( MkChans a
                                  , Record (HChan a) ~ r3 t2
                                  , ArityRev (WithInput a IO) (HLength (ExpandInputToCh a))
                                  , ArityFwd (WithInput a IO) (HLength (ExpandInputToCh a))
                                  , HCurry' (HLength (ExpandInputToCh a)) (WithInput a IO) l3 (IO ())
                                  , ArityRev (WithGenerator a IO) (HLength (ExpandGenToCh a))
                                  , ArityFwd (WithGenerator a IO) (HLength (ExpandGenToCh a))
                                  , HCurry' (HLength (ExpandGenToCh a)) (WithGenerator a IO) (HAppendListR l1 l2) (IO ())
                                  , ArityRev (WithOutput a IO) (HLength (ExpandOutputToCh a))
                                  , ArityFwd (WithOutput a IO) (HLength (ExpandOutputToCh a))
                                  , HCurry' (HLength (ExpandOutputToCh a)) (WithOutput a IO) l4 (IO ())
                                  , AllChans r2 r3 l1 r4 l2 t2 s t1 s2 t5 l3 l4)
                                  => DynamicPipeline a -> IO ()
runDP DynamicPipeline{..} = do
  let (cIns, cGen, cOut) = inGenOut $ makeChans @a
  async (runStage (run onInput) cIns) >> async (runStage (run onGenerator) cGen) >> async (runStage (run onOutput) cOut) >>= wait

mkDP :: forall a. ValidDP (IsDP a) => Stage (WithInput a IO) -> Stage (WithGenerator a IO) -> Stage (WithOutput a IO) -> DynamicPipeline a
mkDP = DynamicPipeline @a
{-# INLINE forall #-}
forall :: ReadChannel a -> (Maybe a -> IO ()) -> IO ()
forall c io = do 
  e <- pull c
  io e
  maybe (pure ()) (const $ forall c io) e

{-# NOINLINE newChannel #-}
newChannel :: forall a. (WriteChannel a, ReadChannel a)
newChannel = bimap WriteChannel ReadChannel $ unsafePerformIO newChan

{-# INLINE end #-}
end :: WriteChannel a -> IO ()
end = flip writeChan Nothing . unWrite

{-# INLINE push #-}
push :: a -> WriteChannel a -> IO ()
push a c = writeChan (unWrite c) (Just a) 

{-# INLINE pull #-}
pull :: ReadChannel a -> IO (Maybe a)
pull = readChan (CC.threadDelay 100) . unRead


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

---------------------------------------------------------------------------------------------------

type DPExample = Input (Channel Int) :>> Generator (Channel Int) :>> Output

input :: Stage (WriteChannel Int -> IO ())
input = withInput @DPExample @IO $ \cout -> forM_ [1..100] (`push` cout) >> end cout

generator :: Stage (ReadChannel Int -> WriteChannel Int -> IO ())
generator = withGenerator @DPExample @IO $ \cin cout -> forall cin $ maybe (end cout) (flip push cout . (+1))

output :: Stage (ReadChannel Int -> IO ())
output = withOutput @DPExample @IO $ \cin -> forall cin print

program :: IO ()
program = runDP $ mkDP @DPExample input generator output

-- b :: (MonadIO m, MonadState Int m) => Filter Int m a
-- b = (modify (+2) >> get >>= print . mappend "Hello: " . show) |>> actor (get >>= print . mappend "Hello 2: " . show)


