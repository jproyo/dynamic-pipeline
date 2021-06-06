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

data ChansFilter (a :: Type)

data ChanWriteInput (a :: Type)
data ChanReadWriteGen (a :: Type)
data ChanReadOut (a :: Type)

data Input (a :: Type)
data Generator (a :: Type)
data Output

data Eof

type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And a b         = 'False

type family IsDP (a :: k) :: Bool where
  IsDP (Input (Channel inToGen)
        :>> Generator (Channel genToOut)
        :>> Output)
                                            = And (IsDP (Input (Channel inToGen))) (IsDP (Generator (Channel genToOut)))
  IsDP (Input (Channel (a :<+> more)))      = IsDP (Input (Channel more))
  IsDP (Input (Channel Eof))                = 'True
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
  WithInput (ChanIn Eof) m                   = m ()
  WithInput (ChanOutIn (a :<+> more) ins) m  = ReadChannel a -> WithInput (ChanOutIn more ins) m
  WithInput (ChanOutIn Eof ins) m            = WithInput (ChanIn ins) m 
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

type family WithGenerator (a :: Type) (filter :: Type) (m :: Type -> Type) :: Type where
  WithGenerator (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) filter m                         
                                                        = filter -> WithGenerator (ChanOutIn inToGen genToOut) filter m
  WithGenerator (ChanIn (a :<+> more)) filter m         = WriteChannel a -> WithGenerator (ChanIn more) filter m
  WithGenerator (ChanIn Eof) filter m                   = m ()
  WithGenerator (ChanOutIn (a :<+> more) ins) filter m  = ReadChannel a -> WithGenerator (ChanOutIn more ins) filter m
  WithGenerator (ChanOutIn Eof ins) filter m            = WithGenerator (ChanIn ins) filter m 
  WithGenerator a _ _                                   = TypeError
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
  WithFilter (ChanIn Eof) param m                   = m ()
  WithFilter (ChanOutIn (a :<+> more) ins) param m  = ReadChannel a -> WithFilter (ChanOutIn more ins) param m
  WithFilter (ChanOutIn Eof ins) param m            = WithFilter (ChanIn ins) param m 
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
  WithOutput (ChanOut Eof) m                   = m ()
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
  mkCh :: Proxy a -> IO (HList (HChI a), HList (HChO a))

instance MkCh more => MkCh (a :<+> more) where
  type HChI (a :<+> more) = WriteChannel a ': HChI more
  type HChO (a :<+> more) = ReadChannel a ': HChO more
  mkCh _ = do 
    (i, o) <- newChannel @a 
    (il, ol) <- mkCh (Proxy @more)
    return (i `HCons` il, o `HCons` ol)

instance MkCh Eof where
  type HChI Eof = '[]
  type HChO Eof = '[]
  mkCh _ = return (HNil, HNil)

type family ExpandToHList (a :: Type) (param :: Type) :: [Type]
type instance ExpandToHList (ChanWriteInput (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output)) _ = 
  HChI inToGen
type instance ExpandToHList (ChanReadWriteGen (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output)) filter = 
  filter ': HAppendListR (HChO inToGen) (HChI genToOut)
type instance ExpandToHList (ChanReadOut (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output)) filter = 
  HChO genToOut

type ExpandInputToCh a = ExpandToHList (ChanWriteInput a) Void
type ExpandGenToCh a filter = ExpandToHList (ChanReadWriteGen a) filter
type ExpandFilterToCh a param = ExpandGenToCh a param
type ExpandOutputToCh a = ExpandToHList (ChanReadOut a) Void

class MkChans (a :: Type) where
  type HChan a :: Type
  mkChans :: Proxy a -> IO (HChan a)
  
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

instance MkCh inToGen
    => MkChans (ChansFilter (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output)) where
 
  type HChan (ChansFilter (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output))
    = Record '[ Tagged "in-ch" (HList (HChO inToGen))
              , Tagged "out-ch" (HList (HChI inToGen))
              ]
    
  mkChans _ =  do
    (writes', reads') <- mkCh (Proxy @inToGen)
    return $ mkRecord (inChLabel .=. reads' `HCons` outChLabel .=. writes' `HCons` HNil)


makeChans :: forall (a :: Type). MkChans a => IO (HChan a)
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

data GeneratorStage s m a param = GeneratorStage 
  { _gsGenerator      :: Stage (WithGenerator a (Filter s m a param) m)
  , _gsFilterTemplate :: Filter s m a param
  }

mkGenerator :: Stage (WithGenerator a (Filter s m a param) m) -> Filter s m a param -> GeneratorStage s m a param
mkGenerator = GeneratorStage

newtype Actor s m a param = Actor {  unActor :: MonadState s m => Stage (WithFilter a param m) }

newtype Filter s m a param = Filter { unFilter :: NonEmpty (Actor s (StateT s m) a param) }
  deriving Generic

instance Wrapped (Filter s m a param)

mkFilter :: forall s m a param. WithFilter a param (StateT s m) -> Filter s m a param
mkFilter = Filter . single

single :: forall s m a param. WithFilter a param (StateT s m) -> NonEmpty (Actor s (StateT s m) a param)
single = one . actor 

actor :: forall s m a param. WithFilter a param m -> Actor s m a param
actor = Actor . mkStage' @(WithFilter a param m)

(|>>>) :: forall s m a param. Actor s (StateT s m) a param -> Filter s m a param -> Filter s m a param
(|>>>) a f = f & _Wrapped' %~ (a <|)
infixr 5 |>>>

(|>>) :: forall s m a param. Actor s (StateT s m) a param -> Actor s (StateT s m) a param -> Filter s m a param
(|>>) a1 a2 = Filter (a1 <|one a2)
infixr 5 |>>

runActor :: ( ArityRev (WithFilter a param m) (HLength (ExpandFilterToCh a param))
            , ArityFwd (WithFilter a param m) (HLength (ExpandFilterToCh a param))
            , HCurry' (HLength (ExpandFilterToCh a param)) (WithFilter a param m) xs r, MonadState s m)
         => Actor s m a param -> HList xs -> r
runActor ac = runStage . run $ unActor ac

runFilter :: ( ArityRev (WithFilter a param (StateT s1 m1)) (HLength (ExpandFilterToCh a param))
             , ArityFwd (WithFilter a param (StateT s1 m1)) (HLength (ExpandFilterToCh a param))
             , HCurry' (HLength (ExpandFilterToCh a param)) (WithFilter a param (StateT s1 m1)) xs (StateT s2 m2 b)
             , Monad m2, Monad m1) 
           => HList xs -> Filter s1 m1 a param -> s2 -> m2 ()
runFilter clist f s = flip evalStateT s . mapM_ (`runActor` clist) . unFilter $ f 

withInput :: forall (a :: Type) (m :: Type -> Type). WithInput a m -> Stage (WithInput a m)
withInput = mkStage' @(WithInput a m)

withGenerator :: forall (a :: Type) (filter :: Type) (m :: Type -> Type). WithGenerator a filter m -> Stage (WithGenerator a filter m)
withGenerator = mkStage' @(WithGenerator a filter m)

withOutput :: forall (a :: Type) (m :: Type -> Type). WithOutput a m -> Stage (WithOutput a m)
withOutput = mkStage' @(WithOutput a m)

data DynamicPipeline a s param = ValidDP (IsDP a) => DynamicPipeline 
  { input     :: Stage (WithInput a IO)
  , generator :: GeneratorStage s IO a param 
  , output    :: Stage (WithOutput a IO)
  }

runDP :: forall a s param filter r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4. 
                                  ( MkChans a
                                  , HChan a ~ r3 t2
                                  , Filter s IO a param ~ filter
                                  , ArityRev (WithInput a IO) (HLength (ExpandInputToCh a))
                                  , ArityFwd (WithInput a IO) (HLength (ExpandInputToCh a))
                                  , HCurry' (HLength (ExpandInputToCh a)) (WithInput a IO) l3 (IO ())
                                  , ArityRev (WithGenerator a filter IO) (HLength (ExpandGenToCh a filter))
                                  , ArityFwd (WithGenerator a filter IO) (HLength (ExpandGenToCh a filter))
                                  , HCurry' (HLength (ExpandGenToCh a filter)) (WithGenerator a filter IO) (filter ': HAppendListR l1 l2) (IO ())
                                  , ArityRev (WithOutput a IO) (HLength (ExpandOutputToCh a))
                                  , ArityFwd (WithOutput a IO) (HLength (ExpandOutputToCh a))
                                  , HCurry' (HLength (ExpandOutputToCh a)) (WithOutput a IO) l4 (IO ())
                                  , AllChans r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4)
                                  => DynamicPipeline a s param -> IO ()
runDP DynamicPipeline{..} = do
  (cIns, cGen, cOut) <- inGenOut <$> makeChans @a
  let genWithFilter      = _gsFilterTemplate generator `HCons` cGen
  _ <- async (runStage (run input) cIns)
  _ <- async (runStage @(HLength (ExpandGenToCh a filter)) @(WithGenerator a filter IO) (run (_gsGenerator generator)) genWithFilter)
  oa <- async (runStage (run output) cOut) 
  wait oa

mkDP :: forall a s param. ValidDP (IsDP a) => Stage (WithInput a IO) -> GeneratorStage s IO a param -> Stage (WithOutput a IO) -> DynamicPipeline a s param
mkDP = DynamicPipeline @a

{-# INLINE forall #-}
forall :: ReadChannel a -> (a -> IO ()) -> IO ()
forall = loop'
  where 
    loop' c io = maybe (pure ()) (\e -> io e >> loop' c io) =<< pull c

{-# NOINLINE newChannel #-}
newChannel :: forall a. IO (WriteChannel a, ReadChannel a)
newChannel = bimap WriteChannel ReadChannel <$> newChan

{-# INLINE end #-}
end :: WriteChannel a -> IO ()
end = flip writeChan Nothing . unWrite

{-# INLINE push #-}
push :: a -> WriteChannel a -> IO ()
push a c = writeChan (unWrite c) (Just a) 

{-# INLINE pull #-}
pull :: ReadChannel a -> IO (Maybe a)
pull = readChan (CC.threadDelay 100) . unRead


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


spawnFilterForAll :: forall a b s param l r t l1 b0 l2 l3 b2 b3 l4. 
                ( MkChans (ChansFilter a)
                , FilterChans r (HList l3) t (HList (ReadChannel b : l1))
                , l1 ~ l
                , HAppendList l l3
                , l4 ~ HAppendListR l l3
                , l2 ~ (b ': ReadChannel b ': l4)
                , HChan (ChansFilter a) ~ r t
                , WithFilter a param (StateT s IO) ~ (b2 -> ReadChannel b2 -> b3)
                , HLength (ExpandFilterToCh a param) ~ HLength l2
                , HCurry' (HLength l2) (WithFilter a param (StateT s IO)) l2 (StateT s IO b0)
                , ArityFwd (WithFilter a param (StateT s IO)) (HLength (ExpandFilterToCh a param))
                , ArityRev b3 (HLength l4)
                )
                => ReadChannel b 
                -> HList l
                -> Filter s IO a param 
                -> (b -> s)
                -> (b -> IO ())
                -> IO (HList l)
spawnFilterForAll cin restIns filter' initState = 
  spawnFilterWith cin restIns filter' initState (const True)


spawnFilterWith :: forall a b s param l r t l1 b0 l2 l3 b2 b3 l4. 
                ( MkChans (ChansFilter a)
                , FilterChans r (HList l3) t (HList (ReadChannel b : l1))
                , l1 ~ l
                , HAppendList l l3
                , l4 ~ HAppendListR l l3
                , l2 ~ (b ': ReadChannel b ': l4)
                , HChan (ChansFilter a) ~ r t
                , WithFilter a param (StateT s IO) ~ (b2 -> ReadChannel b2 -> b3)
                , HLength (ExpandFilterToCh a param) ~ HLength l2
                , HCurry' (HLength l2) (WithFilter a param (StateT s IO)) l2 (StateT s IO b0)
                , ArityFwd (WithFilter a param (StateT s IO)) (HLength (ExpandFilterToCh a param))
                , ArityRev b3 (HLength l4)
                )
                => ReadChannel b 
                -> HList l
                -> Filter s IO a param 
                -> (b -> s)
                -> (b -> Bool)
                -> (b -> IO ())
                -> IO (HList l)
spawnFilterWith cin restIns filter' initState spawnIf onElem = 
  loopSpawn filter' initState spawnIf onElem cin restIns

loopSpawn :: forall a b s param l r t l1 b0 l2 l3 b2 b3 l4. 
                ( MkChans (ChansFilter a)
                , FilterChans r (HList l3) t (HList (ReadChannel b : l1))
                , l1 ~ l
                , HAppendList l l3
                , l4 ~ HAppendListR l l3
                , l2 ~ (b ': ReadChannel b ': l4)
                , HChan (ChansFilter a) ~ r t
                , WithFilter a param (StateT s IO) ~ (b2 -> ReadChannel b2 -> b3)
                , HLength (ExpandFilterToCh a param) ~ HLength l2
                , HCurry' (HLength l2) (WithFilter a param (StateT s IO)) l2 (StateT s IO b0)
                , ArityFwd (WithFilter a param (StateT s IO)) (HLength (ExpandFilterToCh a param))
                , ArityRev b3 (HLength l4)
                )
                => Filter s IO a param 
                -> (b -> s)
                -> (b -> Bool)
                -> (b -> IO ())
                -> ReadChannel b 
                -> HList l
                -> IO (HList l)
loopSpawn filter'' initState' spawnIf' onElem' cin' restIns' = 
  maybe (pure restIns') (whenNewElem cin' restIns' filter'' initState' spawnIf' onElem') 
  =<< pull cin'

whenNewElem :: forall a b s param l r t l1 b0 l2 l3 b2 b3 l4. 
                ( MkChans (ChansFilter a)
                , FilterChans r (HList l3) t (HList (ReadChannel b : l1))
                , l1 ~ l
                , HAppendList l l3
                , l4 ~ HAppendListR l l3
                , l2 ~ (b ': ReadChannel b ': l4)
                , HChan (ChansFilter a) ~ r t
                , WithFilter a param (StateT s IO) ~ (b2 -> ReadChannel b2 -> b3)
                , HLength (ExpandFilterToCh a param) ~ HLength l2
                , HCurry' (HLength l2) (WithFilter a param (StateT s IO)) l2 (StateT s IO b0)
                , ArityFwd (WithFilter a param (StateT s IO)) (HLength (ExpandFilterToCh a param))
                , ArityRev b3 (HLength l4)
                )
                => ReadChannel b 
                -> HList l
                -> Filter s IO a param 
                -> (b -> s)
                -> (b -> Bool)
                -> (b -> IO ())
                -> b
                -> IO (HList l)
whenNewElem cin' restIns' filter'' initState' spawnIf' onElem' = 
  uncurry (loopSpawn filter'' initState' spawnIf' onElem') <=< doOnElem cin' restIns' filter'' initState' spawnIf' onElem'

doOnElem :: forall a b s param l r t l1 b0 l2 l3 b2 b3 l4. 
                ( MkChans (ChansFilter a)
                , FilterChans r (HList l3) t (HList (ReadChannel b : l1))
                , l1 ~ l
                , HAppendList l l3
                , l4 ~ HAppendListR l l3
                , l2 ~ (b ': ReadChannel b ': l4)
                , HChan (ChansFilter a) ~ r t
                , WithFilter a param (StateT s IO) ~ (b2 -> ReadChannel b2 -> b3)
                , HLength (ExpandFilterToCh a param) ~ HLength l2
                , HCurry' (HLength l2) (WithFilter a param (StateT s IO)) l2 (StateT s IO b0)
                , ArityFwd (WithFilter a param (StateT s IO)) (HLength (ExpandFilterToCh a param))
                , ArityRev b3 (HLength l4)
                )
                => ReadChannel b 
                -> HList l
                -> Filter s IO a param 
                -> (b -> s)
                -> (b -> Bool)
                -> (b -> IO ())
                -> b
                -> IO (ReadChannel b, HList l1)
doOnElem cin' restIns' filter'' initState' spanwIf' onElem' elem' = do
  onElem' elem'
  if spanwIf' elem' 
    then do 
      (reads', writes' :: HList l3) <- getFilterChannels <$> makeChans @(ChansFilter a)
      let hlist = elem' `HCons` cin' `HCons` (restIns' `hAppendList` writes')
      void $ async (runFilter hlist filter'' (initState' elem'))
      return (hHead reads', hTail reads')
    else return (cin', restIns')

type FilterChans r b t a = (LabeledOpticF (LabelableTy r) (Const b),
                      LabeledOpticTo (LabelableTy r) "out-ch" (->),
                      Labelable "out-ch" r t t b b,
                      LabeledOpticF (LabelableTy r) (Const a),
                      LabeledOpticP (LabelableTy r) (->),
                      LabeledOpticTo (LabelableTy r) "in-ch" (->),
                      Labelable "in-ch" r t t a a)

getFilterChannels :: FilterChans r b t a => r t -> (a, b)
getFilterChannels ch = 
   let inch = hLens' inChLabel
       outch = hLens' outChLabel
       reads' = ch^.inch 
       writes' = ch^.outch 
    in (reads', writes')