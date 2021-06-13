{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}
module DynamicPipeline.Stage where

import           Control.Concurrent.Async
import           Control.Lens             hiding ((<|))
import           Control.Monad.Indexed
import           Data.HList
import           Data.List.NonEmpty
import           DynamicPipeline.Channel
import           GHC.TypeLits
import           Relude                   as R


-- Type Level Functions: Boolean And
type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And a b         = 'False

-- Type Level Functions: Validation of DP Construct at Type Level
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

-- Inductive Type Family for Expanding and building Input, Generator, Filter and Output Functions Signatures
type family WithInput (a :: Type) (m :: Type -> Type) :: Type where
  WithInput (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) m = WithInput (ChanIn inToGen) m
  WithInput (ChanIn (a :<+> more)) m         = WriteChannel a -> WithInput (ChanIn more) m
  WithInput (ChanIn Eof) m                   = m ()
  WithInput (ChanOutIn (a :<+> more) ins) m  = ReadChannel a -> WithInput (ChanOutIn more ins) m
  WithInput (ChanOutIn Eof ins) m            = WithInput (ChanIn ins) m
  WithInput a _                              = TypeError
                                                  ( 'Text "Invalid Semantic for Input Stage"
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
  WithGenerator (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) filter m = filter -> WithGenerator (ChanOutIn inToGen genToOut) filter m
  WithGenerator (ChanIn (a :<+> more)) filter m         = WriteChannel a -> WithGenerator (ChanIn more) filter m
  WithGenerator (ChanIn Eof) filter m                   = m ()
  WithGenerator (ChanOutIn (a :<+> more) ins) filter m  = ReadChannel a -> WithGenerator (ChanOutIn more ins) filter m
  WithGenerator (ChanOutIn Eof ins) filter m            = WithGenerator (ChanIn ins) filter m
  WithGenerator a _ _                                   = TypeError
                                                            ( 'Text "Invalid Semantic for Generator Stage"
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
                                                ( 'Text "Invalid Semantic Semantic for Generator Stage"
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
  WithOutput a _                               = TypeError
                                                  ( 'Text "Invalid Semantic for Output Stage"
                                                    ':$$: 'Text "in the type '"
                                                    ':<>: 'ShowType a
                                                    ':<>: 'Text "'"
                                                    ':$$: 'Text "Language Grammar:"
                                                    ':$$: 'Text "DP    = Input CHANS :>> Generator CHANS :>> Output"
                                                    ':$$: 'Text "CHANS = Channel CH"
                                                    ':$$: 'Text "CH    = Type | Type :<+> CH"
                                                    ':$$: 'Text "Example: 'Input (Channel (Int :<+> Int)) :>> Generator (Channel (Int :<+> Int)) :>> Output'"
                                                  )



-- Monad Index
newtype Ix m i j a = Ix { unsafeRunIx :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Functor m => IxFunctor (Ix m) where
  imap = fmap
instance Applicative m => IxPointed (Ix m) where
  ireturn = pure

instance Applicative m => IxApplicative (Ix m) where
  iap :: forall i j k a b. Ix m i j (a -> b)
      -> Ix m j k a
      -> Ix m i k b
  iap = coerce $ (<*>) @m @a @b

instance Monad m => IxMonad (Ix m) where
  ibind :: forall i j k a b. (a -> Ix m j k b)
        -> Ix m i j a
        -> Ix m i k b
  ibind = coerce $ (=<<) @m @a @b

-- Stages Definitions
data ChannelState = ChannelState
  { channelNext   :: Nat
  , channelOpened :: [Nat]
  }

newtype DP s a = DP
  { runStage :: IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

withDP :: IO a -> DP s a
withDP = DP

-- Defunctionalization
data Stage a where
  Stage :: Proxy a -> a -> Stage a

{-# INLINE mkStage #-}
mkStage :: forall a. Proxy a -> a -> Stage a
mkStage = Stage @a

{-# INLINE mkStage' #-}
mkStage' :: forall a. a -> Stage a
mkStage' = Stage (Proxy @a)

class EvalC l t | l -> t where
  run :: l -> t

instance forall a b. (a ~ b) => EvalC (Stage a) b where
  run (Stage _ f) = f

{-# INLINE runStageWith #-}
runStageWith :: forall (n :: HNat) f (xs :: [*]) s.
            (HCurry' n f xs (DP s ()), ArityFwd f n, ArityRev f n, CloseList xs)
            => Stage f -> HList xs -> DP s (Async ())
runStageWith fn cIns = withDP $ async (runStage (hUncurry (run fn) cIns) >> closeList cIns)

{-# INLINE runStageWith' #-}
runStageWith' :: forall (n :: HNat) f (xs :: [*]) (ss :: [*]) s.
            (HCurry' n f xs (DP s ()), ArityFwd f n, ArityRev f n, CloseList ss)
            => Stage f -> HList xs -> HList ss -> DP s (Async ())
runStageWith' fn cIns cClose = withDP (async (runStage (hUncurry (run fn) cIns) >> closeList cClose))

-- Generator and Filter specific Definitions
data DynamicPipeline dpDefinition filterState filterParam st =
  DynamicPipeline
    { input     :: Stage (WithInput dpDefinition (DP st))
    , generator :: GeneratorStage dpDefinition filterState filterParam st
    , output    :: Stage (WithOutput dpDefinition (DP st))
    }

data GeneratorStage dpDefinition filterState filterParam st = GeneratorStage
  { _gsGenerator      :: Stage (WithGenerator dpDefinition (Filter dpDefinition filterState filterParam st) (DP st))
  , _gsFilterTemplate :: Filter dpDefinition filterState filterParam st
  }
newtype Actor dpDefinition filterState filterParam monadicAction = 
  Actor {  unActor :: MonadState filterState monadicAction => Stage (WithFilter dpDefinition filterParam monadicAction) }

newtype Filter dpDefinition filterState filterParam st =
  Filter { unFilter :: NonEmpty (Actor dpDefinition filterState filterParam (StateT filterState (DP st))) }
  deriving Generic

instance Wrapped (Filter s' s a param)

{-# INLINE mkGenerator #-}
mkGenerator :: Stage (WithGenerator dpDefinition (Filter dpDefinition filterState filterParam st) (DP st)) 
            -> Filter dpDefinition filterState filterParam st 
            -> GeneratorStage dpDefinition filterState filterParam st
mkGenerator = GeneratorStage

{-# INLINE mkFilter #-}
mkFilter :: forall dpDefinition filterState filterParam st. WithFilter dpDefinition filterParam (StateT filterState (DP st))
         -> Filter dpDefinition filterState filterParam st 
mkFilter = Filter . single

{-# INLINE single #-}
single :: forall dpDefinition filterState filterParam st.  WithFilter dpDefinition filterParam (StateT filterState (DP st)) 
       -> NonEmpty (Actor dpDefinition filterState filterParam (StateT filterState (DP st)))
single = one . actor

{-# INLINE actor #-}
actor :: forall dpDefinition filterState filterParam st. WithFilter dpDefinition filterParam (StateT filterState (DP st)) 
      -> Actor dpDefinition filterState filterParam (StateT filterState (DP st))
actor = Actor . mkStage' @(WithFilter dpDefinition filterParam (StateT filterState (DP st)))

{-# INLINE (|>>>) #-}
(|>>>) :: forall dpDefinition filterState filterParam st. Actor dpDefinition filterState filterParam (StateT filterState (DP st)) 
       -> Filter dpDefinition filterState filterParam st -> Filter dpDefinition filterState filterParam st 
(|>>>) a f = f & _Wrapped' %~ (a <|)
infixr 5 |>>>

{-# INLINE (|>>) #-}
(|>>) :: forall dpDefinition filterState filterParam st. Actor dpDefinition filterState filterParam (StateT filterState (DP st)) 
      -> Actor dpDefinition filterState filterParam (StateT filterState (DP st)) -> Filter dpDefinition filterState filterParam st 
(|>>) a1 a2 = Filter (a1 <|one a2)
infixr 5 |>>

{-# INLINE runActor #-}
runActor :: ( MonadState filterState monadicAction
            , HCurry' n (WithFilter dpDefinition filterParam monadicAction) xs r
            , ArityFwd (WithFilter dpDefinition filterParam monadicAction) n
            , ArityRev (WithFilter dpDefinition filterParam monadicAction) n
            ) => Actor dpDefinition filterState filterParam monadicAction -> HList xs -> r
runActor = hUncurry . run . unActor

{-# INLINE runFilter #-}
runFilter :: ( CloseList ss
             , HCurry' n (WithFilter dpDefinition filterParam (StateT filterState (DP st))) xs (StateT filterState2 (DP st) ())
             , ArityFwd (WithFilter dpDefinition filterParam (StateT filterState (DP st))) n
             , ArityRev (WithFilter dpDefinition filterParam (StateT filterState (DP st))) n
             ) => Filter dpDefinition filterState filterParam st -> filterState2 -> HList xs -> HList ss -> DP st (Async ())
runFilter f s clist cClose = DP $ async $ do
  void . runStage . flip evalStateT s  . mapM_ (`runActor` clist) . unFilter $ f
  closeList cClose

{-# INLINE withInput #-}
withInput :: forall (a :: Type) s. WithInput a (DP s) -> Stage (WithInput a (DP s))
withInput = mkStage' @(WithInput a (DP s))

{-# INLINE withGenerator #-}
withGenerator :: forall (a :: Type) s (filter :: Type). WithGenerator a filter (DP s) -> Stage (WithGenerator a filter (DP s))
withGenerator = mkStage' @(WithGenerator a filter (DP s))

{-# INLINE withOutput #-}
withOutput :: forall (a :: Type) s. WithOutput a (DP s) -> Stage (WithOutput a (DP s))
withOutput = mkStage' @(WithOutput a (DP s))

{-# INLINE mkDP' #-}
mkDP' :: forall dpDefinition filterState filterParam st.
         Stage (WithInput dpDefinition (DP st))
      -> GeneratorStage dpDefinition filterState filterParam st 
      -> Stage (WithOutput dpDefinition (DP st))
      -> DynamicPipeline dpDefinition filterState filterParam st
mkDP' = DynamicPipeline @dpDefinition

-- Hiding DP Constraint for running DP
type DPConstraint dpDefinition filterState st filterParam filter iparams gparams oparams r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4 =
  ( MkChans dpDefinition
  , HChan dpDefinition ~ r3 t2
  , Filter dpDefinition filterState filterParam st ~ filter
  , CloseList l3
  , CloseList l4
  , CloseList (HAppendListR l1 l2)
  , iparams ~ WithInput dpDefinition (DP st)
  , gparams ~ WithGenerator dpDefinition filter (DP st)
  , oparams ~ WithOutput dpDefinition (DP st)
  , ArityRev iparams (HLength (ExpandInputToCh dpDefinition))
  , ArityFwd iparams (HLength (ExpandInputToCh dpDefinition))
  , HCurry' (HLength (ExpandInputToCh dpDefinition)) iparams l3 (DP st ())
  , ArityRev gparams (HLength (ExpandGenToCh dpDefinition filter))
  , ArityFwd gparams (HLength (ExpandGenToCh dpDefinition filter))
  , HCurry' (HLength (ExpandGenToCh dpDefinition filter)) gparams (filter ': HAppendListR l1 l2) (DP st ())
  , ArityRev oparams (HLength (ExpandOutputToCh dpDefinition))
  , ArityFwd oparams (HLength (ExpandOutputToCh dpDefinition))
  , HCurry' (HLength (ExpandOutputToCh dpDefinition)) oparams l4 (DP st ())
  , AllChans r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4)

{-# INLINE buildDPProg #-}
buildDPProg :: forall dpDefinition filterState st filterParam filter iparams gparams oparams r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4.
               DPConstraint dpDefinition filterState st filterParam filter iparams gparams oparams r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4
            => DynamicPipeline dpDefinition filterState filterParam st -> DP st ()
buildDPProg DynamicPipeline{..} = do
  (cIns, cGen, cOut) <- inGenOut <$> withDP (makeChans @dpDefinition)
  let genWithFilter   = _gsFilterTemplate generator .*. cGen
  runStageWith input cIns
    >> runStageWith' @(HLength (ExpandGenToCh dpDefinition filter)) @gparams (_gsGenerator generator) genWithFilter cGen
    >> runStageWith output cOut >>= DP . wait

{-# INLINE mkDP #-}
mkDP :: forall dpDefinition filterState st filterParam filter iparams gparams oparams r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4.
        DPConstraint dpDefinition filterState st filterParam filter iparams gparams oparams r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4
     => Stage (WithInput dpDefinition (DP st))
     -> GeneratorStage dpDefinition filterState filterParam st
     -> Stage (WithOutput dpDefinition (DP st))
     -> DP st ()
mkDP inS gS oS = buildDPProg (mkDP' inS gS oS)

{-# INLINE runDP #-}
runDP :: (forall s. DP s a) -> IO a
runDP = runStage

-- Closable Automatic Write Channels
data NotClose (a :: Type)

class CloseList xs where
  closeList :: HList xs -> IO ()

instance (IsClosable x, CloseList xs) => CloseList (x ': xs) where
  closeList (HCons x xs) = close x >> closeList xs

instance CloseList '[] where
  closeList _ = pure ()

class IsClosable f where
  close :: f -> IO ()

instance IsClosable (WriteChannel a) where
  close = end

instance IsClosable (ReadChannel a) where
  close = const $ pure ()


type SpawnFilterConstraint dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4 =
                ( MkChans (ChansFilter dpDefinition)
                , FilterChans r (HList l3) t (HList (ReadChannel readElem : l1))
                , l1 ~ l
                , CloseList (ReadChannel readElem ': l4)
                , HAppendList l l3
                , l4 ~ HAppendListR l l3
                , l2 ~ (readElem ': ReadChannel readElem ': l4)
                , HChan (ChansFilter dpDefinition) ~ r t
                , WithFilter dpDefinition filterParam (StateT filterState (DP st)) ~ (b2 -> ReadChannel b2 -> b3)
                , HLength (ExpandFilterToCh dpDefinition filterParam) ~ HLength l2
                , HCurry' (HLength l2) (WithFilter dpDefinition filterParam (StateT filterState (DP st))) l2 (StateT filterState (DP st) ())
                , ArityFwd (WithFilter dpDefinition filterParam (StateT filterState (DP st))) (HLength (ExpandFilterToCh dpDefinition filterParam))
                , ArityRev b3 (HLength l4)
                )

{-# INLINE spawnFilterForAll #-}
spawnFilterForAll :: forall dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4.
                     SpawnFilterConstraint dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4
                  => Filter dpDefinition filterState filterParam st  -- Filter Template
                  -> (readElem -> filterState) -- Given the First element in this Filter Instance how to Initiate Internal Filter State (Memory)
                  -> ReadChannel readElem -- Main ReadChannel to feed filter
                  -> HList l -- HList with the rest of the ReadChannels if There are needed or HNil if it only contians 1 read channel
                  -> DP st (HList l) -- Rreturn the list of input Channels with the results to be read for the Generator at the end
spawnFilterForAll filter' initState = spawnFilterForAll' filter' initState (const $ pure ())

{-# INLINE spawnFilterForAll' #-}
spawnFilterForAll' :: forall dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4.
                      SpawnFilterConstraint dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4
                   => Filter dpDefinition filterState filterParam st  -- Filter Template
                   -> (readElem -> filterState) -- Given the First element in this Filter Instance how to Initiate Internal Filter State (Memory)
                   -> (readElem -> DP st ()) -- For each element that the Filter is consuming allow to do something outside the filter with that element. For example trace or debug
                   -> ReadChannel readElem -- Main ReadChannel to feed filter
                   -> HList l -- HList with the rest of the ReadChannels if There are needed or HNil if it only contians 1 read channel
                   -> DP st (HList l) -- Rreturn the list of input Channels with the results to be read for the Generator at the end
spawnFilterForAll' filter' initState = spawnFilterWith filter' initState (const True)

{-# INLINE spawnFilterWith #-}
spawnFilterWith  :: forall dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4.
                    SpawnFilterConstraint dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4
                 => Filter dpDefinition filterState filterParam st  -- Filter Template
                 -> (readElem -> filterState) -- Given the First element in this Filter Instance how to Initiate Internal Filter State (Memory)
                 -> (readElem -> Bool) -- Given a new Element determine if we need to interpose a new Filter or not
                 -> (readElem -> DP st ()) -- For each element that the Filter is consuming allow to do something outside the filter with that element. For example trace or debug
                 -> ReadChannel readElem -- Main ReadChannel to feed filter
                 -> HList l -- HList with the rest of the ReadChannels if There are needed or HNil if it only contians 1 read channel
                 -> DP st (HList l) -- Rreturn the list of input Channels with the results to be read for the Generator at the end
spawnFilterWith = loopSpawn

  where
    loopSpawn filter'' initState' spawnIf' onElem' cin' restIns' =
      maybe (pure restIns') (whenNewElem cin' restIns' filter'' initState' spawnIf' onElem')
      =<< DP (pull cin')

    whenNewElem cin' restIns' filter'' initState' spawnIf' onElem' =
      uncurry (loopSpawn filter'' initState' spawnIf' onElem') <=< doOnElem cin' restIns' filter'' initState' spawnIf' onElem'

    doOnElem cin' restIns' filter'' initState' spanwIf' onElem' elem' = do
      onElem' elem'
      if spanwIf' elem'
        then do
          (reads', writes' :: HList l3) <- getFilterChannels <$> DP (makeChans @(ChansFilter dpDefinition))
          let hlist = elem' .*. cin' .*. (restIns' `hAppendList` writes')
          void $ runFilter filter'' (initState' elem') hlist (cin' .*. (restIns' `hAppendList` writes'))
          return (hHead reads', hTail reads')
        else return (cin', restIns')


