{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE UndecidableInstances    #-}
module DynamicPipeline.Stage where

import           Control.Concurrent.Async
import           Control.Lens                             hiding ((<|))
import           Data.HList
import           Data.List.NonEmpty
import           DynamicPipeline.Channel
import           GHC.TypeLits
import           Relude                                   as R


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

-- Stages Definitions
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

{-# INLINE runStage #-}
runStage :: forall (n :: HNat) f (xs :: [*]).
            (HCurry' n f xs (IO ()), ArityFwd f n, ArityRev f n, CloseList xs)
            => Stage f -> HList xs -> IO (Async ())
runStage fn cIns = async (hUncurry (run fn) cIns >> closeList cIns)

{-# INLINE runStage' #-}
runStage' :: forall (n :: HNat) f (xs :: [*]) r.
          (HCurry' n f xs r, ArityFwd f n, ArityRev f n)
          => Stage f -> HList xs -> r
runStage' = hUncurry . run

{-# INLINE runStage'' #-}
runStage'' :: forall (n :: HNat) f (xs :: [*]) (ss :: [*]).
            (HCurry' n f xs (IO ()), ArityFwd f n, ArityRev f n, CloseList ss)
            => Stage f -> HList xs -> HList ss -> IO (Async ())
runStage'' fn cIns cClose = async (hUncurry (run fn) cIns >> closeList cClose)

-- Generator and Filter specific Definitions
data GeneratorStage s m a param = GeneratorStage
  { _gsGenerator      :: Stage (WithGenerator a (Filter s m a param) m)
  , _gsFilterTemplate :: Filter s m a param
  }

{-# INLINE mkGenerator #-}
mkGenerator :: Stage (WithGenerator a (Filter s m a param) m) -> Filter s m a param -> GeneratorStage s m a param
mkGenerator = GeneratorStage

newtype Actor s m a param = Actor {  unActor :: MonadState s m => Stage (WithFilter a param m) }

newtype Filter s m a param = Filter { unFilter :: NonEmpty (Actor s (StateT s m) a param) }
  deriving Generic

instance Wrapped (Filter s m a param)

{-# INLINE mkFilter #-}
mkFilter :: forall s m a param. WithFilter a param (StateT s m) -> Filter s m a param
mkFilter = Filter . single

{-# INLINE single #-}
single :: forall s m a param. WithFilter a param (StateT s m) -> NonEmpty (Actor s (StateT s m) a param)
single = one . actor

{-# INLINE actor #-}
actor :: forall s m a param. WithFilter a param m -> Actor s m a param
actor = Actor . mkStage' @(WithFilter a param m)

{-# INLINE (|>>>) #-}
(|>>>) :: forall s m a param. Actor s (StateT s m) a param -> Filter s m a param -> Filter s m a param
(|>>>) a f = f & _Wrapped' %~ (a <|)
infixr 5 |>>>

{-# INLINE (|>>) #-}
(|>>) :: forall s m a param. Actor s (StateT s m) a param -> Actor s (StateT s m) a param -> Filter s m a param
(|>>) a1 a2 = Filter (a1 <|one a2)
infixr 5 |>>

{-# INLINE runActor #-}
runActor :: ( MonadState s m2
            , HCurry' n (WithFilter a param m2) xs r
            , ArityFwd (WithFilter a param m2) n
            , ArityRev (WithFilter a param m2) n
            ) => Actor s m2 a param -> HList xs -> r
runActor ac = runStage' (unActor ac)

{-# INLINE runFilter #-}
runFilter :: ( MonadIO m1, Monad m2, CloseList ss
             , HCurry' n (WithFilter a param (StateT s1 m2)) xs (StateT s2 IO ())
             , ArityFwd (WithFilter a param (StateT s1 m2)) n
             , ArityRev (WithFilter a param (StateT s1 m2)) n
             ) => Filter s1 m2 a param -> s2 -> HList xs -> HList ss -> m1 (Async ())
runFilter f s clist cClose = liftIO $ async $ do
  flip evalStateT s . mapM_ (`runActor` clist) . unFilter $ f
  liftIO $ closeList cClose

{-# INLINE withInput #-}
withInput :: forall (a :: Type) (m :: Type -> Type). WithInput a m -> Stage (WithInput a m)
withInput = mkStage' @(WithInput a m)

{-# INLINE withGenerator #-}
withGenerator :: forall (a :: Type) (filter :: Type) (m :: Type -> Type). WithGenerator a filter m -> Stage (WithGenerator a filter m)
withGenerator = mkStage' @(WithGenerator a filter m)

{-# INLINE withOutput #-}
withOutput :: forall (a :: Type) (m :: Type -> Type). WithOutput a m -> Stage (WithOutput a m)
withOutput = mkStage' @(WithOutput a m)

data DynamicPipeline a s param = ValidDP (IsDP a) => DynamicPipeline
  { input     :: Stage (WithInput a IO)
  , generator :: GeneratorStage s IO a param
  , output    :: Stage (WithOutput a IO)
  }

{-# INLINE mkDP #-}
mkDP :: forall a s param. ValidDP (IsDP a) => Stage (WithInput a IO) -> GeneratorStage s IO a param -> Stage (WithOutput a IO) -> DynamicPipeline a s param
mkDP = DynamicPipeline @a

-- Hiding DP Constraint for running DP
type DPConstraint a s param filter r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4 = (MkChans a
                                                                        , HChan a ~ r3 t2
                                                                        , Filter s IO a param ~ filter
                                                                        , CloseList l3
                                                                        , CloseList l4
                                                                        , CloseList (HAppendListR l1 l2)
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

{-# INLINE runDP #-}
runDP :: forall a s param filter r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4.
         DPConstraint a s param filter r2 r3 l1 r4 l2 t2 s1 t1 s2 t5 l3 l4
       => DynamicPipeline a s param -> IO ()
runDP DynamicPipeline{..} = do
  (cIns, cGen, cOut) <- inGenOut <$> makeChans @a
  let genWithFilter   = _gsFilterTemplate generator .*. cGen
  runStage input cIns
    >> runStage'' @(HLength (ExpandGenToCh a filter)) @(WithGenerator a filter IO) (_gsGenerator generator) genWithFilter cGen
    >> runStage output cOut >>= wait

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


type SpawnFilterConstraint a b s param l r t l1 b0 l2 l3 b2 b3 l4 =
                ( MkChans (ChansFilter a)
                , FilterChans r (HList l3) t (HList (ReadChannel b : l1))
                , l1 ~ l
                , CloseList (ReadChannel b ': l4)
                , HAppendList l l3
                , l4 ~ HAppendListR l l3
                , l2 ~ (b ': ReadChannel b ': l4)
                , HChan (ChansFilter a) ~ r t
                , WithFilter a param (StateT s IO) ~ (b2 -> ReadChannel b2 -> b3)
                , HLength (ExpandFilterToCh a param) ~ HLength l2
                , HCurry' (HLength l2) (WithFilter a param (StateT s IO)) l2 (StateT s IO ())
                , ArityFwd (WithFilter a param (StateT s IO)) (HLength (ExpandFilterToCh a param))
                , ArityRev b3 (HLength l4)
                )

{-# INLINE spawnFilterForAll #-}
spawnFilterForAll :: forall a b s param l r t l1 b0 l2 l3 b2 b3 l4.
                     SpawnFilterConstraint a b s param l r t l1 b0 l2 l3 b2 b3 l4
                  => Filter s IO a param -- Filter Template
                  -> (b -> s) -- Given the First element in this Filter Instance how to Initiate Internal Filter State (Memory)
                  -> ReadChannel b -- Main ReadChannel to feed filter
                  -> HList l -- HList with the rest of the ReadChannels if There are needed or HNil if it only contians 1 read channel
                  -> IO (HList l) -- Rreturn the list of input Channels with the results to be read for the Generator at the end
spawnFilterForAll filter' initState = spawnFilterForAll' filter' initState (const $ pure ())

{-# INLINE spawnFilterForAll' #-}
spawnFilterForAll' :: forall a b s param l r t l1 b0 l2 l3 b2 b3 l4.
                     SpawnFilterConstraint a b s param l r t l1 b0 l2 l3 b2 b3 l4
                  => Filter s IO a param -- Filter Template
                  -> (b -> s) -- Given the First element in this Filter Instance how to Initiate Internal Filter State (Memory)
                  -> (b -> IO ()) -- For each element that the Filter is consuming allow to do something outside the filter with that element. For example trace or debug
                  -> ReadChannel b -- Main ReadChannel to feed filter
                  -> HList l -- HList with the rest of the ReadChannels if There are needed or HNil if it only contians 1 read channel
                  -> IO (HList l) -- Rreturn the list of input Channels with the results to be read for the Generator at the end
spawnFilterForAll' filter' initState = spawnFilterWith filter' initState (const True)

{-# INLINE spawnFilterWith #-}
spawnFilterWith :: forall a b s param l r t l1 b0 l2 l3 b2 b3 l4.
                   SpawnFilterConstraint a b s param l r t l1 b0 l2 l3 b2 b3 l4
                => Filter s IO a param
                -> (b -> s) -- Given the First element in this Filter Instance how to Initiate Internal Filter State (Memory)
                -> (b -> Bool) -- Given a new Element determine if we need to interpose a new Filter or not
                -> (b -> IO ()) -- For each element that the Filter is consuming allow to do something outside the filter with that element. For example trace or debug
                -> ReadChannel b -- Main ReadChannel to feed filter
                -> HList l -- HList with the rest of the ReadChannels if There are needed or HNil if it only contians 1 read channel
                -> IO (HList l) -- Rreturn the list of input Channels with the results to be read for the Generator at the end
spawnFilterWith = loopSpawn

  where
    loopSpawn filter'' initState' spawnIf' onElem' cin' restIns' =
      maybe (pure restIns') (whenNewElem cin' restIns' filter'' initState' spawnIf' onElem')
      =<< pull cin'

    whenNewElem cin' restIns' filter'' initState' spawnIf' onElem' =
      uncurry (loopSpawn filter'' initState' spawnIf' onElem') <=< doOnElem cin' restIns' filter'' initState' spawnIf' onElem'

    doOnElem cin' restIns' filter'' initState' spanwIf' onElem' elem' = do
      onElem' elem'
      if spanwIf' elem'
        then do
          (reads', writes' :: HList l3) <- getFilterChannels <$> makeChans @(ChansFilter a)
          let hlist = elem' .*. cin' .*. (restIns' `hAppendList` writes')
          void $ runFilter filter'' (initState' elem') hlist (cin' .*. (restIns' `hAppendList` writes'))
          return (hHead reads', hTail reads')
        else return (cin', restIns')


