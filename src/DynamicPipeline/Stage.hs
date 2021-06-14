-- |
-- Module      : DynamicPipeline.Channel
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
module DynamicPipeline.Stage where

import           Control.Concurrent.Async
import           Control.Lens             hiding ((<|))
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
type family IsDP (dpDefinition :: k) :: Bool where
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
type family WithInput (dpDefinition :: Type) (monadicAction :: Type -> Type) :: Type where
  WithInput (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) monadicAction 
                                                                    = WithInput (ChanIn inToGen) monadicAction
  WithInput (ChanIn (dpDefinition :<+> more)) monadicAction         = WriteChannel dpDefinition -> WithInput (ChanIn more) monadicAction
  WithInput (ChanIn Eof) monadicAction                              = monadicAction ()
  WithInput (ChanOutIn (dpDefinition :<+> more) ins) monadicAction  = ReadChannel dpDefinition -> WithInput (ChanOutIn more ins) monadicAction
  WithInput (ChanOutIn Eof ins) monadicAction                       = WithInput (ChanIn ins) monadicAction
  WithInput dpDefinition _                                          = TypeError
                                                                        ( 'Text "Invalid Semantic for Input Stage"
                                                                          ':$$: 'Text "in the DP Definition '"
                                                                          ':<>: 'ShowType dpDefinition
                                                                          ':<>: 'Text "'"
                                                                          ':$$: 'Text "Language Grammar:"
                                                                          ':$$: 'Text "DP    = Input CHANS :>> Generator CHANS :>> Output"
                                                                          ':$$: 'Text "CHANS = Channel CH"
                                                                          ':$$: 'Text "CH    = Type | Type :<+> CH"
                                                                          ':$$: 'Text "Example: 'Input (Channel (Int :<+> Int)) :>> Generator (Channel (Int :<+> Int)) :>> Output'"
                                                                        )

type family WithGenerator (a :: Type) (filter :: Type) (monadicAction :: Type -> Type) :: Type where
  WithGenerator (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) filter monadicAction 
                                                                    = filter -> WithGenerator (ChanOutIn inToGen genToOut) filter monadicAction
  WithGenerator (ChanIn (a :<+> more)) filter monadicAction         = WriteChannel a -> WithGenerator (ChanIn more) filter monadicAction
  WithGenerator (ChanIn Eof) filter monadicAction                   = monadicAction ()
  WithGenerator (ChanOutIn (a :<+> more) ins) filter monadicAction  = ReadChannel a -> WithGenerator (ChanOutIn more ins) filter monadicAction
  WithGenerator (ChanOutIn Eof ins) filter monadicAction            = WithGenerator (ChanIn ins) filter monadicAction
  WithGenerator dpDefinition _ _                                     = TypeError
                                                                        ( 'Text "Invalid Semantic for Generator Stage"
                                                                          ':$$: 'Text "in the DP Definition '"
                                                                          ':<>: 'ShowType dpDefinition
                                                                          ':<>: 'Text "'"
                                                                          ':$$: 'Text "Language Grammar:"
                                                                          ':$$: 'Text "DP    = Input CHANS :>> Generator CHANS :>> Output"
                                                                          ':$$: 'Text "CHANS = Channel CH"
                                                                          ':$$: 'Text "CH    = Type | Type :<+> CH"
                                                                          ':$$: 'Text "Example: 'Input (Channel (Int :<+> Int)) :>> Generator (Channel (Int :<+> Int)) :>> Output'"
                                                                        )

type family WithFilter (dpDefinition :: Type) (param :: Type) (monadicAction :: Type -> Type) :: Type where
  WithFilter (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) param monadicAction
                                                    = param -> WithFilter (ChanOutIn inToGen genToOut) param monadicAction
  WithFilter (ChanIn (dpDefinition :<+> more)) param monadicAction         = WriteChannel dpDefinition -> WithFilter (ChanIn more) param monadicAction
  WithFilter (ChanIn Eof) param monadicAction                   = monadicAction ()
  WithFilter (ChanOutIn (dpDefinition :<+> more) ins) param monadicAction  = ReadChannel dpDefinition -> WithFilter (ChanOutIn more ins) param monadicAction
  WithFilter (ChanOutIn Eof ins) param m            = WithFilter (ChanIn ins) param m
  WithFilter dpDefinition _ _                                  = TypeError
                                                ( 'Text "Invalid Semantic Semantic for Generator Stage"
                                                  ':$$: 'Text "in the DP Definition '"
                                                  ':<>: 'ShowType dpDefinition
                                                  ':<>: 'Text "'"
                                                  ':$$: 'Text "Language Grammar:"
                                                  ':$$: 'Text "DP    = Input CHANS :>> Generator CHANS :>> Output"
                                                  ':$$: 'Text "CHANS = Channel CH"
                                                  ':$$: 'Text "CH    = Type | Type :<+> CH"
                                                  ':$$: 'Text "Example: 'Input (Channel (Int :<+> Int)) :>> Generator (Channel (Int :<+> Int)) :>> Output'"
                                                )

type family WithOutput (dpDefinition :: Type) (monadicAction :: Type -> Type) :: Type where
  WithOutput (Input (Channel inToGen) :>> Generator (Channel genToOut) :>> Output) monadicAction
                                                                = WithOutput (ChanOut genToOut) monadicAction
  WithOutput (ChanOut (dpDefinition :<+> more)) monadicAction   = ReadChannel dpDefinition -> WithOutput (ChanOut more) monadicAction
  WithOutput (ChanOut Eof) monadicAction                        = monadicAction ()
  WithOutput dpDefinition _                                     = TypeError
                                                                    ( 'Text "Invalid Semantic for Output Stage"
                                                                      ':$$: 'Text "in the DP Definition '"
                                                                      ':<>: 'ShowType dpDefinition
                                                                      ':<>: 'Text "'"
                                                                      ':$$: 'Text "Language Grammar:"
                                                                      ':$$: 'Text "DP    = Input CHANS :>> Generator CHANS :>> Output"
                                                                      ':$$: 'Text "CHANS = Channel CH"
                                                                      ':$$: 'Text "CH    = Type | Type :<+> CH"
                                                                      ':$$: 'Text "Example: 'Input (Channel (Int :<+> Int)) :>> Generator (Channel (Int :<+> Int)) :>> Output'"
                                                                    )



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
withInput :: forall (dpDefinition :: Type) st. WithInput dpDefinition (DP st) -> Stage (WithInput dpDefinition (DP st))
withInput = mkStage' @(WithInput dpDefinition (DP st))

{-# INLINE withGenerator #-}
withGenerator :: forall (dpDefinition :: Type) (filter :: Type) st. WithGenerator dpDefinition filter (DP st) 
              -> Stage (WithGenerator dpDefinition filter (DP st))
withGenerator = mkStage' @(WithGenerator dpDefinition filter (DP st))

{-# INLINE withOutput #-}
withOutput :: forall (dpDefinition :: Type) st. WithOutput dpDefinition (DP st) 
           -> Stage (WithOutput dpDefinition (DP st))
withOutput = mkStage' @(WithOutput dpDefinition (DP st))

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
runDP :: (forall st. DP st a) -> IO a
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

{-# INLINE unfoldFilterForAll #-}
unfoldFilterForAll :: forall dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4.
                     SpawnFilterConstraint dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4
                  => Filter dpDefinition filterState filterParam st  -- Filter Template
                  -> (readElem -> filterState) -- Given the First element in this Filter Instance how to Initiate Internal Filter State (Memory)
                  -> ReadChannel readElem -- Main ReadChannel to feed filter
                  -> HList l -- HList with the rest of the ReadChannels if There are needed or HNil if it only contians 1 read channel
                  -> DP st (HList l) -- Rreturn the list of input Channels with the results to be read for the Generator at the end
unfoldFilterForAll filter' initState = unfoldFilterForAll' filter' initState (const $ pure ())

{-# INLINE unfoldFilterForAll' #-}
unfoldFilterForAll' :: forall dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4.
                      SpawnFilterConstraint dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4
                   => Filter dpDefinition filterState filterParam st  -- Filter Template
                   -> (readElem -> filterState) -- Given the First element in this Filter Instance how to Initiate Internal Filter State (Memory)
                   -> (readElem -> DP st ()) -- For each element that the Filter is consuming allow to do something outside the filter with that element. For example trace or debug
                   -> ReadChannel readElem -- Main ReadChannel to feed filter
                   -> HList l -- HList with the rest of the ReadChannels if There are needed or HNil if it only contians 1 read channel
                   -> DP st (HList l) -- Rreturn the list of input Channels with the results to be read for the Generator at the end
unfoldFilterForAll' filter' initState = unfoldFilterWith filter' initState (const True)

{-# INLINE unfoldFilterWith #-}
unfoldFilterWith  :: forall dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4.
                    SpawnFilterConstraint dpDefinition readElem st filterState filterParam l r t l1 b0 l2 l3 b2 b3 l4
                 => Filter dpDefinition filterState filterParam st  -- Filter Template
                 -> (readElem -> filterState) -- Given the First element in this Filter Instance how to Initiate Internal Filter State (Memory)
                 -> (readElem -> Bool) -- Given a new Element determine if we need to interpose a new Filter or not
                 -> (readElem -> DP st ()) -- For each element that the Filter is consuming allow to do something outside the filter with that element. For example trace or debug
                 -> ReadChannel readElem -- Main ReadChannel to feed filter
                 -> HList l -- HList with the rest of the ReadChannels if There are needed or HNil if it only contians 1 read channel
                 -> DP st (HList l) -- Rreturn the list of input Channels with the results to be read for the Generator at the end
unfoldFilterWith = loopSpawn

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


