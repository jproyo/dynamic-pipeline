{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module      : DynamicPipeline.Channel
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
module DynamicPipeline.Stage
  ( DynamicPipeline,
    Filter,
    Actor,
    GeneratorStage,
    Stage,
    ValidDP,
    IsDP,
    DP,
    UnFoldFilter,
    withDP,
    mkGenerator,
    mkFilter,
    single,
    actor,
    (|>>>),
    (|>>),
    withSource,
    withGenerator,
    withSink,
    mkDP,
    runDP,
    unfoldF,
    mkUnfoldFilter,
    mkUnfoldFilter',
    mkUnfoldFilterForAll,
    mkUnfoldFilterForAll'
  ) where

import           Control.Concurrent.Async
import           Control.Lens             hiding ((<|))
import           Data.HList
import           Data.List.NonEmpty
import           DynamicPipeline.Channel hiding (mapM_)
import           DynamicPipeline.Flow
import           GHC.TypeLits
import           Relude                   as R


-- | FCF - Type Level Defunctionalization: Boolean 'And' Type Level Function
type family And (a :: Bool) (b :: Bool) :: Bool where
  And 'True 'True = 'True
  And a b         = 'False

-- | FCF - Type Level Defunctionalization
-- 'IsDP' Validates if /DP/ Flow Type Level Definition is Correct according to the Grammar
--
-- [@dpDefinition ~ 'Source' ('Channel' ..) ':=>' 'Generator' ('Channel' ..) ':=>' 'Sink'@]: /DP/ Type level Flow Definition
--
type family IsDP (dpDefinition :: k) :: Bool where
  IsDP (Source (Channel inToGen)
        :=> Generator (Channel genToOut)
        :=> Sink)
                                            = And (IsDP (Source (Channel inToGen))) (IsDP (Generator (Channel genToOut)))
  IsDP ( Source (Channel inToGen)
         :=> Generator (Channel genToOut)
         :=> FeedbackChannel toSource 
         :=> Sink 
        )
                                            = And (IsDP (Source (Channel inToGen))) (IsDP (Generator (Channel genToOut)))
  IsDP (Source (Channel (a :<+> more)))     = IsDP (Source (Channel more))
  IsDP (Source (Channel Eof))               = 'True
  IsDP (Generator (Channel (a :<+> more)))  = IsDP (Generator (Channel more))
  IsDP (Generator (Channel a))              = 'True
  IsDP x                                    = 'False


-- | FCF - Type Level Defunctionalization
-- 'ValidDP' Check if 'IsDP' is True
--
-- [@a@]: @IsDP dpDefinition ~ 'True@
--
-- Throw a 'TypeError' if Grammar is not correct
type family ValidDP (a :: Bool) :: Constraint where
  ValidDP 'True = ()
  ValidDP 'False = TypeError
                    ( 'Text "Invalid Semantic for Building DP Program"
                      ':$$: 'Text "Language Grammar:"
                      ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> Sink"
                      ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> FEEDBACK :=> Sink"
                      ':$$: 'Text "CHANS    -> Channel CH"
                      ':$$: 'Text "FEEDBACK -> FeedbackChannel CH"
                      ':$$: 'Text "CH       -> Type :<+> CH | Eof"
                      ':$$: 'Text "Example: 'Source (Channel (Int :<+> Int)) :=> Generator (Channel (Int :<+> Int)) :=> Sink'"
                    )

-- Inductive Type Family for Expanding and building Source, Generator, Filter and Sink Functions Signatures
type family WithSource (dpDefinition :: Type) (monadicAction :: Type -> Type) :: Type where
  WithSource (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> Sink) monadicAction
                                                                     = WithSource (ChanIn inToGen) monadicAction
  WithSource (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> FeedbackChannel toSource :=> Sink) monadicAction
                                                                     = WithSource (ChanOutIn toSource inToGen) monadicAction
  WithSource (ChanIn (dpDefinition :<+> more)) monadicAction         = WriteChannel dpDefinition -> WithSource (ChanIn more) monadicAction
  WithSource (ChanIn Eof) monadicAction                              = monadicAction ()
  WithSource (ChanOutIn (dpDefinition :<+> more) ins) monadicAction  = ReadChannel dpDefinition -> WithSource (ChanOutIn more ins) monadicAction
  WithSource (ChanOutIn Eof ins) monadicAction                       = WithSource (ChanIn ins) monadicAction
  WithSource dpDefinition _                                          = TypeError
                                                                        ( 'Text "Invalid Semantic for Source Stage"
                                                                          ':$$: 'Text "in the DP Definition '"
                                                                          ':<>: 'ShowType dpDefinition
                                                                          ':<>: 'Text "'"
                                                                          ':$$: 'Text "Language Grammar:"
                                                                          ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> Sink"
                                                                          ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> FEEDBACK :=> Sink"
                                                                          ':$$: 'Text "CHANS    -> Channel CH"
                                                                          ':$$: 'Text "FEEDBACK -> FeedbackChannel CH"
                                                                          ':$$: 'Text "CH       -> Type :<+> CH | Eof"
                                                                          ':$$: 'Text "Example: 'Source (Channel (Int :<+> Int)) :=> Generator (Channel (Int :<+> Int)) :=> Sink'"
                                                                        )

type family WithGenerator (a :: Type) (filter :: Type) (monadicAction :: Type -> Type) :: Type where
  WithGenerator (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> Sink) filter monadicAction
                                                                         = filter -> WithGenerator (ChanOutIn inToGen genToOut) filter monadicAction
  WithGenerator (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> FeedbackChannel toSource :=> Sink) filter monadicAction
                                                                         = filter -> WithGenerator (ChanOutIn inToGen (ChanInIn genToOut toSource)) filter monadicAction
  WithGenerator (ChanInIn (a :<+> more) ins) filter monadicAction        = WriteChannel a -> WithGenerator (ChanInIn more ins) filter monadicAction
  WithGenerator (ChanInIn Eof ins) filter monadicAction                  = WithGenerator (ChanIn ins) filter monadicAction
  WithGenerator (ChanIn (a :<+> more)) filter monadicAction              = WriteChannel a -> WithGenerator (ChanIn more) filter monadicAction
  WithGenerator (ChanIn Eof) filter monadicAction                        = monadicAction ()
  WithGenerator (ChanOutIn (a :<+> more) ins) filter monadicAction       = ReadChannel a -> WithGenerator (ChanOutIn more ins) filter monadicAction
  WithGenerator (ChanOutIn Eof (ChanInIn ins ins2)) filter monadicAction = WithGenerator (ChanInIn ins ins2) filter monadicAction
  WithGenerator (ChanOutIn Eof ins) filter monadicAction                 = WithGenerator (ChanIn ins) filter monadicAction
  WithGenerator dpDefinition _ _                                         = TypeError
                                                                             ( 'Text "Invalid Semantic for Generator Stage"
                                                                               ':$$: 'Text "in the DP Definition '"
                                                                               ':<>: 'ShowType dpDefinition
                                                                               ':<>: 'Text "'"
                                                                               ':$$: 'Text "Language Grammar:"
                                                                               ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> Sink"
                                                                               ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> FEEDBACK :=> Sink"
                                                                               ':$$: 'Text "CHANS    -> Channel CH"
                                                                               ':$$: 'Text "FEEDBACK -> FeedbackChannel CH"
                                                                               ':$$: 'Text "CH       -> Type :<+> CH | Eof"
                                                                               ':$$: 'Text "Example: 'Source (Channel (Int :<+> Int)) :=> Generator (Channel (Int :<+> Int)) :=> Sink'"
                                                                             )
     
type family WithFilter (dpDefinition :: Type) (param :: Type) (filterState :: Type) (monadicAction :: Type -> Type) :: Type where
  WithFilter (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> Sink) param filterState monadicAction
                                                    = IORef filterState -> param -> WithFilter (ChanOutIn inToGen genToOut) param filterState monadicAction
  WithFilter (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> FeedbackChannel toSource  :=> Sink) param filterState monadicAction
                                                    = IORef filterState -> param -> WithFilter (ChanOutIn inToGen (ChanInIn genToOut toSource)) param filterState monadicAction
  WithFilter (ChanInIn (a :<+> more) ins) param filterState monadicAction       
                                                    = WriteChannel a -> WithFilter (ChanInIn more ins) param filterState monadicAction
  WithFilter (ChanInIn Eof ins) param filterState monadicAction                 
                                                    = WithFilter (ChanIn ins) param filterState monadicAction
  WithFilter (ChanIn (dpDefinition :<+> more)) param filterState monadicAction         
                                                    = WriteChannel dpDefinition -> WithFilter (ChanIn more) param filterState monadicAction
  WithFilter (ChanIn Eof) param filterState monadicAction       
                                                    = monadicAction ()
  WithFilter (ChanOutIn (dpDefinition :<+> more) ins) param filterState monadicAction  
                                                    = ReadChannel dpDefinition -> WithFilter (ChanOutIn more ins) param filterState monadicAction
  WithFilter (ChanOutIn Eof (ChanInIn ins ins2)) param filterState monadicAction 
                                                    = WithFilter (ChanInIn ins ins2) param filterState monadicAction
  WithFilter (ChanOutIn Eof ins) param s m          = WithFilter (ChanIn ins) param s m
  WithFilter dpDefinition _ _ _                     = TypeError
                                                        ( 'Text "Invalid Semantic Semantic for Generator Stage"
                                                          ':$$: 'Text "in the DP Definition '"
                                                          ':<>: 'ShowType dpDefinition
                                                          ':<>: 'Text "'"
                                                          ':$$: 'Text "Language Grammar:"
                                                          ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> Sink"
                                                          ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> FEEDBACK :=> Sink"
                                                          ':$$: 'Text "CHANS    -> Channel CH"
                                                          ':$$: 'Text "FEEDBACK -> FeedbackChannel CH"
                                                          ':$$: 'Text "CH       -> Type :<+> CH | Eof"
                                                          ':$$: 'Text "Example: 'Source (Channel (Int :<+> Int)) :=> Generator (Channel (Int :<+> Int)) :=> Sink'"
                                                        )

type family WithSink (dpDefinition :: Type) (monadicAction :: Type -> Type) :: Type where
  WithSink (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> Sink) monadicAction
                                                              = WithSink (ChanOut genToOut) monadicAction
  WithSink (Source (Channel inToGen) :=> Generator (Channel genToOut) :=> FeedbackChannel toSource :=> Sink) monadicAction
                                                              = WithSink (ChanOut genToOut) monadicAction
  WithSink (ChanOut (dpDefinition :<+> more)) monadicAction   = ReadChannel dpDefinition -> WithSink (ChanOut more) monadicAction
  WithSink (ChanOut Eof) monadicAction                        = monadicAction ()
  WithSink dpDefinition _                                     = TypeError
                                                                    ( 'Text "Invalid Semantic for Sink Stage"
                                                                      ':$$: 'Text "in the DP Definition '"
                                                                      ':<>: 'ShowType dpDefinition
                                                                      ':<>: 'Text "'"
                                                                      ':$$: 'Text "Language Grammar:"
                                                                      ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> Sink"
                                                                      ':$$: 'Text "DP       -> Source CHANS :=> Generator CHANS :=> FEEDBACK :=> Sink"
                                                                      ':$$: 'Text "CHANS    -> Channel CH"
                                                                      ':$$: 'Text "FEEDBACK -> FeedbackChannel CH"
                                                                      ':$$: 'Text "CH       -> Type :<+> CH | Eof"
                                                                      ':$$: 'Text "Example: 'Source (Channel (Int :<+> Int)) :=> Generator (Channel (Int :<+> Int)) :=> Sink'"
                                                                    )


-- | 'DP' is the only Monadic Action allowed to run a /DP/ Defined Flow.
-- It is restricted on Scope by its Existential Type @st@ in order to not escape out from this Monadic Context.
--
-- [@st@]: Existential Type to Ensure context of Monadic 'DP'
--
-- [@a@]: Any Type that carries the Monadic Context 'DP'
--
newtype DP st a = DP
  { runStage :: IO a
  } deriving newtype (Functor, Applicative, Monad, MonadIO)

-- | Smart Constructor of 'DP' from 'IO' action
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
runStageWith :: forall (n :: HNat) f (xs :: [*]).
            (HCurry' n f xs (IO ()), ArityFwd f n, ArityRev f n, CloseList xs)
            => Stage f -> HList xs -> IO (Async ())
runStageWith fn cIns = async (hUncurry (run fn) cIns >> closeList cIns)

{-# INLINE runStageWith' #-}
runStageWith' :: forall (n :: HNat) f (xs :: [*]) (ss :: [*]).
            (HCurry' n f xs (IO ()), ArityFwd f n, ArityRev f n, CloseList ss)
            => Stage f -> HList xs -> HList ss -> IO (Async ())
runStageWith' fn cIns cClose = async (hUncurry (run fn) cIns >> closeList cClose)

-- | 'DynamicPipeline' data type which contains all the three Stages definitions that have been generated by other combinators like 'withSource',
-- 'withGenerator' and 'withSink'.
--
-- [@dpDefinition ~ 'Source' ('Channel' ..) ':=>' 'Generator' ('Channel' ..) ':=>' 'Sink'@]: /DP/ Type level Flow Definition
--
-- [@filterState@]: State of the 'StateT' 'Monad' that is the local State of the Filter execution
--
-- [@filterParam@]: Type of the First Parameter that is pass to the Filter when it is created by the Generator /Anamorphism/. Generator can change the type received from the Reader Channels.
--
-- [@st@]: Existential Scope of 'DP' 'Monad'.
data DynamicPipeline dpDefinition filterState filterParam =
  DynamicPipeline
    { source    :: Stage (WithSource dpDefinition IO)
    , generator :: GeneratorStage dpDefinition filterState filterParam
    , sink      :: Stage (WithSink dpDefinition IO)
    }

-- | 'GeneartorStage' is a special 'Stage' data type according to /DPP/ Definition which contains a 'Filter' template definition,
-- in orther to know how to spawn a new 'Filter' if it is needed, and the 'Stage' of the Generator to allow the user to perform some computation
-- in that case.
--
-- [@dpDefinition ~ 'Source' ('Channel' ..) ':=>' 'Generator' ('Channel' ..) ':=>' 'Sink'@]: /DP/ Type level Flow Definition
--
-- [@filterState@]: State of the 'StateT' 'Monad' that is the local State of the Filter execution
--
-- [@filterParam@]: Type of the First Parameter that is pass to the Filter when it is created by the Generator /Anamorphism/. Generator can change the type received from the Reader Channels.
--
-- [@st@]: Existential Scope of 'DP' 'Monad'.
data GeneratorStage dpDefinition filterState filterParam = GeneratorStage
  { _gsGenerator      :: Stage (WithGenerator dpDefinition (Filter dpDefinition filterState filterParam) IO)
  , _gsFilterTemplate :: Filter dpDefinition filterState filterParam
  }

-- | 'Filter' Is the template definition of the 'Filter' that may be spawned when reading elements on the Stream.
--
-- * 'Filter' is a 'NonEmpty' List of 'Actor's. 
-- 
-- * Each 'Actor' is executed sequentially on the that List when an Element arrive to that 'Filter' instance.
-- 
-- * All the 'Filter' execution (a.k.a. @forM_ actors runStage@) executes in a 'StateT' 'Monad' to share an internal state among 'Actor's.
--
-- [@dpDefinition ~ 'Source' ('Channel' ..) ':=>' 'Generator' ('Channel' ..) ':=>' 'Sink'@]: /DP/ Type level Flow Definition
--
-- [@filterState@]: State of the 'StateT' 'Monad' that is the local State of the Filter execution
--
-- [@filterParam@]: Type of the First Parameter that is pass to the Filter when it is created by the Generator /Anamorphism/. Generator can change the type received from the Reader Channels.
--
-- [@st@]: Existential Scope of 'DP' 'Monad'.
newtype Filter dpDefinition filterState filterParam =
  Filter { unFilter :: NonEmpty (Actor dpDefinition filterState filterParam IO) }
  deriving Generic

instance Wrapped (Filter s' s a)

-- | 'Actor' Is a particular 'Stage' computation inside a 'Filter'.
--
-- [@dpDefinition ~ 'Source' ('Channel' ..) ':=>' 'Generator' ('Channel' ..) ':=>' 'Sink'@]: /DP/ Type level Flow Definition
--
-- [@filterState@]: State of the 'StateT' 'Monad' that is the local State of the Filter execution
--
-- [@filterParam@]: Type of the First Parameter that is pass to the Filter when it is created by the Generator /Anamorphism/. Generator can change the type received from the Reader Channels.
--
-- [@monadicAction@]: 'Monad' Wrapped in 'StateT'.
newtype Actor dpDefinition filterState filterParam monadicAction =
  Actor { unActor :: Stage (WithFilter dpDefinition filterParam filterState monadicAction) }


-- | Smart Constructor of 'GeneratorStage'.
{-# INLINE mkGenerator #-}
mkGenerator :: Stage (WithGenerator dpDefinition (Filter dpDefinition filterState filterParam) IO) -- ^Generator 'Stage'
            -> Filter dpDefinition filterState filterParam -- ^'Filter' template
            -> GeneratorStage dpDefinition filterState filterParam
mkGenerator = GeneratorStage

-- | Smart Constructor of 'Filter'.
{-# INLINE mkFilter #-}
mkFilter :: forall dpDefinition filterState filterParam. 
            WithFilter dpDefinition filterParam filterState IO -- ^Associated type family to Generate Function Signature
         -> Filter dpDefinition filterState filterParam
mkFilter = Filter . single

-- | Smart Constructor of Single 'Actor' Wrapped in 'NonEmpty' List.  
{-# INLINE single #-}
single :: forall dpDefinition filterState filterParam. 
          WithFilter dpDefinition filterParam filterState IO -- ^Associated type family to Generate Function Signature
       -> NonEmpty (Actor dpDefinition filterState filterParam IO)
single = one . actor

-- | Smart Constructor of 'Actor'.
{-# INLINE actor #-}
actor :: forall dpDefinition filterState filterParam.
         WithFilter dpDefinition filterParam filterState IO -- ^Associated type family to Generate Function Signature
      -> Actor dpDefinition filterState filterParam IO
actor = Actor . mkStage' @(WithFilter dpDefinition filterParam filterState IO)

-- | Combinator to build 'Filter' in a /DSL/ approach.
-- Add a new 'Actor' to an already existing 'Filter'.
{-# INLINE (|>>>) #-}
(|>>>) :: forall dpDefinition filterState filterParam. 
          Actor dpDefinition filterState filterParam IO -- ^New 'Actor' to put on front
       -> Filter dpDefinition filterState filterParam -- ^Existing 'Filter'
       -> Filter dpDefinition filterState filterParam
(|>>>) a f = f & _Wrapped' %~ (a <|)
infixr 5 |>>>

-- | Combinator to build 'Filter' in a /DSL/ approach .
-- Given 2 'Actor's build a 'Filter'.
{-# INLINE (|>>) #-}
(|>>) :: forall dpDefinition filterState filterParam. 
         Actor dpDefinition filterState filterParam IO -- ^'Actor' 1
      -> Actor dpDefinition filterState filterParam IO -- ^'Actor' 2
      -> Filter dpDefinition filterState filterParam
(|>>) a1 a2 = Filter (a1 <|one a2)
infixr 5 |>>

{-# INLINE runActor #-}
runActor :: ( HCurry' n (WithFilter dpDefinition filterParam filterState monadicAction) xs r
            , ArityFwd (WithFilter dpDefinition filterParam filterState monadicAction) n
            , ArityRev (WithFilter dpDefinition filterParam filterState monadicAction) n
            ) => Actor dpDefinition filterState filterParam monadicAction -> HList xs -> r
runActor = hUncurry . run . unActor

{-# INLINE runFilter #-}
runFilter :: ( CloseList ss
             , HCurry' n (WithFilter dpDefinition filterParam filterState IO) xs (IO ())
             , ArityFwd (WithFilter dpDefinition filterParam filterState IO) n
             , ArityRev (WithFilter dpDefinition filterParam filterState IO) n
             ) => Filter dpDefinition filterState filterParam -> HList xs -> HList ss -> IO (Async ())
runFilter f clist cClose = async $ do
  void . mapM_ (`runActor` clist) . unFilter $ f
  closeList cClose

-- | Combinator for Building a 'Source' Stage. It uses an Associated Type Class to deduce the Function Signature required to the user
-- taken from /DP/ Type Level Flow Definition 
-- [@dpDefinition ~ 'Source' ('Channel' ..) ':=>' 'Generator' ('Channel' ..) ':=>' 'Sink'@]: /DP/ Type level Flow Definition
--
-- [@st@]: Existential Scope of 'DP' 'Monad'.
{-# INLINE withSource #-}
withSource :: forall (dpDefinition :: Type). 
              WithSource dpDefinition IO -- ^Associated type family to Generate Function Signature
           -> Stage (WithSource dpDefinition IO)
withSource = mkStage' @(WithSource dpDefinition IO)

-- | Combinator for Building a 'Generator' Stage. It uses an Associated Type Class to deduce the Function Signature required to the user
-- taken from /DP/ Type Level Flow Definition 
-- [@dpDefinition ~ 'Source' ('Channel' ..) ':=>' 'Generator' ('Channel' ..) ':=>' 'Sink'@]: /DP/ Type level Flow Definition
--
-- [@filter@]: 'Filter' template type
--
-- [@st@]: Existential Scope of 'DP' 'Monad'.
{-# INLINE withGenerator #-}
withGenerator :: forall (dpDefinition :: Type) (filter :: Type). 
                 WithGenerator dpDefinition filter IO -- ^Associated type family to Generate Function Signature 
              -> Stage (WithGenerator dpDefinition filter IO)
withGenerator = mkStage' @(WithGenerator dpDefinition filter IO)

-- | Combinator for Building a 'Sink' Stage. It uses an Associated Type Class to deduce the Function Signature required to the user
-- taken from /DP/ Type Level Flow Definition 
-- [@dpDefinition ~ 'Source' ('Channel' ..) ':=>' 'Generator' ('Channel' ..) ':=>' 'Sink'@]: /DP/ Type level Flow Definition
--
-- [@st@]: Existential Scope of 'DP' 'Monad'.
{-# INLINE withSink #-}
withSink :: forall (dpDefinition :: Type). 
            WithSink dpDefinition IO  -- ^Associated type family to Generate Function Signature 
           -> Stage (WithSink dpDefinition IO)
withSink = mkStage' @(WithSink dpDefinition IO)

{-# INLINE mkDP' #-}
mkDP' :: forall dpDefinition filterState filterParam.
         Stage (WithSource dpDefinition IO)
      -> GeneratorStage dpDefinition filterState filterParam
      -> Stage (WithSink dpDefinition IO)
      -> DynamicPipeline dpDefinition filterState filterParam
mkDP' = DynamicPipeline @dpDefinition

-- Hiding DP Constraint for running DP
type DPConstraint dpDefinition filterState filterParam filter gparams slr slw glr glw silr silw iparams oparams ls lsi 
  = ( MkChans dpDefinition
    , HChan dpDefinition ~ ChanRecord slr slw glr glw silr silw
    , HAppendList slr slw
    , HAppendList glr glw
    , HAppendList silr silw
    , Filter dpDefinition filterState filterParam ~ filter
    , ls ~ HAppendListR slr slw
    , lsi ~ HAppendListR silr silw
    , CloseList ls
    , CloseList lsi
    , CloseList (HAppendListR glr glw)
    , iparams ~ WithSource dpDefinition IO
    , gparams ~ WithGenerator dpDefinition filter IO
    , oparams ~ WithSink dpDefinition IO
    , ArityRev iparams (HLength (ExpandSourceToCh dpDefinition))
    , ArityFwd iparams (HLength (ExpandSourceToCh dpDefinition))
    , HCurry' (HLength (ExpandSourceToCh dpDefinition)) iparams ls (IO ())
    , ArityRev gparams (HLength (ExpandGenToCh dpDefinition filter))
    , ArityFwd gparams (HLength (ExpandGenToCh dpDefinition filter))
    , HCurry' (HLength (ExpandGenToCh dpDefinition filter)) gparams (filter ': HAppendListR glr glw) (IO ())
    , ArityRev oparams (HLength (ExpandSinkToCh dpDefinition))
    , ArityFwd oparams (HLength (ExpandSinkToCh dpDefinition))
    , HCurry' (HLength (ExpandSinkToCh dpDefinition)) oparams lsi (IO ())
    )

{-# INLINE buildDPProg #-}
buildDPProg :: forall dpDefinition filterState filterParam filter gparams slr slw glr glw silr silw iparams oparams ls lsi.
            DPConstraint dpDefinition filterState filterParam filter gparams slr slw glr glw silr silw iparams oparams ls lsi => 
            DynamicPipeline dpDefinition filterState filterParam -> IO ()
buildDPProg DynamicPipeline{..} = do
  (cIns, cGen, cOut) <- inGenOut <$> (makeChans @dpDefinition)
  let genWithFilter   = _gsFilterTemplate generator .*. cGen
  liftIO $ do 
    runStageWith source cIns
    >> runStageWith' @(HLength (ExpandGenToCh dpDefinition filter)) @gparams (_gsGenerator generator) genWithFilter cGen
    >> runStageWith sink cOut >>= wait

-- | Smart constructor for 'DynamicPipeline' Definition
{-# INLINE mkDP #-}
mkDP :: forall dpDefinition filterState st filterParam filter gparams slr slw glr glw silr silw iparams oparams ls lsi.
        DPConstraint dpDefinition filterState filterParam filter gparams slr slw glr glw silr silw iparams oparams ls lsi
     => Stage (WithSource dpDefinition IO) -- ^ 'Source' Stage generated by 'withSource' combinator
     -> GeneratorStage dpDefinition filterState filterParam  -- ^ 'Generator' Stage generated by 'withGenerator' combinator
     -> Stage (WithSink dpDefinition IO)   -- ^ 'Sink' Stage generated by 'withSink' combinator
     -> IO ()
mkDP inS gS oS = buildDPProg (mkDP' inS gS oS)

-- | Run 'DP' 'Monad' to final 'IO' result
{-# INLINE runDP #-}
runDP :: IO a -> IO a
runDP = id

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

-- | 'SpawnFilterConstraint' Constraint type alias
type SpawnFilterConstraint dpDefinition readElem filterState filterParam l l1 l2 l3 b2 b3 l4 =
                ( MkChans (ChansFilter dpDefinition)
                , l1 ~ l
                , CloseList (ReadChannel readElem ': l4)
                , HAppendList l l3
                , l4 ~ HAppendListR l l3
                , b2 ~ readElem
                , l2 ~ (IORef filterState ': readElem ': ReadChannel readElem ': l4)
                , HChan (ChansFilter dpDefinition) ~ InOutChan (ReadChannel readElem : l) l3
                , WithFilter dpDefinition filterParam filterState IO ~ (IORef filterState -> b2 -> ReadChannel b2 -> b3)
                , HCurry' (HLength l2) (WithFilter dpDefinition filterParam filterState IO) l2 (IO ())
                , ArityFwd (WithFilter dpDefinition filterParam filterState IO) (HLength l2)
                , ArityRev b3 (HLength l4)
                )

-- | 'UnFoldFilter' is a wrapper Data Type that contains all the information needed to spawn 'Filter' instances according to /DPP/.
-- The user will have the capability to select how those filters are going to be spawned, for example on each read element, how to setup
-- initial states of 'StateT' Monad on 'Actor' computations in filters, among others.
--
-- [@dpDefinition ~ 'Source' ('Channel' ..) ':=>' 'Generator' ('Channel' ..) ':=>' 'Sink'@]: /DP/ Type level Flow Definition
--
-- [@readElem@]: Type of the element that is being read from the Selected Channel in the 'Generator' Stage
--
-- [@st@]: Existential Scope of 'DP' 'Monad'.
--
-- [@filterState@]: State of the 'StateT' 'Monad' that is the local State of the Filter execution
--
-- [@filterParam@]: Type of the First Parameter that is pass to the Filter when it is created by the Generator /Anamorphism/. Generator can change the type received from the Reader Channels.
--
data UnFoldFilter dpDefinition readElem filterState filterParam l = 
  UnFoldFilter 
    { _ufSpawnIf :: readElem -> Bool -- ^Given a new Element determine if we need to interpose a new Filter or not
    , _ufOnElem :: readElem -> IO () -- ^For each element that the Filter is consuming allow to do something outside the filter with that element. For example trace or debug
    , _ufFilter :: Filter dpDefinition filterState filterParam  -- ^'Filter' Template
    , _ufInitState :: readElem -> filterState -- ^Given the First element in this Filter Instance how to Initiate Internal 'Filter' 'StateT' (Memory)
    , _ufReadChannel :: ReadChannel readElem -- ^Main 'ReadChannel' to feed filter
    , _ufRsChannels :: HList l -- ^'HList' with the rest of the ReadChannels if There are needed or 'HNil' if it only contians 1 read channel
    }
    
-- | Smart Constructor for 'UnFoldFilter'
mkUnfoldFilter :: (readElem -> Bool) -- ^Given a new Element determine if we need to interpose a new Filter or not
               -> (readElem -> IO ()) -- ^For each element that the Filter is consuming allow to do something outside the filter with that element. For example trace or debug
               -> Filter dpDefinition filterState filterParam  -- ^'Filter' Template
               -> (readElem -> filterState) -- ^Given the First element in this Filter Instance how to Initiate Internal 'Filter' 'StateT' (Memory)
               -> ReadChannel readElem -- ^Main 'ReadChannel' to feed filter
               -> HList l -- ^'HList' with the rest of the ReadChannels if There are needed or 'HNil' if it only contians 1 read channel
               -> UnFoldFilter dpDefinition readElem filterState filterParam l
mkUnfoldFilter = UnFoldFilter

-- | Smart Constructor for 'UnFoldFilter' which bypass to do something externally on each read element
mkUnfoldFilter' :: (readElem -> Bool) -- ^ 
                -> Filter dpDefinition filterState filterParam -- ^ 
                -> (readElem -> filterState) -- ^ 
                -> ReadChannel readElem -- ^ 
                -> HList l -- ^ 
                -> UnFoldFilter dpDefinition readElem filterState filterParam l
mkUnfoldFilter' spawnIf = mkUnfoldFilter spawnIf (const $ pure ())

-- | Smart Constructor for 'UnFoldFilter' That creates a 'Filter' for each element on the Read Channel and interpose on Front of 'Generator' Stage
-- and Last 'Filter'
--
-- @ Source ---> Filter1 ---> Filter2 ... ---> FilterN ---> Generator ---> Sink @
--
mkUnfoldFilterForAll :: Filter dpDefinition filterState filterParam -- ^ 
                     -> (readElem -> filterState) -- ^ 
                     -> ReadChannel readElem -- ^ 
                     -> HList l -- ^ 
                     -> UnFoldFilter dpDefinition readElem filterState filterParam l
mkUnfoldFilterForAll = mkUnfoldFilter' (const True)

-- | Idem for 'mkUnfoldFilterForAll' but do something on each Element externally
--
mkUnfoldFilterForAll' :: (readElem -> IO ()) -- ^ 
                      -> Filter dpDefinition filterState filterParam -- ^ 
                      -> (readElem -> filterState) -- ^ 
                      -> ReadChannel readElem -- ^ 
                      -> HList l -- ^ 
                      -> UnFoldFilter dpDefinition readElem filterState filterParam l
mkUnfoldFilterForAll' = mkUnfoldFilter (const True)

-- | Run 'UnFoldFilter'
{-# INLINE unfoldF #-}
unfoldF :: forall dpDefinition readElem filterState filterParam l l1 l2 l3 b2 b3 l4.
           SpawnFilterConstraint dpDefinition readElem filterState filterParam l l1 l2 l3 b2 b3 l4
        => UnFoldFilter dpDefinition readElem filterState filterParam l -- ^ 'UnFoldFilter'
        -> IO (HList l) -- ^Return the list of 'ReadChannel's with the results to be read for the 'Generator' at the end. You can use this to pass the results to 'Sink'
unfoldF = loopSpawn

  where
    loopSpawn uf@UnFoldFilter{..} =
      maybe (pure _ufRsChannels) (loopSpawn <=< doOnElem uf) =<< pull _ufReadChannel

    doOnElem uf@UnFoldFilter{..} elem' = do
      _ufOnElem elem'
      if _ufSpawnIf elem'
        then do
          state' <- R.newIORef (_ufInitState elem')
          (reads', writes' :: HList l3) <- getFilterChannels <$> makeChansF @(ChansFilter dpDefinition)
          let hlist = state' .*. elem' .*. _ufReadChannel .*. (_ufRsChannels `hAppendList` writes')
          void $ runFilter _ufFilter hlist (_ufReadChannel .*. (_ufRsChannels `hAppendList` writes'))
          return $ uf { _ufReadChannel = hHead reads', _ufRsChannels = hTail reads' }
        else return uf



