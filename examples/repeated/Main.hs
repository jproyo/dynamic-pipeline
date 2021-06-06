module Main where

import           Control.Concurrent.Async
import           Data.HList
import           DynamicPipeline
import           Relude

type DPExample = Input (Channel (Int :<+> Int :<+> Eof)) :>> Generator (Channel (Int :<+> Int :<+> Eof)) :>> Output

input' :: Stage (WriteChannel Int -> WriteChannel Int -> IO ())
input' = withInput @DPExample @IO $ \cout cout2 -> do 
  forM_ ([1 .. 1000] <> [1 .. 1000]) (`push` cout) >> end cout >> end cout2

generator' :: GeneratorStage (Maybe Int) IO DPExample Int
generator' =
  let gen = withGenerator @DPExample @(Filter (Maybe Int) IO DPExample Int) @IO $ genAction

  in  mkGenerator gen filterTemp

genAction :: Filter (Maybe Int) IO DPExample Int -> ReadChannel Int -> ReadChannel Int -> WriteChannel Int -> WriteChannel Int -> IO ()
genAction filter' cin cin' cout cout' = do
  (_, cin'') <- foldrS filter' cin cin'
  forall cin'' (`push` cout')
  end cout
  end cout'

filterTemp :: Filter (Maybe Int) IO DPExample Int
filterTemp = actor actorRepeted |>> actor passElem

passElem :: Int -> ReadChannel Int -> ReadChannel Int -> WriteChannel Int -> WriteChannel Int -> StateT (Maybe Int) IO ()
passElem _ _ rc2 _ wc2 = do 
  liftIO $ forall rc2 (`push` wc2)
  maybe (pure ()) (\e -> liftIO $ push e wc2) =<< get
  liftIO $ end wc2

actorRepeted :: Int -> ReadChannel Int -> ReadChannel Int -> WriteChannel Int -> WriteChannel Int -> StateT (Maybe Int) IO ()
actorRepeted i rc _ wc _ = do 
  liftIO $ do
    forall rc $ \e -> if e /= i then push e wc else pure ()
    end wc

{-
(newcin2, newcin3) <- spawnFilterWith cin1 filter' (\a -> True) (\a -> push a wc) cin2 cin3 
end wc
-}

a :: ReadChannel Int -> IO (HList '[])
a cin = spawnFilterWith cin HNil filterTemp (1::Int) (const True) (const $ pure ())

foldrS :: Filter (Maybe Int) IO DPExample Int -> ReadChannel Int -> ReadChannel Int -> IO (ReadChannel Int, ReadChannel Int)
foldrS = loop'
 where
  loop' fil c c' = maybe (pure (c, c')) (uncurry (loop' fil) <=< onElem fil c c') =<< pull c

  onElem filter' cin cin' elem' = do
    (newWrite, newRead) <- newChannel 
    (newWrite', newRead') <- newChannel 
    --(newRead, _, hlist) <- splitedFilterChans elem' cin <$> makeChans @DPExample
    let hlist               = elem' `HCons` cin `HCons` cin' `HCons` newWrite `HCons` newWrite' `HCons` HNil
    void $ async (runFilter hlist filter' (Just elem'))
    return (newRead, newRead')

output' :: Stage (ReadChannel Int -> ReadChannel Int -> IO ())
output' = withOutput @DPExample @IO $ \_ ci -> forall ci print

main :: IO ()
main = runDP $ mkDP @DPExample input' generator' output'
