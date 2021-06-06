module Main where

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
  others <- spawnFilterForAll cin (cin' `HCons` HNil) filter' Just (const $ pure ())
  forall (hHead others) (`push` cout')
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

output' :: Stage (ReadChannel Int -> ReadChannel Int -> IO ())
output' = withOutput @DPExample @IO $ \_ ci -> forall ci print

main :: IO ()
main = runDP $ mkDP @DPExample input' generator' output'
