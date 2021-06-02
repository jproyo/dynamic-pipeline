module Main where

import           Control.Concurrent.Async
import           Data.HList
import           DynamicPipeline
import           Relude

type DPExample = Input (Channel Int) :>> Generator (Channel Int) :>> Output

input' :: Stage (WriteChannel Int -> IO ())
input' = withInput @DPExample @IO $ \cout -> forM_ ([1 .. 1000] <> [1 .. 1000]) (`push` cout) >> end cout

generator' :: GeneratorStage Int IO DPExample Int
generator' =
  let filterTemp :: Filter Int IO DPExample Int
      filterTemp = mkFilter actorRepeted
      --filterTemp = actor actor1 |>>> actor actor2 |>> actor actor3

      actorRepeted i rc wc = liftIO $ do 
        forall rc $ \e -> if e /= i then push e wc else pure ()
        end wc

      actor1 i _ _ = modify (i +)

      actor2 _ _ _ = modify (+ 2)

      actor3 _ rc wc = do
        memory <- get
        liftIO $ do 
            push memory wc
            forall rc (`push` wc)
            end wc

      gen = withGenerator @DPExample @(Filter Int IO DPExample Int) @IO $ genAction

  in  mkGenerator gen filterTemp

genAction :: Filter Int IO DPExample Int -> ReadChannel Int -> WriteChannel Int -> IO ()
genAction filter' cin cout = do
  foldrS filter' cin cout
  end cout


foldrS :: Filter Int IO DPExample Int -> ReadChannel Int -> WriteChannel Int -> IO ()
foldrS = loop
 where
  loop fil c o = maybe (pure ()) (flip (loop fil) o <=< onElem fil c o) =<< pull c

  onElem filter' cin cout elem' = do
    push elem' cout
    (newWrite, newRead) <- newChannel
    let hlist               = elem' `HCons` cin `HCons` newWrite `HCons` HNil
    void $ async (runFilter hlist filter' 1)
    return newRead

output' :: Stage (ReadChannel Int -> IO ())
output' = withOutput @DPExample @IO $ flip forall print

main :: IO ()
main = runDP $ mkDP @DPExample input' generator' output'
