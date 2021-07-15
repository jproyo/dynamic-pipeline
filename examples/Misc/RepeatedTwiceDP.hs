-- |
-- Module      : Misc.RepeatedTwiceDP
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
module Misc.RepeatedTwiceDP where

import           DynamicPipeline as DP
import           Relude

type DPExample =   Source (Channel (Int :<+> Int :<+> Double :<+> String :<+> Eof)) 
               :=> Generator (Channel (Int :<+> Int :<+> Double :<+> Eof)) 
               :=> FeedbackChannel (String :<+> Eof)
               :=> Sink

source' :: Stage (ReadChannel String -> WriteChannel Int -> WriteChannel Int -> WriteChannel Double -> WriteChannel String -> DP st ())
source' = withSource @DPExample $ \feedback cout cout' cout'' toFilter -> do 
    unfoldT ([1 .. 10] <> [1 .. 10]) cout identity
    liftIO $ end cout >> end cout' >> end cout''
    foldM feedback (`push` toFilter)

generator' :: GeneratorStage DPExample (Maybe Int) Int s
generator' =
  let gen = withGenerator @DPExample genAction
   in  mkGenerator gen filterTemp

genAction :: Filter DPExample (Maybe Int) Int st 
          -> ReadChannel Int 
          -> ReadChannel Int 
          -> ReadChannel Double
          -> ReadChannel String 
          -> WriteChannel Int 
          -> WriteChannel Int 
          -> WriteChannel Double
          -> WriteChannel String 
          -> DP st ()
genAction filter' cin cin' cdn cin'' _ _ odn cout = do
  let unfoldFilter = mkUnfoldFilterForAll filter' Just cin (cin' .*. cdn .*. cin'' .*. HNil)
  HCons ft (HCons sec _) <- unfoldF unfoldFilter
  foldM ft $ flip push cout . show
  liftIO $ end cout
  foldM sec $ flip push odn 
  liftIO $ end odn

filterTemp :: Filter DPExample (Maybe Int) Int s 
filterTemp = mkFilter actorRepeted

actorRepeted :: Int
             -> ReadChannel Int
             -> ReadChannel Int
             -> ReadChannel Double
             -> ReadChannel String
             -> WriteChannel Int
             -> WriteChannel Int
             -> WriteChannel Double
             -> WriteChannel String
             -> StateT (Maybe Int) (DP s) ()
actorRepeted i rc rc' rd rs wc wc' wd wc'' = do
  foldM rc $ \e -> do 
    putTextLn $ "1) Elem: " <> show e <> " - Param: " <> show i
    if e /= i then push e wc else pure ()
  liftIO $ end wc
  push i wc'
  foldM rc' $ \e -> do 
    putTextLn $ "2) Elem: " <> show e <> " - Param: " <> show i
    push e wc'
  liftIO $ end wc'
  foldM rs $ \e -> 
    let x = maybe 0 identity $ readMaybe @Int e 
     in if i == x 
          then push (fromIntegral x) wd
          else push e wc''
  liftIO $ end wc''
  foldM rd $ flip push wd
  liftIO $ end wd


sink' :: Stage (ReadChannel Int -> ReadChannel Int -> ReadChannel Double -> DP s ())
sink' = withSink @DPExample $ \_ _ ci -> foldM ci print

program :: IO ()
program = runDP $ mkDP @DPExample source' generator' sink'
