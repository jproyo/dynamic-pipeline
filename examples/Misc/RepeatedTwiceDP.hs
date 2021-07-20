-- |
-- Module      : Misc.RepeatedTwiceDP
-- Copyright   : (c) 2021 Juan Pablo Royo Sales
--
-- License     : BSD3
-- Maintainer  : juanpablo.royo@gmail.com
-- Stability   : experimental
-- Portability : GHC
module Misc.RepeatedTwiceDP where

import           DynamicPipeline
import           Relude

type DPExample =   Source (Channel (Int :<+> Int :<+> Double :<+> String :<+> Eof)) 
               :=> Generator (Channel (Int :<+> Int :<+> Double :<+> Eof)) 
               :=> FeedbackChannel (String :<+> Eof)
               :=> Sink

source' :: Stage (ReadChannel String -> WriteChannel Int -> WriteChannel Int -> WriteChannel Double -> WriteChannel String -> DP st ())
source' = withSource @DPExample $ \feedback cout cout' cout'' toFilter -> do 
    finish cout' >> finish cout''
    unfoldT ([1 .. 10] <> [1 .. 10]) cout identity
    feedback |=> toFilter

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
    ft |=>| cout $ show
    sec |=>| odn $ id

filterTemp :: Filter DPExample (Maybe Int) Int s 
filterTemp = mkFilter actorRepeted

actorRepeted :: IORef (Maybe Int)
             -> Int
             -> ReadChannel Int
             -> ReadChannel Int
             -> ReadChannel Double
             -> ReadChannel String
             -> WriteChannel Int
             -> WriteChannel Int
             -> WriteChannel Double
             -> WriteChannel String
             -> DP s ()
actorRepeted _ i rc rc' rd rs wc wc' wd wc'' = do
  rc |>=>| wc $ \e -> do 
    -- putTextLn $ "1) Elem: " <> show e <> " - Param: " <> show i
    if e /= i then pure $ Just e else pure Nothing
  push i wc'
  rc' |>=>| wc' $ \e -> do 
    -- putTextLn $ "2) Elem: " <> show e <> " - Param: " <> show i
    pure $ Just e
  foldM_ rs $ \e -> 
    let x = maybe 0 identity $ readMaybe @Int e 
     in if i == x 
          then push (fromIntegral x) wd
          else push e wc''
  finish wc''
  rd |=>| wd $ id


sink' :: Stage (ReadChannel Int -> ReadChannel Int -> ReadChannel Double -> DP s ())
sink' = withSink @DPExample $ \_ _ ci -> foldM_ ci print

program :: IO ()
program = runDP $ mkDP @DPExample source' generator' sink'
