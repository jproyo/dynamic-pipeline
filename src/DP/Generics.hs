module DP.Generics where

-- import           Control.Concurrent.Chan.Unagi.NoBlocking
-- import           GHC.Generics
-- import           GHC.TypeLits
-- import           Relude

-- withEnvVarsOrFail :: Channels a => (a -> IO b) -> IO b
-- withEnvVarsOrFail action = do
--   raw <- gChans
--   case raw of
--     Right x   -> action $ to x
--     Left  err -> fail $ prettify err
--  where
--   prettify :: [[(Text, Bool)]] -> [Char]
--   prettify errs = "Could not load enviorment variables because: "
--     <> intercalate "\nor because: " (intercalate " and " . fmap explain <$> errs)

--   explain (name, parseProblem) | parseProblem = "env var " <> show name <> " could not be parsed"
--                                | otherwise    = "missing env var " <> show name

-- class (Generic a, GChannel (Rep a)) => Channels a where

-- class GChannel f where
--     gChans :: IO (f a)


-- instance (GChannel f, GChannel g) => GChannel (f :*: g) where
--   gChans = do
--     f <- gChans
--     g <- gChans
--     case (f, g) of
--       (Right f', Right g') -> return . Right $ f' :*: g'
--       _                    -> return . Left $ (<>) <$> extractError f <*> extractError g


-- instance (GChannel f, GChannel g) => GChannel (f :+: g) where
--   gChans = do
--     f <- fmap L1 <$> gChans
--     g <- fmap R1 <$> gChans

--     case either (const g) Right f of
--       Right h -> return . Right $ h
--       _       -> return . Left $ extractError f <> extractError g

-- instance (GChannel f) => GChannel (D1 a1 f) where
--   gChans = fmap M1 <$> gChans

-- instance (GChannel f) => GChannel (C1 a1 f) where
--   gChans = fmap M1 <$> gChans

-- instance GChannel U1 where
--   gChans = return $ Right U1

-- instance ( KnownSymbol recName
--          , Read a
--          ) => GChannel (S1 ('MetaSel ('Just recName) meta1 meta2 meta3) (K1 meta4 a))
--    where
--   gChans = do
--     let rawName  = symbolVal @recName Proxy
--         rawName' = fmap toUpper . toSnakeCase . drop 1 $ fromCamelCase rawName
--         varName  = if null rawName' then rawName else rawName'

--     var <- lookupEnv varName
--     case var of
--       Just raw | Just x <- readMaybe raw -> return (Right (M1 (K1 x)))
--                | otherwise               -> return (Left [[(toSL varName, True)]])
--       Nothing -> return (Left [[(toSL varName, False)]])


-- extractError :: Either [[err]] b -> [[err]]
-- extractError = either identity $ const [[]]
