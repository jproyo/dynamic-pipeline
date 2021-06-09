module Main where

import           Graph.ConnectedComp                               as CC
import           Misc.RepeatedDP                                   as Repeated
import           Options.Applicative                               as Opt
import           Relude


data ProgramOptions = RepeatedElements
                    | ConnectedComponents FilePath
                    deriving(Show)


programDesc :: ParserInfo ProgramOptions
programDesc = info (everyProgram <**> helper) (fullDesc <> header "Examples on dynamic-pipeline library")
 where
  everyProgram =
    subparser (command "repeated-elements" subCmdRepeated <> command "connected-components" subCmdConnectedComp)

subCmdRepeated :: ParserInfo ProgramOptions
subCmdRepeated = info
  (pure RepeatedElements)
  (fullDesc <> header "repeated-elements - Dynamic Pipeline Examples" <> progDesc
    "Given a list of 2000 repeated Integers filter and output unique 1000 integers"
  )


subCmdConnectedComp :: ParserInfo ProgramOptions
subCmdConnectedComp = info
  (helper <*> toplevelOpt)
  (  fullDesc
  <> header "connected-components - Dynamic Pipeline Examples"
  <> progDesc
       "Given a file path passed by parameters to this command argument with a list of edges, calculate and output the Sets of Connected Components"
  )
  where toplevelOpt = ConnectedComponents <$> fileOption


fileOption :: Parser FilePath
fileOption =
  Opt.strOption (Opt.long "filepath" <> Opt.short 'f' <> Opt.metavar "FilePath" <> Opt.help "File with Set of Edges")

main :: IO ()
main = execParser programDesc >>= main'

main' :: ProgramOptions -> IO ()
main' = \case
    RepeatedElements -> Repeated.program
    ConnectedComponents file -> CC.program file

