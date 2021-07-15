module Main where

import           Graph.ConnectedComp                               as CC
import           Misc.RepeatedDP                                   as Repeated
import           Misc.RepeatedTwiceDP                              as RepeatedTwice
import           Options.Applicative                               as Opt
import           Relude


data ProgramOptions = RepeatedElements
                    | RepeatedFeedback
                    | ConnectedComponents FilePath
                    deriving(Show)


programDesc :: ParserInfo ProgramOptions
programDesc = info (everyProgram <**> helper) (fullDesc <> header "Examples on dynamic-pipeline library")
 where
  everyProgram = subparser
    (  command "repeated-elements"    subCmdRepeated
    <> command "repeated-feedback"    subCmdRepeatedFeed
    <> command "connected-components" subCmdConnectedComp
    )

subCmdRepeated :: ParserInfo ProgramOptions
subCmdRepeated = info
  (pure RepeatedElements)
  (fullDesc <> header "repeated-elements - Dynamic Pipeline Examples" <> progDesc
    "Given a list of 2000 repeated Integers filter and output unique 1000 integers"
  )

subCmdRepeatedFeed :: ParserInfo ProgramOptions
subCmdRepeatedFeed = info
  (pure RepeatedFeedback)
  (fullDesc <> header "repeated-feedback - Dynamic Pipeline Examples" <> progDesc
    "Given a list of 2000 repeated Integers filter and output unique 1000 integers and do it twice with feedback"
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
  RepeatedElements         -> Repeated.program
  RepeatedFeedback         -> RepeatedTwice.program
  ConnectedComponents file -> CC.program file

