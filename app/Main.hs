module Main where

import DS
import System.Environment (getArgs)

dsYaml :: FilePath
dsYaml = "ds.yaml"

data Command
    = RunInit
    | RunTestData
    | RunBackup
    | RunRestore
    | RunRemove
    | PrintDataLocation
    | GenerateConfigFile
    | PrintHelp

parseArgs :: IO Command
parseArgs = f <$> getArgs
  where
    f ("run":xs) =
        case xs of
            ("init":_)      -> RunInit
            ("test-data":_) -> RunTestData
            ("backup":_)    -> RunBackup
            ("restore":_)   -> RunRestore
            ("remove":_)    -> RunRemove
            _               -> PrintHelp
    f ("location":_) = PrintDataLocation
    f ("gen":_)      = GenerateConfigFile
    f _              = PrintHelp

printHelp :: FilePath -> IO ()
printHelp _ = putStrLn "Usage: ds [run [init | test-data | backup | restore | remove] | location | gen | help]"

main :: IO ()
main = do
    arg <- parseArgs
    ($ dsYaml) $ case arg of
        RunInit -> runInit
        RunTestData -> runTestData
        RunBackup -> runBackup
        RunRestore -> runRestore
        RunRemove -> runRemove
        PrintDataLocation -> printDataLocation
        GenerateConfigFile -> generateEmptyYaml
        PrintHelp -> printHelp
