module DS
    ( generateEmptyYaml
    , runInit
    , runTestData
    , runBackup
    , runRestore
    , runRemove
    , printDataLocation
    ) where

import           Control.Monad
import qualified Data.Yaml        as Y
import           DS.Types
import qualified System.Directory as SD
import           System.Exit      (ExitCode (..), exitFailure)
import           System.IO        (Handle, hGetContents, hPutStrLn, stderr)
import qualified System.Process   as SP

generateEmptyYaml :: FilePath -> IO ()
generateEmptyYaml path = do
    isExist <- SD.doesFileExist path
    if not isExist
        then Y.encodeFile path emptyDSConfig
        else hPutStrLn stderr (path ++ " exists, don't overwrite this.") >> exitFailure

fileIsNotValid :: FilePath -> IO ()
fileIsNotValid path = putStrLn $ path ++ " is not found or not yaml."

commandNotDefined :: String -> IO ()
commandNotDefined key = putStrLn $ key ++ " is not defined."

printCommandFailure :: Int -> IO ()
printCommandFailure n = putStrLn $ "\nFail.\nReturn code: " ++ show n

printCommandSuccess :: IO ()
printCommandSuccess = putStrLn "\nSuccess."

printPipe :: String -> Handle -> IO ()
printPipe prefix h = hGetContents h >>= mapM_ (\l -> putStrLn $ prefix ++ l) . lines

printStdout :: Maybe Handle -> IO ()
printStdout (Just h) = printPipe "STDOUT> " h
printStdout Nothing  = putStrLn "STDOUT is not enabled."

printStderr :: Maybe Handle -> IO ()
printStderr (Just h) = printPipe "STDERR> " h
printStderr Nothing  = putStrLn "STDERR is not enabled."

withDSConfig :: FilePath -> (DSConfig -> IO ()) -> IO ()
withDSConfig path f = Y.decodeFile path >>= maybe (fileIsNotValid path) f

runCommand :: String -> (DSConfig -> Maybe Command) -> FilePath -> IO ()
runCommand key f path = withDSConfig path (go . f)
  where
    go Nothing    = commandNotDefined key
    go (Just cmd) = do
        (_, moh, meh, ph) <- SP.createProcess (SP.shell $ command cmd)
            { SP.std_out = SP.CreatePipe
            , SP.std_err = SP.CreatePipe
            }
        exitCode <- SP.waitForProcess ph
        printStdout moh
        printStderr meh
        case exitCode of
            ExitFailure n -> printCommandFailure n
            ExitSuccess   -> printCommandSuccess

runInit :: FilePath -> IO ()
runInit = runCommand "init" dsInit

runTestData :: FilePath -> IO ()
runTestData = runCommand "test-data" dsTestData

runBackup :: FilePath -> IO ()
runBackup = runCommand "backup" dsBackup

runRestore :: FilePath -> IO ()
runRestore = runCommand "restore" dsRestore

runRemove :: FilePath -> IO ()
runRemove = runCommand "remove" dsRemove

printDataLocation :: FilePath -> IO ()
printDataLocation path = withDSConfig path $ mapM_ (\location -> putStrLn $ "- " ++ location) . dsDataLocation
