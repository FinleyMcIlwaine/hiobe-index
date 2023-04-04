{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use unless" #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Text.Lazy qualified as L
import Options.Applicative
import System.Directory
import System.Environment
import System.Exit

import Database.SQLite.Simple
import Web.Scotty.Trans hiding (header)

import GHC.Debug.Stub
import GHC.Eventlog.Socket qualified

import Handlers
import State

main :: IO ()
main = runHiobe =<< execParser hiobeConfigParserInfo

runHiobe :: HiobeConfig -> IO ()
runHiobe HiobeConfig{..} =
    -- Open ghc-debug and eventlog sockets, if necessary
    maybeGhcDebug $ do
      maybeGhcEventlogSocket

      -- Ensure DB exists
      dbExists <- doesFileExist hiobeConfigDbFile
      when (not dbExists) . dieUsage $
        "Database file \"" ++ hiobeConfigDbFile ++ "\" does not exist"

      -- Open singular database connection, wrap it in an MVar,
      -- initialise app state
      withConnection hiobeConfigDbFile $ \conn -> do
        db <- newMVar conn
        sync <- newTVarIO (initState db)

        let runActionToIO m = runReaderT (runHiobeM m) sync

        scottyT 3000 runActionToIO handlers
  where
    maybeGhcDebug :: IO () -> IO ()
    maybeGhcDebug theMain =
      case hiobeConfigWithGhcDebug of
        Nothing -> theMain
        Just p  -> do
          putStrLn "********************************************"
          putStrLn "*****    Opening a ghc-debug socket    *****"
          putStrLn "********************************************"
          setEnv "GHC_DEBUG_SOCKET" p
          withGhcDebug theMain

    maybeGhcEventlogSocket :: IO ()
    maybeGhcEventlogSocket =
      case hiobeConfigEventlogSocket of
        Nothing -> return ()
        Just p  -> do
          putStrLn "********************************************"
          putStrLn "*****   Opening a ghc-eventlog socket  *****"
          putStrLn "********************************************"
          GHC.Eventlog.Socket.start p

{------------------------------------------------------------------------------
  CLI utilities
------------------------------------------------------------------------------}

data HiobeConfig = HiobeConfig
    { hiobeConfigDbFile         :: FilePath
    , hiobeConfigWithGhcDebug   :: Maybe FilePath
    , hiobeConfigEventlogSocket :: Maybe FilePath
    }
  deriving Show

hiobeConfigParser :: Parser HiobeConfig
hiobeConfigParser = HiobeConfig
    <$> strOption (
               long "database"
            <> metavar "DATABASE_PATH"
            <> help "Path to existing HIOBE Index SQLite database file"
          )
    <*> optional ( strOption $
               long "ghc-debug-socket"
            <> help (
                      "Path at which to create a ghc-debug socket. "
                   ++ "Set to empty string to create a socket in the default "
                   ++ "ghc-debug socket directory (default: no socket, no debug)"
                 )
          )
    <*> optional ( strOption $
               long "ghc-eventlog-socket"
            <> metavar "SOCKET_PATH"
            <> help (
                      "Path at which to create a GHC eventlog socket (default: "
                   ++ "no socket, write eventlog to file)"
                 )
          )

hiobeConfigParserInfo :: ParserInfo HiobeConfig
hiobeConfigParserInfo =
    info
      (hiobeConfigParser <**> helper)
      (    fullDesc
        <> progDesc "Run the HIOBE Index server"
        <> header "Haskell Is Obviously Better at Everything"
      )

dieUsage :: String -> IO ()
dieUsage err = do
    handleParseResult . Failure $
      parserFailure
        defaultPrefs
        hiobeConfigParserInfo
        (ErrorMsg $ "Error: " ++ err)
        mempty
