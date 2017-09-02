module Paths_amplified_challenge (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/moonlight/prg/ampified-challenge/.cabal-sandbox/bin"
libdir     = "/home/moonlight/prg/ampified-challenge/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/amplified-challenge-0.1.0.0-C5wqZODbC2v3MGzX1G0DvC"
datadir    = "/home/moonlight/prg/ampified-challenge/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/amplified-challenge-0.1.0.0"
libexecdir = "/home/moonlight/prg/ampified-challenge/.cabal-sandbox/libexec"
sysconfdir = "/home/moonlight/prg/ampified-challenge/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "amplified_challenge_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "amplified_challenge_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "amplified_challenge_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "amplified_challenge_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "amplified_challenge_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
