module Paths_Logic (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/adam/.cabal/bin"
libdir     = "/home/adam/.cabal/lib/Logic-0.1.0.0/ghc-7.4.1"
datadir    = "/home/adam/.cabal/share/Logic-0.1.0.0"
libexecdir = "/home/adam/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Logic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Logic_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Logic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Logic_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
