{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_workdisplay (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/james/.cabal/bin"
libdir     = "/home/james/.cabal/lib/x86_64-linux-ghc-8.8.1/workdisplay-0.1.0.0-inplace"
dynlibdir  = "/home/james/.cabal/lib/x86_64-linux-ghc-8.8.1"
datadir    = "/home/james/.cabal/share/x86_64-linux-ghc-8.8.1/workdisplay-0.1.0.0"
libexecdir = "/home/james/.cabal/libexec/x86_64-linux-ghc-8.8.1/workdisplay-0.1.0.0"
sysconfdir = "/home/james/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "workdisplay_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "workdisplay_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "workdisplay_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "workdisplay_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "workdisplay_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "workdisplay_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
