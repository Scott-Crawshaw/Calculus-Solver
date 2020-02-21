{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Calculus_Solver (
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

bindir     = "/Users/scottcrawshaw/Programming/Calculus-Solver/.stack-work/install/x86_64-osx/3472e8bb96b0973f0800e0dd4658bff1843566932c04fd8a3b3c562e287de5a5/8.8.2/bin"
libdir     = "/Users/scottcrawshaw/Programming/Calculus-Solver/.stack-work/install/x86_64-osx/3472e8bb96b0973f0800e0dd4658bff1843566932c04fd8a3b3c562e287de5a5/8.8.2/lib/x86_64-osx-ghc-8.8.2/Calculus-Solver-0.1.0.0-46fmzfAedw8HiBFFeG7gGt-Calculus-Solver-exe"
dynlibdir  = "/Users/scottcrawshaw/Programming/Calculus-Solver/.stack-work/install/x86_64-osx/3472e8bb96b0973f0800e0dd4658bff1843566932c04fd8a3b3c562e287de5a5/8.8.2/lib/x86_64-osx-ghc-8.8.2"
datadir    = "/Users/scottcrawshaw/Programming/Calculus-Solver/.stack-work/install/x86_64-osx/3472e8bb96b0973f0800e0dd4658bff1843566932c04fd8a3b3c562e287de5a5/8.8.2/share/x86_64-osx-ghc-8.8.2/Calculus-Solver-0.1.0.0"
libexecdir = "/Users/scottcrawshaw/Programming/Calculus-Solver/.stack-work/install/x86_64-osx/3472e8bb96b0973f0800e0dd4658bff1843566932c04fd8a3b3c562e287de5a5/8.8.2/libexec/x86_64-osx-ghc-8.8.2/Calculus-Solver-0.1.0.0"
sysconfdir = "/Users/scottcrawshaw/Programming/Calculus-Solver/.stack-work/install/x86_64-osx/3472e8bb96b0973f0800e0dd4658bff1843566932c04fd8a3b3c562e287de5a5/8.8.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Calculus_Solver_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Calculus_Solver_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Calculus_Solver_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Calculus_Solver_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Calculus_Solver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Calculus_Solver_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
