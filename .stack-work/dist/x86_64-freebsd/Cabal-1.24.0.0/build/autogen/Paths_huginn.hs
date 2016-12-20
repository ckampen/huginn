{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_huginn (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
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
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/usr/home/casper/projects/huginn/.stack-work/install/x86_64-freebsd/lts-7.14/8.0.1/bin"
libdir     = "/usr/home/casper/projects/huginn/.stack-work/install/x86_64-freebsd/lts-7.14/8.0.1/lib/x86_64-freebsd-ghc-8.0.1/huginn-0.1.0.0-H85hIuVlPwkFCtmCyCbyhs"
datadir    = "/usr/home/casper/projects/huginn/.stack-work/install/x86_64-freebsd/lts-7.14/8.0.1/share/x86_64-freebsd-ghc-8.0.1/huginn-0.1.0.0"
libexecdir = "/usr/home/casper/projects/huginn/.stack-work/install/x86_64-freebsd/lts-7.14/8.0.1/libexec"
sysconfdir = "/usr/home/casper/projects/huginn/.stack-work/install/x86_64-freebsd/lts-7.14/8.0.1/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "huginn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "huginn_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "huginn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "huginn_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "huginn_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
