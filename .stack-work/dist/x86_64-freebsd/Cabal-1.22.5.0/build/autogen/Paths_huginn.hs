module Paths_huginn (
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

bindir     = "/usr/home/casper/projects/falcon/raven/.stack-work/install/x86_64-freebsd/lts-6.4/7.10.3/bin"
libdir     = "/usr/home/casper/projects/falcon/raven/.stack-work/install/x86_64-freebsd/lts-6.4/7.10.3/lib/x86_64-freebsd-ghc-7.10.3/huginn-0.1.0.0-IqzXpp8LztLLTlnmoPtP9a"
datadir    = "/usr/home/casper/projects/falcon/raven/.stack-work/install/x86_64-freebsd/lts-6.4/7.10.3/share/x86_64-freebsd-ghc-7.10.3/huginn-0.1.0.0"
libexecdir = "/usr/home/casper/projects/falcon/raven/.stack-work/install/x86_64-freebsd/lts-6.4/7.10.3/libexec"
sysconfdir = "/usr/home/casper/projects/falcon/raven/.stack-work/install/x86_64-freebsd/lts-6.4/7.10.3/etc"

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
