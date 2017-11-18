module Paths_restful_cyclomatic (
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

bindir     = "/home/jibin/workspace/new_Disributed/restful-cyclomatic/.stack-work/install/x86_64-linux/lts-6.3/7.10.3/bin"
libdir     = "/home/jibin/workspace/new_Disributed/restful-cyclomatic/.stack-work/install/x86_64-linux/lts-6.3/7.10.3/lib/x86_64-linux-ghc-7.10.3/restful-cyclomatic-0.1.0.0-7f1v8GIa5QiKIpN2fDUFTB"
datadir    = "/home/jibin/workspace/new_Disributed/restful-cyclomatic/.stack-work/install/x86_64-linux/lts-6.3/7.10.3/share/x86_64-linux-ghc-7.10.3/restful-cyclomatic-0.1.0.0"
libexecdir = "/home/jibin/workspace/new_Disributed/restful-cyclomatic/.stack-work/install/x86_64-linux/lts-6.3/7.10.3/libexec"
sysconfdir = "/home/jibin/workspace/new_Disributed/restful-cyclomatic/.stack-work/install/x86_64-linux/lts-6.3/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "restful_cyclomatic_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "restful_cyclomatic_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "restful_cyclomatic_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "restful_cyclomatic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "restful_cyclomatic_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
