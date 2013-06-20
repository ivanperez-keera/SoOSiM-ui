{-# LANGUAGE CPP #-}
module Paths.CustomPaths
  (
#ifndef portable_install
    module Paths_SoOSiM_ui
#else
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
#endif
#ifndef linux_HOST_OS
  , module Paths.CustomPaths
#endif
  )
 where

#ifndef portable_install
import Paths_SoOSiM_ui
#else
import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude
#endif

#ifndef linux_HOST_OS
vendorKey :: String
vendorKey = ""

programKey :: String
programKey = ""
#endif

#ifdef portable_install
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,2], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "."
libdir     = "."
datadir    = "data"
libexecdir = "."

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "SoOSiM_ui_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "SoOSiM_ui_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "SoOSiM_ui_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "SoOSiM_ui_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
#endif
