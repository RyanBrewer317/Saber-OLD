{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_prettyprinter (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [1,7,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ryan/.cabal/store/ghc-8.8.4/prettyprinter-1.7.1-2b832bc32d39b064286321c8308dd0355223f1280928b19d91911f9193efdd3c/bin"
libdir     = "/home/ryan/.cabal/store/ghc-8.8.4/prettyprinter-1.7.1-2b832bc32d39b064286321c8308dd0355223f1280928b19d91911f9193efdd3c/lib"
dynlibdir  = "/home/ryan/.cabal/store/ghc-8.8.4/prettyprinter-1.7.1-2b832bc32d39b064286321c8308dd0355223f1280928b19d91911f9193efdd3c/lib"
datadir    = "/home/ryan/.cabal/store/ghc-8.8.4/prettyprinter-1.7.1-2b832bc32d39b064286321c8308dd0355223f1280928b19d91911f9193efdd3c/share"
libexecdir = "/home/ryan/.cabal/store/ghc-8.8.4/prettyprinter-1.7.1-2b832bc32d39b064286321c8308dd0355223f1280928b19d91911f9193efdd3c/libexec"
sysconfdir = "/home/ryan/.cabal/store/ghc-8.8.4/prettyprinter-1.7.1-2b832bc32d39b064286321c8308dd0355223f1280928b19d91911f9193efdd3c/etc"

getBinDir     = catchIO (getEnv "prettyprinter_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "prettyprinter_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "prettyprinter_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "prettyprinter_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prettyprinter_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prettyprinter_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
