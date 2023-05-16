{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_saber (
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
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ryan/.cabal/bin"
libdir     = "/home/ryan/.cabal/lib/x86_64-linux-ghc-8.8.4/saber-0.1.0.0-inplace-saber"
dynlibdir  = "/home/ryan/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/ryan/.cabal/share/x86_64-linux-ghc-8.8.4/saber-0.1.0.0"
libexecdir = "/home/ryan/.cabal/libexec/x86_64-linux-ghc-8.8.4/saber-0.1.0.0"
sysconfdir = "/home/ryan/.cabal/etc"

getBinDir     = catchIO (getEnv "saber_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "saber_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "saber_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "saber_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "saber_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "saber_sysconfdir") (\_ -> return sysconfdir)




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
