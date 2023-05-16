{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_attoparsec (
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
version = Version [0,14,4] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/ryan/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-80cc26f19041b1fd56e6d0bdd03fe54cc15fef3f13004cf61cb7ab56e06b38d5/bin"
libdir     = "/home/ryan/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-80cc26f19041b1fd56e6d0bdd03fe54cc15fef3f13004cf61cb7ab56e06b38d5/lib"
dynlibdir  = "/home/ryan/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-80cc26f19041b1fd56e6d0bdd03fe54cc15fef3f13004cf61cb7ab56e06b38d5/lib"
datadir    = "/home/ryan/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-80cc26f19041b1fd56e6d0bdd03fe54cc15fef3f13004cf61cb7ab56e06b38d5/share"
libexecdir = "/home/ryan/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-80cc26f19041b1fd56e6d0bdd03fe54cc15fef3f13004cf61cb7ab56e06b38d5/libexec"
sysconfdir = "/home/ryan/.cabal/store/ghc-8.8.4/attoparsec-0.14.4-80cc26f19041b1fd56e6d0bdd03fe54cc15fef3f13004cf61cb7ab56e06b38d5/etc"

getBinDir     = catchIO (getEnv "attoparsec_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "attoparsec_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "attoparsec_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "attoparsec_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "attoparsec_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "attoparsec_sysconfdir") (\_ -> return sysconfdir)




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
