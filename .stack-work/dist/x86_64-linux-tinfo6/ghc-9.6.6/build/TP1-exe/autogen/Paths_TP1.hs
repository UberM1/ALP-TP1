{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_TP1 (
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
bindir     = "/home/tecla/facu/ALP/TP1/.stack-work/install/x86_64-linux-tinfo6/dd7e8ed73c6621f15ed41a92816c4ebf8f1f1f1d69645bcadaf847292d9720ad/9.6.6/bin"
libdir     = "/home/tecla/facu/ALP/TP1/.stack-work/install/x86_64-linux-tinfo6/dd7e8ed73c6621f15ed41a92816c4ebf8f1f1f1d69645bcadaf847292d9720ad/9.6.6/lib/x86_64-linux-ghc-9.6.6/TP1-0.1.0.0-75UgaYjGAMU2VUsyas8Vz3-TP1-exe"
dynlibdir  = "/home/tecla/facu/ALP/TP1/.stack-work/install/x86_64-linux-tinfo6/dd7e8ed73c6621f15ed41a92816c4ebf8f1f1f1d69645bcadaf847292d9720ad/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/tecla/facu/ALP/TP1/.stack-work/install/x86_64-linux-tinfo6/dd7e8ed73c6621f15ed41a92816c4ebf8f1f1f1d69645bcadaf847292d9720ad/9.6.6/share/x86_64-linux-ghc-9.6.6/TP1-0.1.0.0"
libexecdir = "/home/tecla/facu/ALP/TP1/.stack-work/install/x86_64-linux-tinfo6/dd7e8ed73c6621f15ed41a92816c4ebf8f1f1f1d69645bcadaf847292d9720ad/9.6.6/libexec/x86_64-linux-ghc-9.6.6/TP1-0.1.0.0"
sysconfdir = "/home/tecla/facu/ALP/TP1/.stack-work/install/x86_64-linux-tinfo6/dd7e8ed73c6621f15ed41a92816c4ebf8f1f1f1d69645bcadaf847292d9720ad/9.6.6/etc"

getBinDir     = catchIO (getEnv "TP1_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "TP1_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "TP1_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "TP1_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TP1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TP1_sysconfdir") (\_ -> return sysconfdir)



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
