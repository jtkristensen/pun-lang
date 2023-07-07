{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_pun_lang (
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
bindir     = "/home/joachim/Documents/github/phd/pun-lang/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/4826fa4b68d0362495782d0e9435b6affa1e5637a79839237d2af68bff1c5f80/9.4.5/bin"
libdir     = "/home/joachim/Documents/github/phd/pun-lang/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/4826fa4b68d0362495782d0e9435b6affa1e5637a79839237d2af68bff1c5f80/9.4.5/lib/x86_64-linux-ghc-9.4.5/pun-lang-0.1.0.0-6FOexedHuTgGAsUpAecJLG"
dynlibdir  = "/home/joachim/Documents/github/phd/pun-lang/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/4826fa4b68d0362495782d0e9435b6affa1e5637a79839237d2af68bff1c5f80/9.4.5/lib/x86_64-linux-ghc-9.4.5"
datadir    = "/home/joachim/Documents/github/phd/pun-lang/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/4826fa4b68d0362495782d0e9435b6affa1e5637a79839237d2af68bff1c5f80/9.4.5/share/x86_64-linux-ghc-9.4.5/pun-lang-0.1.0.0"
libexecdir = "/home/joachim/Documents/github/phd/pun-lang/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/4826fa4b68d0362495782d0e9435b6affa1e5637a79839237d2af68bff1c5f80/9.4.5/libexec/x86_64-linux-ghc-9.4.5/pun-lang-0.1.0.0"
sysconfdir = "/home/joachim/Documents/github/phd/pun-lang/.stack-work/install/x86_64-linux-tinfo6-libc6-pre232/4826fa4b68d0362495782d0e9435b6affa1e5637a79839237d2af68bff1c5f80/9.4.5/etc"

getBinDir     = catchIO (getEnv "pun_lang_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "pun_lang_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "pun_lang_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "pun_lang_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "pun_lang_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "pun_lang_sysconfdir") (\_ -> return sysconfdir)




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
