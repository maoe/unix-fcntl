{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module System.Posix.FileControl
  ( fcntl
  , Fcntl(..)

  -- * File descriptor flags
  , FileDescriptorFlags
  , pattern FD_CLOEXEC

  -- * File status flags
  , FileStatusFlags
  , pattern O_RDONLY
  , pattern O_WRONLY
  , pattern O_RDWR
  , pattern O_ACCMODE
  , pattern O_CREAT
  , pattern O_EXCL
  , pattern O_NONBLOCK
  , pattern O_NOCTTY
  , pattern O_TRUNC
  , pattern O_APPEND
  , pattern O_NDELAY

  -- * Advisory and mandatory locking
  , Flock
  , newFlock
  , flockType
  , FlockType
  , pattern F_RDLCK
  , pattern F_WRLCK
  , pattern F_UNLCK
  , flockWhence
  , FlockWhence
  , pattern SEEK_SET
  , pattern SEEK_CUR
  , pattern SEEK_END
  , flockStart
  , flockLen
  , flockPid


#if defined(_GNU_SOURCE)
  -- * Managing signals
  , FOwnerEx
  , newFOwnerEx
  , fOwnerExType
  , fOwnerExPid
  , OwnerType
  , pattern F_OWNER_TID
  , pattern F_OWNER_PID
  , pattern F_OWNER_PGRP

  -- * File and directory change notification (dnotify)
  , DNotify
  , pattern DN_ACCESS
  , pattern DN_MODIFY
  , pattern DN_CREATE
  , pattern DN_DELETE
  , pattern DN_RENAME
  , pattern DN_ATTRIB
#endif

#if defined(F_GET_SEALS)
  -- * File sealing
  , FileSeal
  , pattern F_SEAL_SEAL
  , pattern F_SEAL_SHRINK
  , pattern F_SEAL_GROW
  , pattern F_SEAL_WRITE
#endif
  ) where
import Control.Applicative
import GHC.Conc (Signal)
import Foreign
import Foreign.C
import System.Posix.Types
import Prelude

import Foreign.Var hiding (get)

#include <unistd.h>
#include <fcntl.h>
#include "custom-template-hsc.h"

-- | Perform one of the operations in 'Fcntl'
fcntl :: Fd -> Fcntl a -> IO a
fcntl fd cmd = case cmd of
  -- Duplicating a file descriptor
  F_DUPFD minFd ->
    fcntl_set_int fd (#const F_DUPFD) minFd
  F_DUPFD_CLOEXEC minFd ->
    fcntl_set_int fd (#const F_DUPFD_CLOEXEC) minFd

  -- File descriptor flags
  F_GETFD ->
    FileDescriptorFlags <$> fcntl_get_int fd (#const F_GETFD)
  F_SETFD (FileDescriptorFlags flags) ->
    fcntl_set_int_ fd (#const F_SETFD) flags

  -- File status flags
  F_GETFL ->
    FileStatusFlags <$> fcntl_get_int fd (#const F_GETFL)
  F_SETFL (FileStatusFlags flags) ->
    fcntl_set_int_ fd (#const F_SETFL) flags

  -- Advisory or mandatory locking
  F_GETLK ->
    fcntl_get_flock fd (#const F_GETLK)
  F_SETLK flock ->
    fcntl_set_flock fd (#const F_SETLK) flock
  F_SETLKW flock ->
    fcntl_set_flock fd (#const F_SETLKW) flock

#if defined(_GNU_SOURCE) && defined(F_OFD_GETLK)
  -- Open description locks
  F_OFD_GETLK ->
    fcntl_get_flock fd (#const F_OFD_GETLK)
  F_OFD_SETLK flock ->
    fcntl_set_flock fd (#const F_OFD_SETLK) flock
  F_OFD_SETLKW flock ->
    fcntl_set_flock fd (#const F_OFD_SETLKW) flock
#endif

  -- Managing signals
  F_GETOWN ->
    fcntl_get_int fd (#const F_GETOWN)
  F_SETOWN pid ->
    fcntl_set_int_ fd (#const F_SETFL) pid
#if defined(_GNU_SOURCE)
  F_GETOWN_EX ->
    fcntl_get_f_owner_ex fd (#const F_GETOWN_EX)
  F_SETOWN_EX foe ->
    fcntl_set_f_owner_ex fd (#const F_SETOWN_EX) foe
  F_GETSIG ->
    fcntl_get_int fd (#const F_GETSIG)
  F_SETSIG sig ->
    fcntl_set_int_ fd (#const F_SETSIG) sig

  -- Leases (Linux 2.4)
  F_GETLEASE ->
    FlockType <$> fcntl_get_int fd (#const F_GETLEASE)
  F_SETLEASE (FlockType ty) ->
    fcntl_set_int_ fd (#const F_SETLEASE) ty

  -- File and directory change notification (dnotify; Linux 2.4)
  F_NOTIFY (DNotify n) ->
    fcntl_set_int_ fd (#const F_NOTIFY) n

  -- Changing the capacity of a pipe
  F_GETPIPE_SZ ->
    fcntl_get_int fd (#const F_GETPIPE_SZ)
  F_SETPIPE_SZ size ->
    fcntl_set_int_ fd (#const F_SETPIPE_SZ) size
#endif

#if defined(F_GET_SEALS)
  -- File sealing
  F_GET_SEALS ->
    fcntl_get_int fd (#const F_GET_SEALS)
  F_ADD_SEALS ->
    fcntl_set_int_ fd (#const F_ADD_SEALS)
#endif

-- | Type of operations which 'fcntl' can perform. Available operations vary
-- depending on platforms. Please consult manpage on your platform for details.
--
-- All possible operations are:
--
-- * Duplicating a file descriptor
--
--     * 'F_DUPFD'
--     * 'F_DUPFD_CLOEXEC'
--
-- * File descriptor flags
--
--     * 'F_GETFD':
--     * 'F_SETFD'
--
-- * File status flags
--
--    * 'F_GETFL'
--    * 'F_SETFL'
--
-- * Advisory or mandatory locking
--
--    * 'F_GETLK'
--    * 'F_SETLK'
--    * 'F_SETLKW'
--
-- * Open file description locks (Linux 3.15 or later)
--
--    * 'F_OFD_GETLK'
--    * 'F_OFD_SETLK'
--    * 'F_OFD_SETLKW'
--
-- * Managing signals
--
--    * 'F_GETOWN'
--    * 'F_SETOWN'
--    * 'F_GETOWN_EX'
--    * 'F_SETOWN_EX'
--    * 'F_GETSIG'
--    * 'F_SETSIG'
--
-- * Leases
--
--    * 'F_GETLEASE'
--    * 'F_SETLEASE'
--
-- * File and directory change notification (dnotify; Linux 2.4 or later)
--
--    * 'F_NOTIFY'
--
-- * Changing the capacity of a pipe
--
--    * 'F_GETPIPE_SZ'
--    * 'F_SETPIPE_SZ'
--
-- * File leasing
--
--    * 'F_GET_SEALS'
--    * 'F_ADD_SEALS'
data Fcntl a where
  -- Duplicating a file descriptor
  F_DUPFD :: Fd -> Fcntl Fd
  F_DUPFD_CLOEXEC :: Fd -> Fcntl Fd

  -- File descriptor flags
  F_GETFD :: Fcntl FileDescriptorFlags
  F_SETFD :: FileDescriptorFlags -> Fcntl ()

  -- File status flags
  F_GETFL :: Fcntl FileStatusFlags
  F_SETFL :: FileStatusFlags -> Fcntl ()

  -- Advisory or mandatory locking
  F_GETLK :: Fcntl Flock
  F_SETLK :: Flock -> Fcntl ()
  F_SETLKW :: Flock -> Fcntl ()

#if defined(_GNU_SOURCE) && defined(F_OFD_GETLK)
  -- Open file description locks
  F_OFD_GETLK :: Fcntl Flock
  F_OFD_SETLK :: Flock -> Fcntl ()
  F_OFD_SETLKW :: Flock -> Fcntl ()
#endif

  -- Managing signals
  F_GETOWN :: Fcntl ProcessID
  F_SETOWN :: ProcessID -> Fcntl ()

#if defined(_GNU_SOURCE)
  F_GETOWN_EX :: Fcntl FOwnerEx
  F_SETOWN_EX :: FOwnerEx -> Fcntl ()

  F_GETSIG :: Fcntl Signal
  F_SETSIG :: Signal -> Fcntl ()

  -- Leases
  F_GETLEASE :: Fcntl FlockType
  F_SETLEASE :: FlockType -> Fcntl ()

  -- File and directory change notification (dnotify; Linux 2.4)
#if defined(F_NOTIFY)
  F_NOTIFY :: DNotify -> Fcntl ()
#endif

  -- Changing the capacity of a pipe
  F_GETPIPE_SZ :: Fcntl Int
  F_SETPIPE_SZ :: Int -> Fcntl ()
#endif

#if defined(F_GET_SEALS)
  -- File sealing (Linux 3.17)
  F_GET_SEALS :: Fcntl Seal
  F_ADD_SEALS :: Seal -> Fcntl ()
#endif

-- Helper functions

fcntl_get_int :: Integral a => Fd -> CInt -> IO a
fcntl_get_int fd cmd =
  fromIntegral <$> throwErrnoIfMinus1 "fcntl"
    (c_fcntl_get_int (fromIntegral fd) cmd)

foreign import ccall safe "fcntl"
  c_fcntl_get_int :: CInt -> CInt -> IO CInt

fcntl_set_int :: (Integral a, Integral b) => Fd -> CInt -> a -> IO b
fcntl_set_int fd cmd n =
  fromIntegral <$> throwErrnoIfMinus1 "fcntl"
    (c_fcntl_set_int (fromIntegral fd) cmd (fromIntegral n))

fcntl_set_int_ :: Integral a => Fd -> CInt -> a -> IO ()
fcntl_set_int_ fd cmd n =
  throwErrnoIfMinus1_ "fcntl"
    (c_fcntl_set_int (fromIntegral fd) cmd (fromIntegral n))

foreign import ccall safe "fcntl"
  c_fcntl_set_int :: CInt -> CInt -> CInt -> IO CInt

fcntl_get_flock :: Fd -> CInt -> IO Flock
fcntl_get_flock fd cmd = do
  flock <- newFlock
  throwErrnoIfMinus1_ "fcntl" $
    withFlock flock $ c_fcntl_get_flock (fromIntegral fd) cmd
  return flock

foreign import ccall safe "fcntl"
  c_fcntl_get_flock :: CInt -> CInt -> Ptr Flock -> IO CInt

fcntl_set_flock :: Fd -> CInt -> Flock -> IO ()
fcntl_set_flock fd cmd flock =
  throwErrnoIfMinus1_ "fcntl" $
    withFlock flock $ c_fcntl_set_flock (fromIntegral fd) cmd

foreign import ccall safe "fcntl"
  c_fcntl_set_flock :: CInt -> CInt -> Ptr Flock -> IO CInt

#if defined(_GNU_SOURCE)

fcntl_get_f_owner_ex :: Fd -> CInt -> IO FOwnerEx
fcntl_get_f_owner_ex fd cmd = do
  owner <- newFOwnerEx
  throwErrnoIfMinus1_ "fcntl" $
    withFOwnerEx owner $ c_fcntl_get_f_owner_ex (fromIntegral fd) cmd
  return owner

foreign import ccall safe "fcntl"
  c_fcntl_get_f_owner_ex :: CInt -> CInt -> Ptr FOwnerEx -> IO CInt

fcntl_set_f_owner_ex :: Fd -> CInt -> FOwnerEx -> IO ()
fcntl_set_f_owner_ex fd cmd owner =
  throwErrnoIfMinus1_ "fcntl" $
    withFOwnerEx owner $ c_fcntl_set_f_owner_ex (fromIntegral fd) cmd

foreign import ccall safe "fcntl"
  c_fcntl_set_f_owner_ex :: CInt -> CInt -> Ptr FOwnerEx -> IO CInt

#endif

-----------------------------------------------------------
-- File descriptor flags

newtype FileDescriptorFlags = FileDescriptorFlags CInt

#DEFINE_PATTERN "FD_CLOEXEC", "FileDescriptorFlags"

-----------------------------------------------------------
-- File status flags

newtype FileStatusFlags = FileStatusFlags CInt

-- File access modes
#DEFINE_BIDIRECTIONAL_PATTERN "O_RDONLY", "FileStatusFlags"
#DEFINE_BIDIRECTIONAL_PATTERN "O_WRONLY", "FileStatusFlags"
#DEFINE_BIDIRECTIONAL_PATTERN "O_RDWR", "FileStatusFlags"
#DEFINE_BIDIRECTIONAL_PATTERN "O_ACCMODE", "FileStatusFlags"

-- Open-time flags
#DEFINE_BIDIRECTIONAL_PATTERN "O_CREAT", "FileStatusFlags"
#DEFINE_BIDIRECTIONAL_PATTERN "O_EXCL", "FileStatusFlags"
#DEFINE_BIDIRECTIONAL_PATTERN "O_NONBLOCK", "FileStatusFlags"
#DEFINE_BIDIRECTIONAL_PATTERN "O_NOCTTY", "FileStatusFlags"
#DEFINE_BIDIRECTIONAL_PATTERN "O_TRUNC", "FileStatusFlags"

-- I/O operating modes
#DEFINE_BIDIRECTIONAL_PATTERN "O_APPEND", "FileStatusFlags"
#DEFINE_BIDIRECTIONAL_PATTERN "O_NDELAY", "FileStatusFlags"

-----------------------------------------------------------
-- Advisory and mandatory locking

newtype Flock = Flock (ForeignPtr Flock)

withFlock :: Flock -> (Ptr Flock -> IO a) -> IO a
withFlock (Flock fptr) = withForeignPtr fptr

-- | Allocate a flock structure. The allocated memory will be garbage collected
-- automatically.
newFlock :: IO Flock
newFlock = Flock <$> mallocForeignPtrBytes (#size struct flock)

newtype FlockType = FlockType CInt

#DEFINE_PATTERN "F_RDLCK", "FlockType"
#DEFINE_PATTERN "F_WRLCK", "FlockType"
#DEFINE_PATTERN "F_UNLCK", "FlockType"

flockType :: Flock -> Var FlockType
flockType flock = Var get set
  where
    get = FlockType <$> withFlock flock (#peek struct flock, l_type)
    set (FlockType ty) = withFlock flock $ \p ->
      (#poke struct flock, l_type) p ty

newtype FlockWhence = FlockWhence CInt

#DEFINE_PATTERN "SEEK_SET", "FlockWhence"
#DEFINE_PATTERN "SEEK_CUR", "FlockWhence"
#DEFINE_PATTERN "SEEK_END", "FlockWhence"

flockWhence :: Flock -> Var FlockWhence
flockWhence flock = Var get set
  where
    get = FlockWhence <$> withFlock flock (#peek struct flock, l_whence)
    set (FlockWhence whence) = withFlock flock $ \p ->
      (#poke struct flock, l_whence) p whence

flockStart :: Flock -> Var FileOffset
flockStart flock = Var get set
  where
    get = withFlock flock (#peek struct flock, l_start)
    set offset = withFlock flock $ \p -> (#poke struct flock, l_start) p offset

flockLen :: Flock -> Var FileOffset
flockLen flock = Var get set
  where
    get = withFlock flock (#peek struct flock, l_len)
    set len = withFlock flock $ \p -> (#poke struct flock, l_len) p len

flockPid :: Flock -> Var ProcessID
flockPid flock = Var get set
  where
    get = withFlock flock (#peek struct flock, l_pid)
    set pid = withFlock flock $ \p -> (#poke struct flock, l_pid) p pid

-----------------------------------------------------------
-- Managing signals

#if defined(_GNU_SOURCE)

newtype FOwnerEx = FOwnerEx (ForeignPtr FOwnerEx)

withFOwnerEx :: FOwnerEx -> (Ptr FOwnerEx -> IO a) -> IO a
withFOwnerEx (FOwnerEx fptr) = withForeignPtr fptr

newFOwnerEx :: IO FOwnerEx
newFOwnerEx = FOwnerEx <$> mallocForeignPtrBytes (#size struct f_owner_ex)

newtype OwnerType = OwnerType CInt

#DEFINE_PATTERN "F_OWNER_TID", "OwnerType"
#DEFINE_PATTERN "F_OWNER_PID", "OwnerType"
#DEFINE_PATTERN "F_OWNER_PGRP", "OwnerType"

fOwnerExType :: FOwnerEx -> Var OwnerType
fOwnerExType foe = Var get set
  where
    get = OwnerType <$> withFOwnerEx foe (#peek struct f_owner_ex, type)
    set (OwnerType ty) = withFOwnerEx foe $ \p ->
      (#poke struct f_owner_ex, type) p ty

fOwnerExPid :: FOwnerEx -> Var ProcessID
fOwnerExPid foe = Var get set
  where
    get = withFOwnerEx foe (#peek struct f_owner_ex, pid)
    set pid = withFOwnerEx foe $ \p -> (#poke struct f_owner_ex, pid) p pid

#endif

-----------------------------------------------------------
-- File and directory change notification (dnotify)

#if defined(DN_ACCESS)

newtype DNotify = DNotify CInt

#DEFINE_BIDIRECTIONAL_PATTERN "DN_ACCESS", "DNotify"
#DEFINE_BIDIRECTIONAL_PATTERN "DN_MODIFY", "DNotify"
#DEFINE_BIDIRECTIONAL_PATTERN "DN_CREATE", "DNotify"
#DEFINE_BIDIRECTIONAL_PATTERN "DN_DELETE", "DNotify"
#DEFINE_BIDIRECTIONAL_PATTERN "DN_RENAME", "DNotify"
#DEFINE_BIDIRECTIONAL_PATTERN "DN_ATTRIB", "DNotify"

#endif

-----------------------------------------------------------
-- File sealing

#if defined(F_GET_SEALS)

newtype FileSeal = FileSeal CInt

#DEFINE_BIDIRECTIONAL_PATTERN "F_SEAL_SEAL", "FileSeal"
#DEFINE_BIDIRECTIONAL_PATTERN "F_SEAL_SHRINK", "FileSeal"
#DEFINE_BIDIRECTIONAL_PATTERN "F_SEAL_GROW", "FileSeal"
#DEFINE_BIDIRECTIONAL_PATTERN "F_SEAL_WRITE", "FileSeal"

#endif
