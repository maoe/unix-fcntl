{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
module System.Posix.FileControl
  ( fcntl
  , Fcntl(..)

  , FileDescriptorFlags(..)
  , FileStatusFlags(..)
  , newFlock
  , Flock
  , flockType
  , FlockType(..)
  , flockWhence
  , FlockWhence(..)
  , flockStart
  , flockLen
  , flockPid

#if defined(_GNU_SOURCE)
  , FOwnerEx
  , newFOwnerEx
  , fOwnerExType
  , fOwnerExPid
  , OwnerType(..)

  , DNotify
  , pattern DN_ACCESS
  , pattern DN_MODIFY
  , pattern DN_CREATE
  , pattern DN_DELETE
  , pattern DN_RENAME
  , pattern DN_ATTRIB
#endif -- defined(_GNU_SOURCE)

#if defined(F_GET_SEALS)
  , FileSeal
  , pattern F_SEAL_SEAL
  , pattern F_SEAL_SHRINK
  , pattern F_SEAL_GROW
  , pattern F_SEAL_WRITE
#endif -- defined(F_GET_SEALS)
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

{# pointer *flock as ^ foreign newtype #}

#if defined(_GNU_SOURCE)
{# pointer *f_owner_ex as ^ foreign newtype #}
#endif -- defined(_GNU_SOURCE)

-- | Perform one of the operations in 'Fcntl'
fcntl :: Fd -> Fcntl a -> IO a
fcntl fd cmd = case cmd of
  -- Duplicating a file descriptor
  F_DUPFD minFd ->
    fcntl_set_int fd {# const F_DUPFD #} minFd
  F_DUPFD_CLOEXEC minFd ->
    fcntl_set_int fd {# const F_DUPFD_CLOEXEC #} minFd

  -- File descriptor flags
  F_GETFD ->
    toEnum <$> fcntl_get_int fd {# const F_GETFD #}
  F_SETFD flags ->
    fcntl_set_int_ fd {# const F_SETFD #} (fromEnum flags)

  -- File status flags
  F_GETFL ->
    toEnum <$> fcntl_get_int fd {# const F_GETFL #}
  F_SETFL flags ->
    fcntl_set_int_ fd {# const F_SETFL #} (fromEnum flags)

  -- Advisory or mandatory locking
  F_GETLK ->
    fcntl_get_flock fd {# const F_GETLK #}
  F_SETLK flock ->
    fcntl_set_flock fd {# const F_SETLK #} flock
  F_SETLKW flock ->
    fcntl_set_flock fd {# const F_SETLKW #} flock

#if defined(_GNU_SOURCE) && defined(F_OFD_GETLK)
  -- Open description locks
  F_OFD_GETLK ->
    fcntl_get_flock fd {# const F_OFD_GETLK #}
  F_OFD_SETLK flock ->
    fcntl_set_flock fd {# const F_OFD_SETLK #} flock
  F_OFD_SETLKW flock ->
    fcntl_set_flock fd {# const F_OFD_SETLKW #} flock
#endif

  -- Managing signals
  F_GETOWN ->
    fcntl_get_int fd {# const F_GETOWN #}
  F_SETOWN pid ->
    fcntl_set_int_ fd {# const F_SETFL #} pid
#if defined(_GNU_SOURCE)
  F_GETOWN_EX ->
    fcntl_get_f_owner_ex fd {# const F_GETOWN_EX #}
  F_SETOWN_EX foe ->
    fcntl_set_f_owner_ex fd {# const F_SETOWN_EX #} foe
  F_GETSIG ->
    fcntl_get_int fd {# const F_GETSIG #}
  F_SETSIG sig ->
    fcntl_set_int_ fd {# const F_SETSIG #} sig

  -- Leases (Linux 2.4)
  F_GETLEASE ->
    toEnum <$> fcntl_get_int fd {# const F_GETLEASE #}
  F_SETLEASE ty ->
    fcntl_set_int_ fd {# const F_SETLEASE #} (fromEnum ty)

  -- File and directory change notification (dnotify; Linux 2.4)
  F_NOTIFY (DNotify n) ->
    fcntl_set_int_ fd {# const F_NOTIFY #} n

  -- Changing the capacity of a pipe
  F_GETPIPE_SZ ->
    fcntl_get_int fd {# const F_GETPIPE_SZ #}
  F_SETPIPE_SZ size ->
    fcntl_set_int_ fd {# const F_SETPIPE_SZ #} size
#endif -- defined(_GNU_SOURCE)

#if defined(F_GET_SEALS)
  -- File sealing
  F_GET_SEALS ->
    fcntl_get_int fd {# const F_GET_SEALS #}
  F_ADD_SEALS ->
    fcntl_set_int_ fd {# const F_ADD_SEALS #}
#endif -- defined(F_GET_SEALS)

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
#endif -- defined(_GNU_SOURCE)

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
#endif -- defined(F_NOTIFY)

  -- Changing the capacity of a pipe
  F_GETPIPE_SZ :: Fcntl Int
  F_SETPIPE_SZ :: Int -> Fcntl ()
#endif -- defined(_GNU_SOURCE)

#if defined(F_GET_SEALS)
  -- File sealing (Linux 3.17)
  F_GET_SEALS :: Fcntl Seal
  F_ADD_SEALS :: Seal -> Fcntl ()
#endif -- defined(F_GET_SEALS)

fcntl_get_int :: Integral a => Fd -> CInt -> IO a
fcntl_get_int fd cmd =
  fromIntegral <$> throwErrnoIfMinus1 "fcntl" (c_fcntl_get_int fd cmd)

{# fun fcntl as c_fcntl_get_int
  { fromIntegral `Fd'
  , `CInt'
  } -> `CInt'
  #}

fcntl_set_int :: (Integral a, Integral b) => Fd -> CInt -> a -> IO b
fcntl_set_int fd cmd n =
  fromIntegral <$> throwErrnoIfMinus1 "fcntl"
    (c_fcntl_set_int fd cmd (fromIntegral n))

fcntl_set_int_ :: Integral a => Fd -> CInt -> a -> IO ()
fcntl_set_int_ fd cmd n =
  throwErrnoIfMinus1_ "fcntl" (c_fcntl_set_int fd cmd (fromIntegral n))

{# fun variadic fcntl[int] as c_fcntl_set_int
  { fromIntegral `Fd'
  , `CInt'
  , `CInt'
  } -> `CInt'
  #}

fcntl_get_flock :: Fd -> CInt -> IO Flock
fcntl_get_flock fd cmd = do
  flock <- newFlock
  throwErrnoIfMinus1_ "fcntl" $ c_fcntl_get_flock fd cmd flock
  return flock

{# fun variadic fcntl[struct flock *] as c_fcntl_get_flock
  { fromIntegral `Fd'
  , `CInt'
  , `Flock'
  } -> `CInt'
  #}

fcntl_set_flock :: Fd -> CInt -> Flock -> IO ()
fcntl_set_flock fd cmd =
  throwErrnoIfMinus1_ "fcntl" . c_fcntl_set_flock fd cmd

{# fun variadic fcntl[struct flock *] as c_fcntl_set_flock
  { fromIntegral `Fd'
  , `CInt'
  , `Flock'
  } -> `CInt'
  #}

#if defined(_GNU_SOURCE)
fcntl_get_f_owner_ex :: Fd -> CInt -> IO FOwnerEx
fcntl_get_f_owner_ex fd cmd = do
  owner <- newFOwnerEx
  throwErrnoIfMinus1_ "fcntl" $ c_fcntl_get_f_owner_ex fd cmd owner
  return owner

{# fun variadic fcntl[struct f_owner_ex *] as c_fcntl_get_f_owner_ex
  { fromIntegral `Fd'
  , `CInt'
  , `FOwnerEx'
  } -> `CInt'
  #}

fcntl_set_f_owner_ex :: Fd -> CInt -> FOwnerEx -> IO ()
fcntl_set_f_owner_ex fd cmd =
  throwErrnoIfMinus1_ "fcntl" . c_fcntl_set_f_owner_ex fd cmd

{# fun variadic fcntl[struct f_owner_ex *] as c_fcntl_set_f_owner_ex
  { fromIntegral `Fd'
  , `CInt'
  , `FOwnerEx'
  } -> `CInt'
  #}
#endif -- defined(_GNU_SOURCE)

-----------------------------------------------------------
-- File descriptor flags

{# enum define FileDescriptorFlags
  { FD_CLOEXEC as FD_CLOEXEC
  } deriving (Show, Eq)
  #}

-----------------------------------------------------------
-- File status flags

{# enum define FileStatusFlags
  -- File access modes
  { O_RDONLY as O_RDONLY
  , O_WRONLY as O_WRONLY
  , O_RDWR as O_RDWR
  , O_ACCMODE as O_ACCMODE
  -- Open-time flags
  , O_CREAT as O_CREAT
  , O_EXCL as O_EXCL
  , O_NONBLOCK as O_NONBLOCK
  , O_NOCTTY as O_NOCTTY
  , O_TRUNC as O_TRUNC
  -- I/O operating modes
  , O_APPEND as O_APPEND
  , O_NDELAY as O_NDELAY
  } deriving (Show, Eq)
  #}

-----------------------------------------------------------
-- Advisory and mandatory locking

-- | Allocate a flock structure. The allocated memory will be garbage collected
-- automatically.
newFlock :: IO Flock
newFlock = Flock <$> mallocForeignPtrBytes {# sizeof flock #}

{# enum define FlockType
  { F_RDLCK as F_RDLCK
  , F_WRLCK as F_WRLCK
  , F_UNLCK as F_UNLCK
  } deriving (Show, Eq)
  #}

flockType :: Flock -> Var FlockType
flockType flock = Var get set
  where
    get = toEnum . fromIntegral <$> withFlock flock {# get flock.l_type #}
    set ty = withFlock flock $ \p ->
      {# set flock.l_type #} p (fromIntegral $ fromEnum ty)

{# enum define FlockWhence
  { SEEK_SET as SEEK_SET
  , SEEK_CUR as SEEK_CUR
  , SEEK_END as SEEK_END
  } deriving (Show, Eq)
  #}

flockWhence :: Flock -> Var FlockWhence
flockWhence flock = Var get set
  where
    get = toEnum . fromIntegral <$> withFlock flock {# get flock.l_whence #}
    set ty = withFlock flock $ \p ->
      {# set flock.l_whence #} p (fromIntegral $ fromEnum ty)

flockStart :: Flock -> Var FileOffset
flockStart flock = Var get set
  where
    get = fromIntegral <$> withFlock flock {# get flock.l_start #}
    set ty = withFlock flock $ \p ->
      {# set flock.l_start #} p (fromIntegral ty)

flockLen :: Flock -> Var FileOffset
flockLen flock = Var get set
  where
    get = fromIntegral <$> withFlock flock {# get flock.l_len #}
    set ty = withFlock flock $ \p ->
      {# set flock.l_len #} p (fromIntegral ty)

flockPid :: Flock -> Var ProcessID
flockPid flock = Var get set
  where
    get = fromIntegral <$> withFlock flock {# get flock.l_pid #}
    set ty = withFlock flock $ \p ->
      {# set flock.l_pid #} p (fromIntegral ty)

-----------------------------------------------------------
-- Managing signals

#if defined(_GNU_SOURCE)

newFOwnerEx :: IO FOwnerEx
newFOwnerEx = FOwnerEx <$> mallocForeignPtrBytes {# sizeof f_owner_ex #}

{# enum define OwnerType
  { F_OWNER_TID as F_OWNER_TID
  , F_OWNER_PID as F_OWNER_PID
  , F_OWNER_PGRP as F_OWNER_PGRP
  } deriving (Eq, Show)
  #}

fOwnerExType :: FOwnerEx -> Var OwnerType
fOwnerExType foe = Var get set
  where
    get = toEnum . fromIntegral <$> withFOwnerEx foe {# get f_owner_ex.type #}
    set ty = withFOwnerEx foe $ \p ->
      {# set f_owner_ex.type #} p (fromIntegral $ fromEnum ty)

fOwnerExPid :: FOwnerEx -> Var ProcessID
fOwnerExPid foe = Var get set
  where
    get = fromIntegral <$> withFOwnerEx foe {# get f_owner_ex.pid #}
    set ty = withFOwnerEx foe $ \p ->
      {# set f_owner_ex.pid #} p (fromIntegral ty)

#endif -- defined(_GNU_SOURCE)
-----------------------------------------------------------
-- File and directory change notification (dnotify)

#if defined(DN_ACCESS)

newtype DNotify = DNotify CInt deriving Eq

pattern DN_ACCESS :: DNotify
pattern DN_ACCESS <- ((\(DNotify n) -> n .&. _DN_ACCESS > 0) -> True)
  where
    DN_ACCESS = DNotify _DN_ACCESS

pattern DN_MODIFY :: DNotify
pattern DN_MODIFY <- ((\(DNotify n) -> n .&. _DN_MODIFY > 0) -> True)
  where
    DN_MODIFY = DNotify _DN_MODIFY

pattern DN_CREATE :: DNotify
pattern DN_CREATE <- ((\(DNotify n) -> n .&. _DN_CREATE > 0) -> True)
  where
    DN_CREATE = DNotify _DN_CREATE

pattern DN_DELETE :: DNotify
pattern DN_DELETE <- ((\(DNotify n) -> n .&. _DN_DELETE > 0) -> True)
  where
    DN_DELETE = DNotify _DN_DELETE

pattern DN_RENAME :: DNotify
pattern DN_RENAME <- ((\(DNotify n) -> n .&. _DN_RENAME > 0) -> True)
  where
    DN_RENAME = DNotify _DN_RENAME

pattern DN_ATTRIB :: DNotify
pattern DN_ATTRIB <- ((\(DNotify n) -> n .&. _DN_ATTRIB > 0) -> True)
  where
    DN_ATTRIB = DNotify _DN_ATTRIB

_DN_ACCESS :: CInt
_DN_ACCESS = {# const DN_ACCESS #}

_DN_MODIFY :: CInt
_DN_MODIFY = {# const DN_MODIFY #}

_DN_CREATE :: CInt
_DN_CREATE = {# const DN_CREATE #}

_DN_DELETE :: CInt
_DN_DELETE = {# const DN_DELETE #}

_DN_RENAME :: CInt
_DN_RENAME = {# const DN_RENAME #}

_DN_ATTRIB :: CInt
_DN_ATTRIB = {# const DN_ATTRIB #}

#endif -- defined(DN_ACCESS)

-----------------------------------------------------------
-- File sealing

#if defined(F_GET_SEALS)

newtype FileSeal = FileSeal CInt deriving Eq

pattern F_SEAL_SEAL :: FileSeal
pattern F_SEAL_SEAL <- ((\(FileSeal n) -> n .&. _F_SEAL_SEAL > 0) -> True)
  where
    F_SEAL_SEAL = FileSeal _F_SEAL_SEAL

pattern F_SEAL_SHRINK :: FileSeal
pattern F_SEAL_SHRINK <- ((\(FileSeal n) -> n .&. _F_SEAL_SHRINK > 0) -> True)
  where
    F_SEAL_SHRINK = FileSeal _F_SEAL_SHRINK

pattern F_SEAL_GROW :: FileSeal
pattern F_SEAL_GROW <- ((\(FileSeal n) -> n .&. _F_SEAL_GROW > 0) -> True)
  where
    F_SEAL_GROW = FileSeal _F_SEAL_GROW

pattern F_SEAL_WRITE :: FileSeal
pattern F_SEAL_WRITE <- ((\(FileSeal n) -> n .&. _F_SEAL_WRITE > 0) -> True)
  where
    F_SEAL_WRITE = FileSeal _F_SEAL_WRITE

_F_SEAL_SEAL :: CInt
_F_SEAL_SEAL = {# const F_SEAL_SEAL #}

_F_SEAL_SHRINK :: CInt
_F_SEAL_SHRINK = {# const F_SEAL_SHRINK #}

_F_SEAL_GROW :: CInt
_F_SEAL_GROW = {# const F_SEAL_GROW #}

_F_SEAL_WRITE :: CInt
_F_SEAL_WRITE = {# const F_SEAL_WRITE #}

#endif -- defined(F_GET_SEALS)
