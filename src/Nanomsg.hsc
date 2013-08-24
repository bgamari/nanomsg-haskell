{-# LANGUAGE ForeignFunctionInterface #-}
module Nanomsg where

#include "nanomsg/nn.h"
#include "nanomsg/pair.h"
#include "nanomsg/reqrep.h"
#include "nanomsg/pubsub.h"
#include "nanomsg/survey.h"
#include "nanomsg/pipeline.h"
#include "nanomsg/bus.h"

import qualified Data.ByteString as B
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as U

--import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Applicative ( (<$>) )
import Control.Monad (forever)


newtype SocketOption = SocketOption { unSocketOption :: CInt }
    deriving (Eq, Ord, Show)

#{enum SocketOption, SocketOption
    , linger            = NN_LINGER
    , sndBuf            = NN_SNDBUF
    , rcvBuf            = NN_RCVBUF
    , sndTimeou         = NN_SNDTIMEO
    , rcvTimeou         = NN_RCVTIMEO
    , reconnectIVL      = NN_RECONNECT_IVL
    , reconnectIVLMax   = NN_RECONNECT_IVL_MAX
    , sndPrio           = NN_SNDPRIO
    , sndFD             = NN_SNDFD
    , rcvFD             = NN_SNDFD
    , domain            = NN_DOMAIN
    , protocol          = NN_PROTOCOL
    , ipv4only          = NN_IPV4ONLY
}

-- Socket types
data Pair = Pair
    deriving (Eq, Ord, Show)
data Req = Req
    deriving (Eq, Ord, Show)
data Rep = Rep
    deriving (Eq, Ord, Show)
data Pub = Pub
    deriving (Eq, Ord, Show)
data Sub = Sub
    deriving (Eq, Ord, Show)
data Surveyor = Surveyor
    deriving (Eq, Ord, Show)
data Respondent = Respondent
    deriving (Eq, Ord, Show)
data Push = Push
    deriving (Eq, Ord, Show)
data Pull = Pull
    deriving (Eq, Ord, Show)
data Bus = Bus
    deriving (Eq, Ord, Show)

-- Endpoint identifier. Returned from connect/bind.
data Endpoint = Endpoint CInt
    deriving (Eq, Ord, Show)

-- a is the socket type
data Socket a = Socket a CInt

class SocketType a where
    socketType :: a -> CInt

instance SocketType Pair where
    socketType Pair = #const NN_PAIR

instance SocketType Req where
    socketType Req = #const NN_REQ

instance SocketType Rep where
    socketType Rep = #const NN_REP

instance SocketType Pub where
    socketType Pub = #const NN_PUB

instance SocketType Sub where
    socketType Sub = #const NN_SUB

instance SocketType Surveyor where
    socketType Surveyor = #const NN_SURVEYOR

instance SocketType Respondent where
    socketType Respondent = #const NN_RESPONDENT

instance SocketType Push where
    socketType Push = #const NN_PUSH

instance SocketType Pull where
    socketType Pull = #const NN_PULL

instance SocketType Bus where
    socketType Bus = #const NN_BUS


class Sender a

instance Sender Pair
instance Sender Req
instance Sender Rep
instance Sender Pub
instance Sender Surveyor
instance Sender Respondent
instance Sender Push
instance Sender Bus

class Receiver a

instance Receiver Pair
instance Receiver Rep
instance Receiver Sub
instance Receiver Respondent
instance Receiver Surveyor
instance Receiver Pull
instance Receiver Bus



-- FFI functions

-- NN_EXPORT int nn_socket (int domain, int protocol);
foreign import ccall unsafe "nn.h nn_socket"
    c_nn_socket :: CInt -> CInt -> IO CInt

-- NN_EXPORT int nn_bind (int s, const char *addr);
foreign import ccall unsafe "nn.h nn_bind"
    c_nn_bind :: CInt -> CString -> IO CInt

-- NN_EXPORT int nn_connect (int s, const char *addr);
foreign import ccall unsafe "nn.h nn_connect"
    c_nn_connect :: CInt -> CString -> IO CInt

-- NN_EXPORT int nn_shutdown (int s, int how);
foreign import ccall unsafe "nn.h nn_shutdown"
    c_nn_shutdown :: CInt -> CInt -> IO CInt

-- NN_EXPORT int nn_send (int s, const void *buf, size_t len, int flags);
foreign import ccall unsafe "nn.h nn_send"
    c_nn_send :: CInt -> Ptr CChar -> CInt -> CInt -> IO CInt

-- NN_EXPORT int nn_recv (int s, void *buf, size_t len, int flags);
foreign import ccall unsafe "nn.h nn_recv"
    c_nn_recv :: CInt -> Ptr () -> CInt -> CInt -> IO CInt

-- NN_EXPORT int nn_freemsg (void *msg);
foreign import ccall unsafe "nn.h nn_freemsg"
    c_nn_freemsg :: Ptr () -> IO CInt

-- NN_EXPORT int nn_close (int s);
foreign import ccall unsafe "nn.h nn_close"
    c_nn_close :: CInt -> IO CInt

-- NN_EXPORT void nn_term (void);
foreign import ccall unsafe "nn.h nn_term"
    c_nn_term :: IO ()

-- NN_EXPORT int nn_setsockopt (int s, int level, int option, const void *optval, size_t optvallen);
foreign import ccall unsafe "nn.h nn_setsockopt"
    c_nn_setsockopt :: CInt -> CInt -> CInt -> Ptr () -> CInt -> IO CInt

-- NN_EXPORT int nn_getsockopt (int s, int level, int option, void *optval, size_t *optvallen);
foreign import ccall unsafe "nn.h nn_getsockopt"
    c_nn_getsockopt :: CInt -> CInt -> CInt -> Ptr () -> CInt -> IO CInt

{-

Unbound FFI functions:

NN_EXPORT int nn_sendmsg (int s, const struct nn_msghdr *msghdr, int flags);
NN_EXPORT int nn_recvmsg (int s, struct nn_msghdr *msghdr, int flags);

/*  This function retrieves the errno as it is known to the library.          */
/*  The goal of this function is to make the code 100% portable, including    */
/*  where the library is compiled with certain CRT library (on Windows) and   */
/*  linked to an application that uses different CRT library.                 */
NN_EXPORT int nn_errno (void);

/*  Resolves system errors and native errors to human-readable string.        */
NN_EXPORT const char *nn_strerror (int errnum);

NN_EXPORT void *nn_allocmsg (size_t size, int type);
-}


-- Exported functions
mkSocket :: (SocketType a) => a -> IO (Socket a)
mkSocket a = do
    sid <- c_nn_socket (#const AF_SP) (socketType a)
    return $ Socket a sid

bind :: Socket t -> String -> IO Endpoint
bind (Socket _ sid) addr = withCString addr $ \adr -> Endpoint <$> c_nn_bind sid adr

connect :: Socket t -> String -> IO Endpoint
connect (Socket _ sid) addr = withCString addr $ \adr -> Endpoint <$> c_nn_connect sid adr

shutdown :: Socket t -> Endpoint -> IO ()
shutdown (Socket _ sid) (Endpoint eid) = do
    _ <- c_nn_shutdown sid eid
    return ()

send :: Sender t => Socket t -> B.ByteString -> IO ()
send (Socket _ sid) string = do
    _ <- U.unsafeUseAsCStringLen string (\(ptr, len) -> c_nn_send sid ptr (fromIntegral len) 0)
    return ()

recv :: Socket t -> IO B.ByteString
recv = undefined

recv' :: Socket t -> IO (Maybe B.ByteString)
recv' = undefined

close :: Socket t -> IO ()
close (Socket _ sid) = do
    _ <- c_nn_close sid
    return ()

term :: IO ()
term = c_nn_term

main :: IO ()
main = do
    a <- mkSocket Pub
    _ <- bind a "tcp://*:8899"
    forever $
        send a $ C.pack "100"

