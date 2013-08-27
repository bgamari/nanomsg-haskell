{-# LANGUAGE ForeignFunctionInterface #-}
-- |
-- Module:          Nanomsg
-- Copyright:       (c) 2013 Ivar Nymoen
-- License:         MIT
-- Stability:       experimental
--
-- This is a Haskell binding for the nanomsg messaging
-- library: <http://nanomsg.org/>.
--
-- The goal is to come up with a simple and robust interface. Low level
-- features like raw sockets and some non-essentials (e.g. devices)
-- will not be supported.
--
-- Socket type documentation is adapted or quoted verbatim from the
-- nanomsg manual. Please refer to nanomsg.org for information on
-- how to use the library.
module Nanomsg
        (
        -- * Socket types
          Pair(..)
        , Req(..)
        , Rep(..)
        , Pub(..)
        , Sub(..)
        , Surveyor(..)
        , Respondent(..)
        , Push(..)
        , Pull(..)
        , Bus(..)
        -- * Other types
        , Socket
        , Endpoint
        -- * Functions
        , socket
        , bind
        , connect
        , send
        , recv
        , recv'
        , subscribe
        , unsubscribe
        , shutdown
        , close
        , term
    ) where

#include "nanomsg/nn.h"
#include "nanomsg/pair.h"
#include "nanomsg/reqrep.h"
#include "nanomsg/pubsub.h"
#include "nanomsg/survey.h"
#include "nanomsg/pipeline.h"
#include "nanomsg/bus.h"

import Data.ByteString (ByteString)
--import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as U
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Control.Applicative ( (<$>) )

-- | Socket for communication with exactly one peer. Each
-- party can send messages at any time. If the peer is not
-- available or send buffer is full, subsequent calls to
-- 'send' will block until it’s possible to send the message.
data Pair = Pair

-- | Used to implement a client application that sends requests
-- and receives replies. The socket will resend requests automatically
-- if there's no reply within a given time. The default timeout
-- is 1 minute.
--
-- See also 'setRequestResendInterval'.
data Req = Req

-- | Used to implement a stateless worker that receives requests
-- and sends replies.
data Rep = Rep

-- | This socket is used to distribute messages to multiple destinations.
-- Can not receive.
data Pub = Pub

-- | Receives messages from the publisher. Only messages that the socket is
-- subscribed to are received. When the socket is created there are no
-- subscriptions and thus no messages will be received.
--
-- Send is not defined on this socket. The socket can be connected
-- to at most one peer.
--
-- See also 'subscribe' and 'unsubscribe'.
data Sub = Sub

-- | Surveyor and respondent are used to broadcast a survey to multiple
-- locations and gather the responses.
--
-- This socket is used to send the survey. The survey is delivered to all
-- the connected respondents. Once the query is sent, the socket can be used
-- to receive the responses.
--
-- When the survey deadline expires, receive will return ETIMEDOUT error.
--
-- See also 'setSurveyorDeadline'
data Surveyor = Surveyor

-- | Used to respond to a survey. Survey is received using receive function,
-- response is sent using send function. This socket can be connected to
-- at most one peer.
data Respondent = Respondent

-- | Push and Pull sockets fair queue messages from one processing step, load
-- balancing them among instances of the next processing step.
--
-- This socket is used to send messages to a cluster of load-balanced nodes.
--
-- Receive operation is not implemented on this socket type.
data Push = Push

-- | This socket is used to receive a message from a cluster of nodes.
--
-- Send operation is not implemented on this socket type.
data Pull = Pull

-- | Broadcasts messages from any node to all other nodes in the topology.
-- The socket should never receives messages that it sent itself.
--
-- This pattern scales only to local level (within a single machine or
-- within a single LAN). Trying to scale it further can result in overloading
-- individual nodes with messages.
data Bus = Bus

-- | Endpoint identifier. Created by 'connect' or 'bind'.
--
-- Close connections using 'shutdown'.
data Endpoint = Endpoint CInt
    deriving (Eq)

-- | Sockets are created by 'socket' and connections are established with 'connect' or 'bind'.
--
-- Free sockets using 'close'.
data Socket t = Socket t CInt
    deriving (Eq)

-- | Typeclass used by all sockets, to extract their C type.
class SocketType t where
    -- | Returns the C enum value for each type. E.g. Pair => #const NN_PAIR
    socketType :: t -> CInt

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


-- | Typeclass restricting which sockets can use the send function.
class SendType t
instance SendType Pair
instance SendType Req
instance SendType Rep
instance SendType Pub
instance SendType Surveyor
instance SendType Respondent
instance SendType Push
instance SendType Bus

-- | Typeclass for sockets that implement recv
class RecvType t
instance RecvType Pair
instance RecvType Req
instance RecvType Rep
instance RecvType Sub
instance RecvType Surveyor
instance RecvType Respondent
instance RecvType Pull
instance RecvType Bus

-- | Sub socket functionality
class SubscriberType t
instance SubscriberType Sub

-- | Surveyor socket functionality
class SurvType t
instance SurvType Surveyor

-- | Req socket functionality
class ReqType t
instance ReqType Req


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
--foreign import ccall unsafe "nn.h nn_recv"
--    c_nn_recv :: CInt -> Ptr CChar -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "nn.h nn_recv"
    c_nn_recv_foreignbuf :: CInt -> Ptr (Ptr CChar) -> CInt -> CInt -> IO CInt

-- NN_EXPORT int nn_freemsg (void *msg);
foreign import ccall unsafe "nn.h nn_freemsg"
    c_nn_freemsg :: Ptr CChar -> IO CInt

-- NN_EXPORT int nn_close (int s);
foreign import ccall unsafe "nn.h nn_close"
    c_nn_close :: CInt -> IO CInt

-- NN_EXPORT void nn_term (void);
foreign import ccall unsafe "nn.h nn_term"
    c_nn_term :: IO ()

-- NN_EXPORT int nn_setsockopt (int s, int level, int option, const void *optval, size_t optvallen);
foreign import ccall unsafe "nn.h nn_setsockopt"
    c_nn_setsockopt :: CInt -> CInt -> CInt -> Ptr CChar -> CInt -> IO CInt

-- NN_EXPORT int nn_getsockopt (int s, int level, int option, void *optval, size_t *optvallen);
foreign import ccall unsafe "nn.h nn_getsockopt"
    c_nn_getsockopt :: CInt -> CInt -> CInt -> Ptr CChar -> CInt -> IO CInt

{-

Unbound FFI functions:

NN_EXPORT int nn_sendmsg (int s, const struct nn_msghdr *msghdr, int flags);
NN_EXPORT int nn_recvmsg (int s, struct nn_msghdr *msghdr, int flags);

/*  This function retrieves the errno as it is known to the library.          */
/*  The goal of this function is to make the code 100% portable, including    */
/*  where the library is compiled with certain CRT library (on Windows) and   */
/*  linked to an application that uses different CRT library.                 */
NN_EXPORT int nn_errno (void);
foreign import ccall unsafe "nn.h nn_errno"
    c_nn_errno :: IO CInt

/*  Resolves system errors and native errors to human-readable string.        */
NN_EXPORT const char *nn_strerror (int errnum);
foreign import ccall unsafe "nn.h nn_strerror"
    c_nn_strerror :: CInt -> IO CString

NN_EXPORT void *nn_allocmsg (size_t size, int type);
-}

-- | Creates a socket. Connections are formed using 'bind' or 'connect'.
--
-- See also: 'close'.
socket :: (SocketType t) => t -> IO (Socket t)
socket t = do
    sid <- c_nn_socket (#const AF_SP) (socketType t)
    return $ Socket t sid

-- | Binds the socket to a local interface.
--
-- See the nanomsg documentation for specifics on transports.
-- Note that host names do not work for tcp. Some examples are:
--
-- > bind sock "tcp://*:5560"
-- > bind sock "tcp://eth0:5560"
-- > bind sock "tcp://127.0.0.1:5560"
-- > bind sock "inproc://test"
-- > bind sock "ipc:///tmp/test.ipc"
--
-- This function returns an 'Endpoint', which can be supplied
-- to 'shutdown' to remove a connection.
--
-- See also: 'connect', 'shutdown'.
bind :: Socket t -> String -> IO Endpoint
bind (Socket _ sid) addr = withCString addr $ \adr -> Endpoint <$> c_nn_bind sid adr

-- | Connects the socket to an endpoint.
--
-- e.g. :
--
-- > connect sock "tcp://localhost:5560"
-- > connect sock "inproc://test"
--
-- See also: 'bind', 'shutdown'.
connect :: Socket t -> String -> IO Endpoint
connect (Socket _ sid) addr = withCString addr $ \adr -> Endpoint <$> c_nn_connect sid adr

-- | Removes an endpoint from a socket.
--
-- See also: 'bind', 'connect'.
shutdown :: Socket t -> Endpoint -> IO ()
shutdown (Socket _ sid) (Endpoint eid) = do
    _ <- c_nn_shutdown sid eid
    return ()

-- | Blocking function for sending a message
--
-- See also: 'recv', 'recv''.
send :: (SendType t, SocketType t) => Socket t -> ByteString -> IO ()
send (Socket _ sid) string = do
    _ <- U.unsafeUseAsCStringLen string (\(ptr, len) -> c_nn_send sid ptr (fromIntegral len) 0)
    return ()

-- | Blocking receive.
recv :: (RecvType t, SocketType t) => Socket t -> IO ByteString
recv (Socket _ sid) =
    alloca $ \ptr -> do
        len <- c_nn_recv_foreignbuf sid ptr (#const NN_MSG) 0
        buf <- peek ptr
        str <- C.packCStringLen (buf, fromIntegral len)
        _ <- c_nn_freemsg buf
        return str

-- | Nonblocking receive function.
recv' :: (RecvType t, SocketType t) => Socket t -> IO (Maybe ByteString)
recv' (Socket _ sid) = do
    alloca $ \ptr -> do
        len <- c_nn_recv_foreignbuf sid ptr (#const NN_MSG) (#const NN_DONTWAIT)
        if len >= 0
            then do
                buf <- peek ptr
                str <- C.packCStringLen (buf, fromIntegral len)
                _ <- c_nn_freemsg buf
                return $ Just str
            else return Nothing

-- | Subscribe to a given subject string.
subscribe :: (SubscriberType t, SocketType t) => Socket t -> ByteString -> IO ()
subscribe (Socket t sid) string = do
    _ <- U.unsafeUseAsCStringLen string $
        \(ptr, len) -> c_nn_setsockopt sid (socketType t) (#const NN_SUB_SUBSCRIBE) ptr (fromIntegral len)
    return ()

-- | Unsubscribes from a subject.
unsubscribe :: (SubscriberType t, SocketType t) => Socket t -> ByteString -> IO ()
unsubscribe (Socket t sid) string = do
    _ <- U.unsafeUseAsCStringLen string $
        \(ptr, len) -> c_nn_setsockopt sid (socketType t) (#const NN_SUB_UNSUBSCRIBE) ptr (fromIntegral len)
    return ()

-- | Closes the socket. Any buffered inbound messages that were not yet
-- received by the application will be discarded. The library will try to
-- deliver any outstanding outbound messages for the time specified by
-- NN_LINGER socket option. The call will block in the meantime.
close :: Socket t -> IO ()
close (Socket _ sid) = do
    _ <- c_nn_close sid
    return ()

-- | To help with shutdown of multi-threaded programs nanomsg provides
-- the term function which informs all the open sockets that process
-- termination is underway.
--
-- If a socket is blocked inside a blocking function, such as recv,
-- it will be unblocked and ETERM error will be returned to the user.
--
-- Similarly, any subsequent attempt to invoke a socket function other
-- than close after term was called will result in ETERM error.
term :: IO ()
term = c_nn_term

