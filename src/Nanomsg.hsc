{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}
-- |
-- Module:          Nanomsg
-- Copyright:       (c) 2013 Ivar Nymoen
-- License:         MIT
-- Stability:       experimental
--
-- This is a Haskell binding for the nanomsg library: <http://nanomsg.org/>.
--
-- There's support for (evented) blocking send and recv, a non-blocking receive,
-- and for all the socket types and the functions you need to wire
-- them up and tear them down again.
--
-- Most socket options are available through accessor and mutator
-- functions. Sockets are typed, transports are not.
--
-- The documentation is adapted or quoted verbatim from the nanomsg manual,
-- please refer to nanomsg.org for authoritative info.
-- There's a simple code example in <https://github.com/ivarnymoen/nanomsg-haskell#usage README.md>.
module Nanomsg
        (
        -- * Types
        -- ** Socket types
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
        -- ** Other
        , Socket
        , Endpoint
        , NNException
        , SocketType
        , Sender
        , Receiver
        -- * Operations
        -- ** General operations
        , socket
        , withSocket
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
        -- ** Socket option settings
        , linger
        , setLinger
        , sndBuf
        , setSndBuf
        , rcvBuf
        , setRcvBuf
        , rcvMaxSize
        , setRcvMaxSize
        , reconnectInterval
        , setReconnectInterval
        , reconnectIntervalMax
        , setReconnectIntervalMax
        , sndPrio
        , setSndPrio
        , ipv4Only
        , setIpv4Only
        , requestResendInterval
        , setRequestResendInterval
        , surveyorDeadline
        , setSurveyorDeadline
        , tcpNoDelay
        , setTcpNoDelay
    ) where

#include "nanomsg/nn.h"
#include "nanomsg/pair.h"
#include "nanomsg/reqrep.h"
#include "nanomsg/pubsub.h"
#include "nanomsg/survey.h"
#include "nanomsg/pipeline.h"
#include "nanomsg/bus.h"
#include "nanomsg/tcp.h"

import Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as U
import Foreign (peek, poke, alloca)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable (sizeOf)
import Control.Applicative ( (<$>) )
import Control.Exception.Base (bracket)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Control.Monad (void)
import Text.Printf (printf)
import Control.Concurrent (threadWaitRead, threadWaitWrite)
import System.Posix.Types (Fd(..))


-- * Data and typedefs

-- | Socket for communication with exactly one peer. Each
-- party can send messages at any time. If the peer is not
-- available or the send buffer is full, subsequent calls
-- will block until it’s possible to send the message.
data Pair = Pair

-- | Request socket. Pairs with 'Rep' sockets.
--
-- The socket will resend requests automatically
-- if there's no reply within a given time. The default timeout
-- is 1 minute.
--
-- See also 'Rep', 'setRequestResendInterval'.
data Req = Req

-- | Reply socket.
--
-- See also 'Req'.
data Rep = Rep

-- | Publish socket. Pairs with subscribe sockets.
--
-- See also 'Sub'.
data Pub = Pub

-- | Subscribe socket.
--
-- Only messages that the socket is subscribed to are received. When the socket
-- is created there are no subscriptions and thus no messages will be received.
--
-- See also 'Pub', 'subscribe' and 'unsubscribe'.
data Sub = Sub

-- | Surveyor and respondent are used to broadcast a survey to multiple
-- locations and gather the responses.
--
-- This socket is used to send a survey. The survey is delivered to all
-- onnected respondents. Once the query is sent, the socket can be used
-- to receive the responses.
--
-- When the survey deadline expires, receive will throw an NNException.
--
-- See also 'Respondent', 'setSurveyorDeadline'.
data Surveyor = Surveyor

-- | Used to respond to a survey. Survey is received using receive,
-- response is sent using send. This socket can be connected to
-- at most one peer.
--
-- See also 'Surveyor'.
data Respondent = Respondent

-- | Push and Pull sockets fair queue messages from one processing step, load
-- balancing them among instances of the next processing step.
--
-- See also 'Pull'.
data Push = Push

-- | Pull socket.
--
-- See also 'Push'.
data Pull = Pull

-- | Broadcasts messages from any node to all other nodes in the topology.
-- The socket should never receives messages that it sent itself.
data Bus = Bus

-- | Endpoint identifier. Created by 'connect' or 'bind'.
--
-- Close connections using 'shutdown'.
data Endpoint = Endpoint CInt
    deriving (Eq, Show)

-- | Sockets are created by 'socket' and connections are established with 'connect' or 'bind'.
--
-- Free sockets using 'close'.
data Socket a = Socket a CInt
    deriving (Eq, Show)

-- | Typeclass for all sockets
class SocketType a where
    socketType :: a -> CInt -- ^ Returns the C enum value for each type. E.g. Pair => #const NN_PAIR

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
class (SocketType a) => Sender a
instance Sender Pair
instance Sender Req
instance Sender Rep
instance Sender Pub
instance Sender Surveyor
instance Sender Respondent
instance Sender Push
instance Sender Bus

-- | Typeclass for sockets that implement recv
class (SocketType a) => Receiver a
instance Receiver Pair
instance Receiver Req
instance Receiver Rep
instance Receiver Sub
instance Receiver Surveyor
instance Receiver Respondent
instance Receiver Pull
instance Receiver Bus


-- * Error handling
--
-- Reimplementing some of Foreign.C.Error here, to substitute nanomsg's errno
-- and strerror functions for the posix ones.

-- | Pretty much any error condition throws this exception.
data NNException = NNException String
        deriving (Eq, Show, Typeable)

instance Exception NNException

mkErrorString :: String -> IO String
mkErrorString loc = do
    errNo <- c_nn_errno
    errCString <- c_nn_strerror errNo
    errString <- peekCString errCString
    return $ printf "nanomsg-haskell error at %s. Errno %d: %s" loc (fromIntegral errNo :: Int) errString

throwErrno :: String -> IO a
throwErrno loc = do
    s <- mkErrorString loc
    throwIO $ NNException s

throwErrnoIf :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIf p loc action = do
    res <- action
    if p res then throwErrno loc else return res

throwErrnoIf_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ p loc action = void $ throwErrnoIf p loc action

throwErrnoIfMinus1 :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1 = throwErrnoIf (== -1)

throwErrnoIfMinus1_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1_ = throwErrnoIf_ (== -1)

throwErrnoIfRetry :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry p loc f = do
    res <- f
    if p res
        then do
            err <- c_nn_errno
            if err == (#const EAGAIN) || err == (#const EINTR)
                then throwErrnoIfRetry p loc f
                else throwErrno loc
        else return res

throwErrnoIfRetry_ :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ p loc f = void $ throwErrnoIfRetry p loc f

{-
throwErrnoIfMinus1Retry :: (Eq a, Num a) => String -> IO a -> IO a
throwErrnoIfMinus1Retry = throwErrnoIfRetry (== -1)
-}

throwErrnoIfMinus1Retry_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_ = throwErrnoIfRetry_ (== -1)

throwErrnoIfRetryMayBlock :: (a -> Bool) -> String -> IO a -> IO b -> IO a
throwErrnoIfRetryMayBlock p loc f on_block = do
    res <- f
    if p res
        then do
            err <- c_nn_errno
            if err `elem` [ (#const EAGAIN), (#const EINTR), (#const EWOULDBLOCK) ]
                then do
                    void on_block
                    throwErrnoIfRetryMayBlock p loc f on_block
                else throwErrno loc
        else return res

throwErrnoIfRetryMayBlock_ :: (a -> Bool) -> String -> IO a -> IO b -> IO ()
throwErrnoIfRetryMayBlock_ p loc f on_block = void $ throwErrnoIfRetryMayBlock p loc f on_block

throwErrnoIfMinus1RetryMayBlock :: (Eq a, Num a) => String -> IO a -> IO b -> IO a
throwErrnoIfMinus1RetryMayBlock = throwErrnoIfRetryMayBlock (== -1)

throwErrnoIfMinus1RetryMayBlock_ :: (Eq a, Num a) => String -> IO a -> IO b -> IO ()
throwErrnoIfMinus1RetryMayBlock_ = throwErrnoIfRetryMayBlock_ (== -1)


-- * FFI functions

-- NN_EXPORT int nn_socket (int domain, int protocol);
foreign import ccall safe "nn.h nn_socket"
    c_nn_socket :: CInt -> CInt -> IO CInt

-- NN_EXPORT int nn_bind (int s, const char *addr);
foreign import ccall safe "nn.h nn_bind"
    c_nn_bind :: CInt -> CString -> IO CInt

-- NN_EXPORT int nn_connect (int s, const char *addr);
foreign import ccall safe "nn.h nn_connect"
    c_nn_connect :: CInt -> CString -> IO CInt

-- NN_EXPORT int nn_shutdown (int s, int how);
foreign import ccall safe "nn.h nn_shutdown"
    c_nn_shutdown :: CInt -> CInt -> IO CInt

-- NN_EXPORT int nn_send (int s, const void *buf, size_t len, int flags);
foreign import ccall safe "nn.h nn_send"
    c_nn_send :: CInt -> CString -> CInt -> CInt -> IO CInt

-- NN_EXPORT int nn_recv (int s, void *buf, size_t len, int flags);
foreign import ccall safe "nn.h nn_recv"
    c_nn_recv :: CInt -> Ptr CString -> CInt -> CInt -> IO CInt

-- NN_EXPORT int nn_freemsg (void *msg);
foreign import ccall safe "nn.h nn_freemsg"
    c_nn_freemsg :: Ptr CChar -> IO CInt

-- NN_EXPORT int nn_close (int s);
foreign import ccall safe "nn.h nn_close"
    c_nn_close :: CInt -> IO CInt

-- NN_EXPORT void nn_term (void);
foreign import ccall safe "nn.h nn_term"
    c_nn_term :: IO ()

-- NN_EXPORT int nn_setsockopt (int s, int level, int option, const void *optval, size_t optvallen);
foreign import ccall safe "nn.h nn_setsockopt"
    c_nn_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt

-- NN_EXPORT int nn_getsockopt (int s, int level, int option, void *optval, size_t *optvallen);
foreign import ccall safe "nn.h nn_getsockopt"
    c_nn_getsockopt :: CInt -> CInt -> CInt -> Ptr a -> Ptr CInt -> IO CInt

-- /*  Resolves system errors and native errors to human-readable string.        */
-- NN_EXPORT const char *nn_strerror (int errnum);
foreign import ccall safe "nn.h nn_strerror"
    c_nn_strerror :: CInt -> IO CString

-- /*  This function retrieves the errno as it is known to the library.          */
-- /*  The goal of this function is to make the code 100% portable, including    */
-- /*  where the library is compiled with certain CRT library (on Windows) and   */
-- /*  linked to an application that uses different CRT library.                 */
-- NN_EXPORT int nn_errno (void);
foreign import ccall safe "nn.h nn_errno"
    c_nn_errno :: IO CInt

{-

Unbound FFI functions:

NN_EXPORT int nn_sendmsg (int s, const struct nn_msghdr *msghdr, int flags);
NN_EXPORT int nn_recvmsg (int s, struct nn_msghdr *msghdr, int flags);

NN_EXPORT void *nn_allocmsg (size_t size, int type);
-}

-- * Operations

-- | Creates a socket. Connections are formed using 'bind' or 'connect'.
--
-- See also: 'close'.
socket :: (SocketType a) => a -> IO (Socket a)
socket t = do
    sid <- throwErrnoIfMinus1 "socket" $ c_nn_socket (#const AF_SP) (socketType t)
    return $ Socket t sid

-- | Creates a socket and runs your action with it.
--
-- E.g. collecting 10 messages:
--
-- > withSocket Sub $ \sub -> do
-- >     _ <- connect sub "tcp://localhost:5560"
-- >     subscribe sub (C.pack "")
-- >     replicateM 10 (recv sub)
--
-- Ensures the socket is closed when your action is done.
withSocket :: (SocketType a) => a -> (Socket a -> IO b) -> IO b
withSocket t = bracket (socket t) close

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
bind :: Socket a -> String -> IO Endpoint
bind (Socket _ sid) addr =
    withCString addr $ \adr -> do
        epid <- throwErrnoIfMinus1 "bind" $ c_nn_bind sid adr
        return $ Endpoint epid

-- | Connects the socket to an endpoint.
--
-- e.g. :
--
-- > connect sock "tcp://localhost:5560"
-- > connect sock "inproc://test"
--
-- See also: 'bind', 'shutdown'.
connect :: Socket a -> String -> IO Endpoint
connect (Socket _ sid) addr =
    withCString addr $ \adr -> do
        epid <- throwErrnoIfMinus1 "connect" $ c_nn_connect sid adr
        return $ Endpoint epid

-- | Removes an endpoint from a socket.
--
-- See also: 'bind', 'connect'.
shutdown :: Socket a -> Endpoint -> IO ()
shutdown (Socket _ sid) (Endpoint eid) =
    throwErrnoIfMinus1_ "shutdown" $ c_nn_shutdown sid eid

-- | Blocking function for sending a message
--
-- See also: 'recv', 'recv''.
send :: Sender a => Socket a -> ByteString -> IO ()
send (Socket t sid) string =
    U.unsafeUseAsCStringLen string $ \(ptr, len) ->
        throwErrnoIfMinus1RetryMayBlock_
            "send"
            (c_nn_send sid ptr (fromIntegral len) (#const NN_DONTWAIT))
            (getOptionFd (Socket t sid) (#const NN_SNDFD) >>= threadWaitWrite)

-- | Blocking receive.
recv :: Receiver a => Socket a -> IO ByteString
recv (Socket t sid) =
    alloca $ \ptr -> do
        len <- throwErrnoIfMinus1RetryMayBlock
                "recv"
                (c_nn_recv sid ptr (#const NN_MSG) (#const NN_DONTWAIT))
                (getOptionFd (Socket t sid) (#const NN_RCVFD) >>= threadWaitRead)
        buf <- peek ptr
        str <- C.packCStringLen (buf, fromIntegral len)
        throwErrnoIfMinus1_ "recv freeing message buffer" $ c_nn_freemsg buf
        return str

-- | Nonblocking receive function.
recv' :: Receiver a => Socket a -> IO (Maybe ByteString)
recv' (Socket _ sid) =
    alloca $ \ptr -> do
        len <- c_nn_recv sid ptr (#const NN_MSG) (#const NN_DONTWAIT)
        if len >= 0
            then do
                buf <- peek ptr
                str <- C.packCStringLen (buf, fromIntegral len)
                throwErrnoIfMinus1_ "recv' freeing message buffer" $ c_nn_freemsg buf
                return $ Just str
            else do
                errno <- c_nn_errno
                if errno == (#const EAGAIN) || errno == (#const EINTR)
                    then return Nothing
                    else throwErrno "recv'"

-- | Subscribe to a given subject string.
subscribe :: Socket Sub -> ByteString -> IO ()
subscribe (Socket t sid) string =
    setOption (Socket t sid) (socketType t) (#const NN_SUB_SUBSCRIBE) (StringOption string)

-- | Unsubscribes from a subject.
unsubscribe :: Socket Sub -> ByteString -> IO ()
unsubscribe (Socket t sid) string =
    setOption (Socket t sid) (socketType t) (#const NN_SUB_UNSUBSCRIBE) (StringOption string)

-- | Closes the socket. Any buffered inbound messages that were not yet
-- received by the application will be discarded. The library will try to
-- deliver any outstanding outbound messages for the time specified by
-- NN_LINGER socket option. The call will block in the meantime.
close :: Socket a -> IO ()
close (Socket _ sid) =
    throwErrnoIfMinus1Retry_ "close" $ c_nn_close sid

-- | Switches nanomsg into shutdown modus and interrupts any waiting
-- function calls.
term :: IO ()
term = c_nn_term


-- * Socket option accessors and mutators

-- not sure if this beats having setOptionInt and setOptionString..
data SocketOption = IntOption Int | StringOption ByteString
    deriving (Show)

-- Used for setting a socket option.
setOption :: Socket a -> CInt -> CInt -> SocketOption -> IO ()

setOption (Socket _ sid) level option (IntOption val) =
    alloca $ \ptr -> do
        poke ptr (fromIntegral val :: CInt)
        let cintSize = fromIntegral $ sizeOf (fromIntegral val :: CInt) :: CInt
        throwErrnoIfMinus1_ "setOption (int)" $ c_nn_setsockopt sid level option ptr cintSize

setOption (Socket _ sid) level option (StringOption str) =
    throwErrnoIfMinus1_ "setOption (string)" <$> U.unsafeUseAsCStringLen str $
        \(ptr, len) -> c_nn_setsockopt sid level option ptr (fromIntegral len)

-- Reads a socket option.
getOption :: Socket a -> CInt -> CInt -> IO CInt
getOption (Socket _ sid) level option =
    alloca $ \ptr ->
        alloca $ \sizePtr -> do
            let a = 1 :: CInt
            let cintSize = fromIntegral $ sizeOf a
            poke sizePtr cintSize
            throwErrnoIfMinus1_ "getOption" $ c_nn_getsockopt sid level option (ptr :: Ptr CInt) sizePtr
            value <- peek ptr
            size <- peek sizePtr
            if cintSize /= size then throwErrno "getOption: output size not as expected" else return value

-- Retrieves a nanomsg file descriptor for polling ready status.
getOptionFd :: Socket a -> CInt -> IO Fd
getOptionFd (Socket _ sid) option =
    alloca $ \ptr ->
        alloca $ \sizePtr -> do
            let a = 1 :: Fd
            let fdSize = fromIntegral $ sizeOf a
            poke sizePtr fdSize
            throwErrnoIfMinus1_ "getOptionFd" $ c_nn_getsockopt sid (#const NN_SOL_SOCKET) option (ptr :: Ptr Fd) sizePtr
            value <- peek ptr
            size <- peek sizePtr
            if fdSize /= size then throwErrno "getOptionFd: output size not as expected" else return value

-- | Specifies how long the socket should try to send pending outbound
-- messages after close has been called, in milliseconds.
--
-- Negative value means infinite linger. Default value is 1000 (1 second).
linger :: Socket a -> IO Int
linger s =
    fromIntegral <$> getOption s (#const NN_SOL_SOCKET) (#const NN_LINGER)

-- | Specifies how long should the socket try to send pending outbound
-- messages after close has been called, in milliseconds.
--
-- Negative value means infinite linger. Default value is 1000 (1 second).
setLinger :: Socket a -> Int -> IO ()
setLinger s val =
    setOption s (#const NN_SOL_SOCKET) (#const NN_LINGER) (IntOption val)

-- | Size of the send buffer, in bytes. To prevent blocking for messages
-- larger than the buffer, exactly one message may be buffered in addition
-- to the data in the send buffer.
--
-- Default value is 128kB.
sndBuf :: Socket a -> IO Int
sndBuf s =
    fromIntegral <$> getOption s (#const NN_SOL_SOCKET) (#const NN_SNDBUF)

-- | Size of the send buffer, in bytes. To prevent blocking for messages
-- larger than the buffer, exactly one message may be buffered in addition
-- to the data in the send buffer.
--
-- Default value is 128kB.
setSndBuf :: Socket a -> Int -> IO ()
setSndBuf s val =
    setOption s (#const NN_SOL_SOCKET) (#const NN_SNDBUF) (IntOption val)

-- | Size of the receive buffer, in bytes. To prevent blocking for messages
-- larger than the buffer, exactly one message may be buffered in addition
-- to the data in the receive buffer.
--
-- Default value is 128kB.
rcvBuf :: Socket a -> IO Int
rcvBuf s =
    fromIntegral <$> getOption s (#const NN_SOL_SOCKET) (#const NN_RCVBUF)

-- | Size of the receive buffer, in bytes. To prevent blocking for messages
-- larger than the buffer, exactly one message may be buffered in addition
-- to the data in the receive buffer.
--
-- Default value is 128kB.
setRcvBuf :: Socket a -> Int -> IO ()
setRcvBuf s val =
    setOption s (#const NN_SOL_SOCKET) (#const NN_RCVBUF) (IntOption val)

-- | Maximum message size that can be received, in bytes.
-- Negative value means that the received size is limited only by available addressable memory.
-- The type of this option is int.
--
-- Default is 1024kB.
rcvMaxSize :: Socket a -> IO Int
rcvMaxSize s =
    fromIntegral <$> getOption s (#const NN_SOL_SOCKET) (#const NN_RCVMAXSIZE)

-- | Maximum message size that can be received, in bytes.
-- Negative value means that the received size is limited only by available addressable memory.
-- The type of this option is int.
--
-- Default is 1024kB.
setRcvMaxSize :: Socket a -> Int -> IO ()
setRcvMaxSize s val =
    setOption s (#const NN_SOL_SOCKET) (#const NN_RCVMAXSIZE) (IntOption val)

-- Think I'll just skip these. There's recv' for nonblocking receive, and
-- adding a return value to send seems awkward.
--sendTimeout
--recvTimeout

-- | For connection-based transports such as TCP, this option specifies
-- how long to wait, in milliseconds, when connection is broken before
-- trying to re-establish it.
--
-- Note that actual reconnect interval may be randomised to some extent
-- to prevent severe reconnection storms.
--
-- Default value is 100 (0.1 second).
reconnectInterval :: Socket a -> IO Int
reconnectInterval s =
    fromIntegral <$> getOption s (#const NN_SOL_SOCKET) (#const NN_RECONNECT_IVL)

-- | For connection-based transports such as TCP, this option specifies
-- how long to wait, in milliseconds, when connection is broken before
-- trying to re-establish it.
--
-- Note that actual reconnect interval may be randomised to some extent
-- to prevent severe reconnection storms.
--
-- Default value is 100 (0.1 second).
setReconnectInterval :: Socket a -> Int -> IO ()
setReconnectInterval s val =
    setOption s (#const NN_SOL_SOCKET) (#const NN_RECONNECT_IVL) (IntOption val)

-- | This option is to be used only in addition to NN_RECONNECT_IVL option.
-- It specifies maximum reconnection interval. On each reconnect attempt,
-- the previous interval is doubled until NN_RECONNECT_IVL_MAX is reached.
--
-- Value of zero means that no exponential backoff is performed and reconnect
-- interval is based only on NN_RECONNECT_IVL. If NN_RECONNECT_IVL_MAX is
-- less than NN_RECONNECT_IVL, it is ignored.
--
-- Default value is 0.
reconnectIntervalMax :: Socket a -> IO Int
reconnectIntervalMax s =
    fromIntegral <$> getOption s (#const NN_SOL_SOCKET) (#const NN_RECONNECT_IVL_MAX)

-- | This option is to be used only in addition to NN_RECONNECT_IVL option.
-- It specifies maximum reconnection interval. On each reconnect attempt,
-- the previous interval is doubled until NN_RECONNECT_IVL_MAX is reached.
--
-- Value of zero means that no exponential backoff is performed and reconnect
-- interval is based only on NN_RECONNECT_IVL. If NN_RECONNECT_IVL_MAX is
-- less than NN_RECONNECT_IVL, it is ignored.
--
-- Default value is 0.
setReconnectIntervalMax :: Socket a -> Int -> IO ()
setReconnectIntervalMax s val =
    setOption s (#const NN_SOL_SOCKET) (#const NN_RECONNECT_IVL_MAX) (IntOption val)

-- | Sets outbound priority for endpoints subsequently added to the socket.
-- This option has no effect on socket types that send messages to all the
-- peers. However, if the socket type sends each message to a single peer
-- (or a limited set of peers), peers with high priority take precedence over
-- peers with low priority.
--
-- Highest priority is 1, lowest priority is 16. Default value is 8.
sndPrio :: Socket a -> IO Int
sndPrio s =
    fromIntegral <$> getOption s (#const NN_SOL_SOCKET) (#const NN_SNDPRIO)

-- | Sets outbound priority for endpoints subsequently added to the socket.
-- This option has no effect on socket types that send messages to all the
-- peers. However, if the socket type sends each message to a single peer
-- (or a limited set of peers), peers with high priority take precedence over
-- peers with low priority.
--
-- Highest priority is 1, lowest priority is 16. Default value is 8.
setSndPrio :: Socket a -> Int -> IO ()
setSndPrio s val =
    setOption s (#const NN_SOL_SOCKET) (#const NN_SNDPRIO) (IntOption val)

-- | If set to 1, only IPv4 addresses are used. If set to 0, both IPv4
-- and IPv6 addresses are used.
--
-- Default value is 1.
ipv4Only :: Socket a -> IO Int
ipv4Only s =
    fromIntegral <$> getOption s (#const NN_SOL_SOCKET) (#const NN_IPV4ONLY)

-- | If set to 1, only IPv4 addresses are used. If set to 0, both IPv4
-- and IPv6 addresses are used.
--
-- Default value is 1.
setIpv4Only :: Socket a -> Int -> IO ()
setIpv4Only s val =
    setOption s (#const NN_SOL_SOCKET) (#const NN_IPV4ONLY) (IntOption val)

-- | This option is defined on the full REQ socket. If reply is not received
-- in specified amount of milliseconds, the request will be automatically
-- resent.
--
-- Default value is 60000 (1 minute).
requestResendInterval :: Socket Req -> IO Int
requestResendInterval s =
    fromIntegral <$> getOption s (#const NN_REQ) (#const NN_REQ_RESEND_IVL)

-- | This option is defined on the full REQ socket. If reply is not received
-- in specified amount of milliseconds, the request will be automatically
-- resent.
--
-- Default value is 60000 (1 minute).
setRequestResendInterval :: Socket Req -> Int -> IO ()
setRequestResendInterval s val =
    setOption s (#const NN_REQ) (#const NN_REQ_RESEND_IVL) (IntOption val)

-- | Get timeout for Surveyor sockets
surveyorDeadline :: Socket Surveyor -> IO Int
surveyorDeadline s =
    fromIntegral <$> getOption s (#const NN_SURVEYOR) (#const NN_SURVEYOR_DEADLINE)

-- | Set timeout for Surveyor sockets
setSurveyorDeadline :: Socket Surveyor -> Int -> IO ()
setSurveyorDeadline s val =
    setOption s (#const NN_SURVEYOR) (#const NN_SURVEYOR_DEADLINE) (IntOption val)

-- | This option, when set to 1, disables Nagle's algorithm.
--
-- Default value is 0.
tcpNoDelay :: Socket a -> IO Int
tcpNoDelay s =
    fromIntegral <$> getOption s (#const NN_TCP) (#const NN_TCP_NODELAY)

-- | This option, when set to 1, disables Nagle's algorithm.
--
-- Default value is 0.
setTcpNoDelay :: Socket a -> Int -> IO ()
setTcpNoDelay s val =
    setOption s (#const NN_TCP) (#const NN_TCP_NODELAY) (IntOption val)

