{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}
-- |
-- Module:          Nanomsg.Binary
-- Copyright:       (c) 2013 Ivar Nymoen
-- License:         MIT
-- Stability:       experimental
--
-- This is a Haskell binding for the nanomsg library: <http://nanomsg.org/>.
--
-- There's support for blocking send and recv, a non-blocking receive,
-- and for all the socket types and the functions you need to wire
-- them up and tear them down again.
--
-- Most socket options are available through accessor and mutator
-- functions. Sockets are typed, transports are not.
--
-- Socket type documentation is adapted or quoted verbatim from the
-- nanomsg manual. Please refer to nanomsg.org for information on
-- how to use the library.
--
-- This module offers a thin serialization layer ("Binary" based)
-- over @'send'@ and @'receive'@. You just need to import 
-- @Nanomsg.Binary@ instead of @Nanomsg@.

module Nanomsg.Binary
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
--  , eTERM
--  , eFSM
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

import Control.Applicative
import Nanomsg hiding (send,recv,recv')
import qualified Nanomsg as NM
import Data.Binary
import Data.ByteString.Lazy

send
  :: (Sender s, Binary dat)
  => Socket s
  -> dat
  -> IO ()
send s d = NM.send s (toStrict . encode $ d)

recv
  :: (Receiver s, Binary dat)
  => Socket s
  -> IO dat
recv s = decode . fromStrict <$> NM.recv s

recv'
  :: (Receiver s, Binary dat)
  => Socket s
  -> IO (Maybe dat)
recv' s = fmap (decode . fromStrict) <$> NM.recv' s

