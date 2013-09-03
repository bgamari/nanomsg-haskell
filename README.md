# Overview

This is a Haskell binding for the nanomsg library: <http://nanomsg.org/>.

The goal is to create a simple and usable interface to nanomsg, not to
provide a complete mapping of the C api.


# Usage

Simple Pub/sub example:

Server:

    module Main where

    import Nanomsg
    import qualified Data.ByteString.Char8 as C
    import Control.Monad (mapM_)

    main :: IO ()
    main = do
        withSocket Pub $ \s -> do
            _ <- bind s "tcp://*:5560"
            mapM_ (\num -> sendNumber s num) (cycle [1..1000000])
        where
            sendNumber s number = do
                let numAsString = show number
                send s (C.pack numAsString)

Client:

    module Main where

    import Nanomsg
    import qualified Data.ByteString.Char8 as C
    import Control.Monad (forever)

    main :: IO ()
    main = do
        withSocket Sub $ \s -> do
            _ <- connect s "tcp://localhost:5560"
            _ <- subscribe s $ C.pack ""
            forever $ do
                msg <- recv s
                C.putStrLn msg

Nonblocking client:

    module Main where

    import Nanomsg
    import qualified Data.ByteString.Char8 as C
    import Control.Monad (forever)

    main :: IO ()
    main =
        withSocket Sub $ \s -> do
            _ <- connect s "tcp://localhost:5560"
            _ <- subscribe s $ C.pack ""
            forever $ do
                msg <- recv' s
                C.putStrLn $ case msg of
                    Nothing -> C.pack "No message"
                    Just s  -> s

