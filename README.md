# nanomsg-haskell

This is a Haskell binding for the nanomsg library: <http://nanomsg.org/>.

There's support for [blocking](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Concurrent.html#v:threadWaitRead) send and recv, a non-blocking receive,
and for all the socket types and the functions you need to wire them up and
tear them down again.

Most socket options are available through accessor and mutator
functions. Sockets are typed, transports are not.


## Building

You would normally make sure the nanomsg library is on your system and then
install from Hackage using "cabal update && cabal install nanomsg-haskell",
but can build from the repository following these steps:

  1. Build and install nanomsg (and zmq3, if you are building benchmarks)
  1. git clone https://github.com/ivarnymoen/nanomsg-haskell
  1. cd nanomsg-haskell && cabal sandbox init
  1. cabal install --dependencies-only [--enable-tests] [--enable-benchmarks]
  1. cabal configure [--enable-tests] [--enable-benchmarks]
  1. cabal build
  1. [cabal test]


## Contributing

Just submit a pull request for small stuff, but please get in touch
beforehand before sinking a lot of effort into major/API changes.

Remember adding your name to the AUTHORS file.


## Usage

Simple Pub/sub example:

Server:

    module Main where

    import Nanomsg
    import qualified Data.ByteString.Char8 as C
    import Control.Monad (mapM_)
    import Control.Concurrent (threadDelay)

    main :: IO ()
    main = do
        withSocket Pub $ \s -> do
            _ <- bind s "tcp://*:5560"
            mapM_ (\num -> sendNumber s num) (cycle [1..1000000])
        where
            sendNumber s number = do
                threadDelay 1000        -- let's conserve some cycles
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
            subscribe s $ C.pack ""
            forever $ do
                msg <- recv s
                C.putStrLn msg

Nonblocking client:

    module Main where

    import Nanomsg
    import qualified Data.ByteString.Char8 as C
    import Control.Monad (forever)
    import Control.Concurrent (threadDelay)

    main :: IO ()
    main =
        withSocket Sub $ \s -> do
            _ <- connect s "tcp://localhost:5560"
            subscribe s $ C.pack ""
            forever $ do
                threadDelay 700           -- let's conserve some cycles
                msg <- recv' s
                C.putStrLn $ case msg of
                    Nothing -> C.pack "No message"
                    Just s  -> s

