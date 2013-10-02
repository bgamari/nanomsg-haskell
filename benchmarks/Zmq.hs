module Main where

import qualified Nanomsg as N
import qualified System.ZMQ3.Monadic as Z
import Criterion.Main
import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_)

nPair :: Int -> Int -> String -> String -> IO ()
nPair size count bindString connString = do
    s1 <- N.socket N.Pair
    _ <- N.bind s1 bindString
    s2 <- N.socket N.Pair
    _ <- N.connect s2 connString
    let msg = C.pack $ replicate size 'a'
    replicateM_ count (N.send s1 msg >> N.recv s2 >>= N.send s2 >> N.recv s1)
    N.close s1
    N.close s2
    return ()

zPair :: Int -> Int -> String -> String -> IO ()
zPair size count bindString connString = Z.runZMQ $ do
    s1 <- Z.socket Z.Pair
    _ <- Z.bind s1 bindString
    s2 <- Z.socket Z.Pair
    _ <- Z.connect s2 connString
    let msg = C.pack $ replicate size 'a'
    replicateM_ count (Z.send s1 [] msg >> Z.receive s2 >>= Z.send s2 [] >> Z.receive s1)
    Z.close s1
    Z.close s2
    return ()

main :: IO ()
main = defaultMain
    [ bench "nanomsg-haskell: 40 bytes x 1k messages, roundtrip, tcp"     $ nfIO $ nPair    40 1000 "tcp://*:5566" "tcp://localhost:5566"
    , bench "zeromq3-haskell: 40 bytes x 1k messages, roundtrip, tcp"     $ nfIO $ zPair    40 1000 "tcp://*:5566" "tcp://localhost:5566"
    , bench "nanomsg-haskell: 20k bytes x 20 messages, roundtrip, tcp"    $ nfIO $ nPair 20000   20 "tcp://*:5566" "tcp://localhost:5566"
    , bench "zeromq3-haskell: 20k bytes x 20 messages, roundtrip, tcp"    $ nfIO $ zPair 20000   20 "tcp://*:5566" "tcp://localhost:5566"
    , bench "nanomsg-haskell: 40 bytes x 1k messages, roundtrip, inproc"  $ nfIO $ nPair    40 1000 "inproc://bench" "inproc://bench"
    , bench "zeromq3-haskell: 40 bytes x 1k messages, roundtrip, inproc"  $ nfIO $ zPair    40 1000 "inproc://bench" "inproc://bench"
    , bench "nanomsg-haskell: 20k bytes x 20 messages, roundtrip, inproc" $ nfIO $ nPair 20000   20 "inproc://bench" "inproc://bench"
    , bench "zeromq3-haskell: 20k bytes x 20 messages, roundtrip, inproc" $ nfIO $ zPair 20000   20 "inproc://bench" "inproc://bench"
    ]

