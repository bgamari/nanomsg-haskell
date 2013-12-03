module Main where

import qualified Nanomsg as N
import qualified System.ZMQ3.Monadic as Z
import Criterion.Main
import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_)

nLat :: Int -> Int -> String -> String -> IO ()
nLat size count bindString connString = do
    s1 <- N.socket N.Pair
    _ <- N.bind s1 bindString
    s2 <- N.socket N.Pair
    _ <- N.connect s2 connString
    let msg = C.pack $ replicate size 'a'
    replicateM_ count (N.send s1 msg >> N.recv s2 >>= N.send s2 >> N.recv s1)
    N.close s1
    N.close s2
    return ()

zLat :: Int -> Int -> String -> String -> IO ()
zLat size count bindString connString = Z.runZMQ $ do
    s1 <- Z.socket Z.Pair
    _ <- Z.bind s1 bindString
    s2 <- Z.socket Z.Pair
    _ <- Z.connect s2 connString
    let msg = C.pack $ replicate size 'a'
    replicateM_ count (Z.send s1 [] msg >> Z.receive s2 >>= Z.send s2 [] >> Z.receive s1)
    Z.close s1
    Z.close s2
    return ()

nThr :: Int -> Int -> String -> String -> IO ()
nThr size count bindString connString = do
    s1 <- N.socket N.Pair
    _ <- N.bind s1 bindString
    s2 <- N.socket N.Pair
    _ <- N.connect s2 connString
    let msg = C.pack $ replicate size 'a'
    replicateM_ count (replicateM_ 100 (N.send s1 msg) >> replicateM_ 100 (N.recv s2))
    N.close s1
    N.close s2
    return ()

zThr :: Int -> Int -> String -> String -> IO ()
zThr size count bindString connString = Z.runZMQ $ do
    s1 <- Z.socket Z.Pair
    _ <- Z.bind s1 bindString
    s2 <- Z.socket Z.Pair
    _ <- Z.connect s2 connString
    let msg = C.pack $ replicate size 'a'
    replicateM_ count (replicateM_ 100 (Z.send s1 [] msg) >> replicateM_ 100 (Z.receive s2))
    Z.close s1
    Z.close s2
    return ()

main :: IO ()
main = defaultMain
    [ bench "nanomsg-haskell: 40 bytes x 2k messages, lat, tcp"     $ nfIO $ nLat    40 1000 "tcp://*:5566" "tcp://localhost:5566"
    , bench "zeromq3-haskell: 40 bytes x 2k messages, lat, tcp"     $ nfIO $ zLat    40 1000 "tcp://*:5566" "tcp://localhost:5566"
    , bench "nanomsg-haskell: 20k bytes x 40 messages, lat, tcp"    $ nfIO $ nLat 20000   20 "tcp://*:5566" "tcp://localhost:5566"
    , bench "zeromq3-haskell: 20k bytes x 40 messages, lat, tcp"    $ nfIO $ zLat 20000   20 "tcp://*:5566" "tcp://localhost:5566"
    , bench "nanomsg-haskell: 40 bytes x 2k messages, lat, inproc"  $ nfIO $ nLat    40 1000 "inproc://bench" "inproc://bench"
    , bench "zeromq3-haskell: 40 bytes x 2k messages, lat, inproc"  $ nfIO $ zLat    40 1000 "inproc://bench" "inproc://bench"
    , bench "nanomsg-haskell: 20k bytes x 40 messages, lat, inproc" $ nfIO $ nLat 20000   20 "inproc://bench" "inproc://bench"
    , bench "zeromq3-haskell: 20k bytes x 40 messages, lat, inproc" $ nfIO $ zLat 20000   20 "inproc://bench" "inproc://bench"
    , bench "nanomsg-haskell: 40 bytes x 10k messages, throughput, tcp"     $ nfIO $ nThr 40 100 "tcp://*:5566" "tcp://localhost:5566"
    , bench "zeromq3-haskell: 40 bytes x 10k messages, throughput, tcp"     $ nfIO $ zThr 40 100 "tcp://*:5566" "tcp://localhost:5566"
    , bench "nanomsg-haskell: 40 bytes x 10k messages, throughput, inproc"  $ nfIO $ nThr 40 100 "inproc://bench" "inproc://bench"
    , bench "zeromq3-haskell: 40 bytes x 10k messages, throughput, inproc"  $ nfIO $ zThr 40 100 "inproc://bench" "inproc://bench"
    ]

