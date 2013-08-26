module Main where

import Nanomsg
import Criterion.Main
import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_)

pair :: Int -> Int -> Int -> IO ()
pair bufsize size count = do
    sender <- socket Pair
    _ <- bind sender "inproc://pairtest"
    recipient <- socket Pair
    _ <- connect recipient "inproc://pairtest"
    let msg = C.pack $ take size $ repeat 'a'
    replicateM_ count (send sender msg >> recv recipient bufsize)
    close sender
    close recipient
    return ()

main :: IO ()
main = defaultMain 
    [ bench "20 bytes x 100k messages, 20B preallocated buffer"     $ nfIO $  pair    20     20 100000
    , bench "20 bytes x 100k messages, 4k preallocated buffer"      $ nfIO $  pair  4000     20 100000
    , bench "20 bytes x 100k messages, 40k preallocated buffer"     $ nfIO $  pair 40000     20 100000
    , bench "20 bytes x 100k messages, nanomsg-allocated buffer"    $ nfIO $  pair     0     20 100000
    , bench "20k bytes x 100 messages, 40k preallocated buffer"     $ nfIO $  pair 20000  20000    100
    , bench "20k bytes x 100 messages, nanomsg-allocated buffer"    $ nfIO $  pair     0  20000    100
    ]

