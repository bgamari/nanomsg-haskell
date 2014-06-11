module Main where

import Network.Nanomsg
import Criterion.Main
import qualified Data.ByteString.Char8 as C
import Control.Monad (replicateM_)

pair :: Int -> Int -> IO ()
pair size count = do
    sender <- socket Pair
    _ <- bind sender "inproc://pairtest"
    recipient <- socket Pair
    _ <- connect recipient "inproc://pairtest"
    let msg = C.pack $ replicate size 'a'
    replicateM_ count (send sender msg >> recv recipient)
    close sender
    close recipient
    return ()

main :: IO ()
main = defaultMain
    [ bench "40 bytes x 10k messages" $ nfIO $ pair    40 10000
    , bench "20k bytes x 20 messages" $ nfIO $ pair 20000    20
    ]

