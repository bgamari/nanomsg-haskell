module Main where

import Nanomsg
import Control.Monad
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    s <- mkPairSocket
    _ <- connect s "tcp://localhost:8899"
    forever $ do
        send s (C.pack "Hello world")
        _ <- recv' s
        _ <- recv' s
        _ <- recv' s
        return (0 :: Int)

