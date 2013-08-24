module Main where

import Nanomsg
import Control.Monad
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    s <- mkPairSocket
    _ <- bind s "tcp://localhost:8899"
    forever $ do
        msg <- recv s
        send s msg
        C.putStrLn msg

