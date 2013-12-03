{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Nanomsg
import Test.Framework.TH (defaultMainGenerator)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Control.Concurrent (threadDelay)
import Control.Applicative ( (<$>) )
import Data.Maybe (catMaybes)

instance Arbitrary ByteString where
    arbitrary = C.pack <$> arbitrary

-- dummy test
prop_reverse :: [Int] -> Bool
prop_reverse xs =
    xs == reverse (reverse xs)

-- test Pub and Sub sockets
prop_PubSub :: Property
prop_PubSub = monadicIO $ do
    msgs <- pick arbitrary
    pre $ not (null msgs)
    res <- run $ do
        pub <- socket Pub
        ep1 <- bind pub "inproc://pubsub"
        sub1 <- socket Sub
        ep2 <- connect sub1 "inproc://pubsub"
        subscribe sub1 $ C.pack ""
        sub2 <- socket Sub
        ep3 <- connect sub2 "inproc://pubsub"
        subscribe sub2 $ C.pack ""
        threadDelay 1000
        r <- mapM (sendMsg pub sub1 sub2) msgs
        unsubscribe sub2 $ C.pack ""
        unsubscribe sub1 $ C.pack ""
        shutdown sub2 ep3
        shutdown sub1 ep2
        shutdown pub ep1
        close pub
        close sub1
        close sub2
        threadDelay 1000
        return r
    assert $ and res
        where
            sendMsg pub sub1 sub2 msg = do
                send pub msg
                send pub msg
                a <- recv sub1
                b <- recv sub1
                c <- recv sub2
                d <- recv sub2
                return $ a == msg && b == msg && c == msg && d == msg

-- test Pair sockets
prop_Pair :: Property
prop_Pair = monadicIO $ do
    msgs <- pick arbitrary
    pre $ not (null msgs)
    res <- run $ do
        s1 <- socket Pair
        _ <- bind s1 "inproc://pair"
        s2 <- socket Pair
        _ <- connect s2 "inproc://pair"
        threadDelay 1000
        -- Send message from s1 to s2, then back from s2 to s1, then make sure it hasn't changed
        r <- mapM (\m -> send s1 m >> recv s2 >>= send s2 >> recv s1 >>= return . (== m)) msgs
        close s1
        close s2
        threadDelay 1000
        return r
    assert $ and res

-- test Pipeline (Push & Pull) sockets
prop_Pipeline :: Property
prop_Pipeline = monadicIO $ do
    msgs <- pick arbitrary
    pre $ not (null msgs)
    res <- run $ do
        push <- socket Push
        _ <- bind push "inproc://pipeline"
        pull1 <- socket Pull
        pull2 <- socket Pull
        _ <- connect pull1 "inproc://pipeline"
        _ <- connect pull2 "inproc://pipeline"
        threadDelay 1000
        r <- mapM (testSockets push pull1 pull2) msgs
        close push
        close pull1
        close pull2
        threadDelay 1000
        return r
    assert $ and res
        where
            testSockets push pull1 pull2 msg = do
                send push msg
                send push msg
                send push msg
                threadDelay 1000
                a <- recv' pull1
                b <- recv' pull1
                c <- recv' pull1
                d <- recv' pull2
                e <- recv' pull2
                f <- recv' pull2
                let xs = catMaybes [a, b, c, d, e, f]
                return $ all (== msg) xs && (length xs == 3)

-- test Req and Rep sockets
prop_ReqRep :: Property
prop_ReqRep = monadicIO $ do
    msgs <- pick arbitrary
    pre $ not (null msgs)
    res <- run $ do
        req <- socket Req
        _ <- bind req "inproc://reqrep"
        rep <- socket Rep
        _ <- connect rep "inproc://reqrep"
        threadDelay 1000
        r <- mapM (\m -> send req m >> recv rep >>= send rep >> recv req >>= return . (== m)) msgs
        close req
        close rep
        threadDelay 1000
        return r
    assert $ and res

-- test Bus socket
prop_Bus :: Property
prop_Bus = monadicIO $ do
    msgs <- pick arbitrary
    pre $ not (null msgs)
    res <- run $ do
        -- Probably not how you're supposed to connect Bus nodes..
        b1 <- socket Bus
        _ <- bind b1 "inproc://bus1"
        b2 <- socket Bus
        _ <- connect b2 "inproc://bus1"
        _ <- bind b2 "inproc://bus2"
        b3 <- socket Bus
        _ <- connect b3 "inproc://bus2"
        _ <- bind b3 "inproc://bus3"
        _ <- connect b1 "inproc://bus3"
        threadDelay 1000
        r <- mapM (testSockets b1 b2 b3) msgs
        close b1
        close b2
        close b3
        threadDelay 1000
        return r
    assert $ and res
        where
            testSockets b1 b2 b3 msg = do
                send b1 msg
                a <- recv b2
                b <- recv b3
                send b2 msg
                c <- recv b1
                d <- recv b3
                send b3 msg
                e <- recv b1
                f <- recv b2
                return $ all (== msg) [a, b, c, d, e, f]

prop_TestOptions :: Property
prop_TestOptions = monadicIO $ do
    res <- run $ do
        req <- socket Req
        _ <- bind req "tcp://*:5560"
        surveyor <- socket Surveyor
        _ <- bind surveyor "inproc://surveyor"
        threadDelay 1000
        setTcpNoDelay req 1
        v1 <- tcpNoDelay req
        setTcpNoDelay req 0
        v2 <- tcpNoDelay req
        setRequestResendInterval req 30000
        v3 <- requestResendInterval req
        setIpv4Only req 0
        v4 <- ipv4Only req
        setIpv4Only req 1
        v5 <- ipv4Only req
        setSndPrio req 7
        v6 <- sndPrio req
        setReconnectInterval req 50
        v7 <- reconnectInterval req
        setReconnectIntervalMax req 400
        v8 <- reconnectIntervalMax req
        setRcvBuf req 200000
        v9 <- rcvBuf req
        setSndBuf req 150000
        v10 <- sndBuf req
        setLinger req 500
        v11 <- linger req
        setSurveyorDeadline surveyor 2000
        v12 <- surveyorDeadline surveyor
        close req
        close surveyor
        threadDelay 1000
        return [v1 == 1, v2 == 0, v3 == 30000, v4 == 0, v5 == 1, v6 == 7,
            v7 == 50, v8 == 400, v9 == 200000, v10 == 150000, v11 == 500, v12 == 2000]
    assert $ and res

main :: IO ()
main = $defaultMainGenerator

