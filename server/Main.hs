{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString
    (recv, sendAll)
import Data.ByteString (append)

logAndEcho :: Socket -> IO () 
logAndEcho sock = forever $ do
    (soc, _) <- accept sock 
    printAndKickback soc
    close soc
    where printAndKickback conn = do
            msg <- recv conn 2500 
            print msg
            sendAll conn (msg <> "!")

main :: IO ()
main = withSocketsDo $ do
    addrinfos <- getAddrInfo
                 (Just (defaultHints
                    { addrFlags = 
                        [AI_PASSIVE] }))
                    Nothing (Just "2000") 
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) 
                Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 1
    logAndEcho sock
    close sock
