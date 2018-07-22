{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Lib
    ( brickTackToe
    ) where

import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (&), (.~), (%~))

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( padAll
  , str
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO)
import Brick.BChan
import Brick.Main
  ( App(..)
  , showFirstCursor
  , customMain
  , continue
  , halt
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Types
  ( Widget
  , Next
  , EventM
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( (<=>)
  , str
  )
import Network.Socket hiding (recv)
import Control.Exception (bracket)
import qualified Network.BSD as BSD
import Network.Socket.ByteString (recv)
import qualified Data.ByteString.Char8 as B

data CustomEvent = Counter 
                 | ServerMsg String 
                 deriving Show

data St =
    St { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent)
       , _stCounter :: Int
       , _serverSocket :: Socket
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [a]
    where
        a = (str $ "Last event: " <> (show $ st^.stLastBrickEvent))
            <=>
            (str $ "Counter value is: " <> (show $ st^.stCounter))

appEvent :: St -> BrickEvent () CustomEvent -> EventM () (Next St)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent _ -> continue $ st & stLastBrickEvent .~ (Just e)
        AppEvent Counter -> continue $ st & stCounter %~ (+1)
                                          & stLastBrickEvent .~ (Just e)
        _ -> continue st

initialState :: Socket -> St
initialState skt =
    St { _stLastBrickEvent = Nothing
       , _stCounter = 0
       , _serverSocket = skt
       }

theApp :: App St CustomEvent ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

cleanSocket :: Socket -> IO ()
cleanSocket skt =
    shutdown skt ShutdownBoth >> close skt

createServerSocket :: IO Socket
createServerSocket = do
    protocol <- BSD.protoNumber <$> BSD.getProtocolByName "TCP"
    socket <- socket AF_INET Stream protocol
    localhost <- inet_addr "127.0.0.1"
    let sktAddr = SockAddrInet 2000 localhost
    connect socket sktAddr
    pure socket

bufferSize :: Int
bufferSize = 1000

brickTackToe :: IO ()
brickTackToe = do
    chan <- newBChan 10

    bracket createServerSocket cleanSocket $ \serverSkt -> do
        forkIO $ forever $ do
            msg <- recv serverSkt bufferSize
            writeBChan chan (ServerMsg $ B.unpack msg)
    
        void $ customMain (V.mkVty V.defaultConfig) (Just chan) theApp (initialState serverSkt)        
