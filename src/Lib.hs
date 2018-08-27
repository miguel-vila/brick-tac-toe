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
  , withBorderStyle
  , (<+>)
  , hBox
  , vBox
  , vLimit
  , hLimit
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
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
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)

data CustomEvent = ServerMsg String 
                 deriving Show

type Position = (Int, Int)

data St =
    St { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent)
       , _serverSocket :: Socket
       , _serverMsg :: Maybe String
       , _selectedPosition :: Maybe Position
       }

makeLenses ''St

box s =
    withBorderStyle BS.unicodeBold $ 
        B.borderWithLabel (str "") $
        vLimit 5 $
        hLimit 5 $
        C.hCenter $ 
        padAll 1 $
        str s

stDebug st =
    (box $ "Lasts event: " <> (show $ st^.stLastBrickEvent))
    <+>
    (box $ "server msg" <> (show $ st^.serverMsg)) 

board :: Widget ()
board = vBox $ replicate 3 $ hBox $ replicate 3 (box "")

drawUI :: St -> [Widget ()]
drawUI st = pure $  board

sendToServer :: Socket -> String -> IO ()
sendToServer skt str =
    sendAll skt (B.pack str)

appEvent :: St -> BrickEvent () CustomEvent -> EventM () (Next St)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
        VtyEvent (V.EvKey V.KEnter []) -> do
            liftIO $ sendToServer (st^.serverSocket) "teeest"
            continue $ st & stLastBrickEvent .~ (Just e)
        VtyEvent _ -> continue $ st & stLastBrickEvent .~ (Just e)
        AppEvent (ServerMsg msg) ->
            continue $ st & serverMsg .~ (Just msg)
                          & stLastBrickEvent .~ (Just e)
        _ -> continue st

initialState :: Socket -> St
initialState skt =
    St { _stLastBrickEvent = Nothing
       , _serverSocket = skt
       , _serverMsg = Nothing
       , _selectedPosition = Just (0,0)
       }

theApp :: App St CustomEvent ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

closeSocket :: Socket -> IO ()
closeSocket skt =
--    shutdown skt ShutdownBoth >> close skt
    pure ()

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

    bracket createServerSocket closeSocket $ \serverSkt -> do
        forkIO $ forever $ do
            msg <- recv serverSkt bufferSize
            unless (B.null msg) $ writeBChan chan (ServerMsg $ B.unpack msg)
    
        void $ customMain (V.mkVty V.defaultConfig) (Just chan) theApp (initialState serverSkt)        
