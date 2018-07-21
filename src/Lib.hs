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

data CustomEvent = Counter deriving Show

data St =
    St { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent)
       , _stCounter :: Int
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

initialState :: St
initialState =
    St { _stLastBrickEvent = Nothing
       , _stCounter = 0
       }

theApp :: App St CustomEvent ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const $ attrMap V.defAttr []
        }

brickTackToe :: IO ()
brickTackToe = do
    chan <- newBChan 10

    forkIO $ forever $ do
        writeBChan chan Counter
        threadDelay 1000000

    void $ customMain (V.mkVty V.defaultConfig) (Just chan) theApp initialState