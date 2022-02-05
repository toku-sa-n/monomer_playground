{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main,
  )
where

import qualified Codec.Picture as Pic
import Codec.Picture.Extra (crop)
import Control.Lens (makeLenses, (%~), (&), (+~), (-~), (.~), (^.))
import qualified Data.ByteString as B
import Data.Text (pack)
import qualified Data.Text as T
import Data.Vector.Storable.ByteString (vectorToByteString)
import Linear.V2 (V2 (V2))
import Monomer
  ( AnimationMsg (AnimationFinished, AnimationStart, AnimationStop),
    AppEventResponse,
    CmbBgColor (bgColor),
    CmbHeight (height),
    CmbOnFinished (onFinished),
    CmbStyleBasic (styleBasic),
    CmbTextSize (textSize),
    EventResponse (Event, Message, Model),
    Size (Size),
    WidgetEnv,
    WidgetKey (WidgetKey),
    WidgetNode,
    animFadeIn,
    animFadeOut,
    animFadeOut_,
    appFontDef,
    appTheme,
    black,
    box,
    darkTheme,
    hstack,
    imageMem,
    keystroke,
    label,
    nodeKey,
    nodeVisible,
    startApp,
    vstack,
  )
import qualified Monomer.Lens as L
import TextShow (TextShow (showt))

data AppModel = AppModel
  {_index :: Int, _changing :: Bool, _visible :: Bool}
  deriving (Eq)

makeLenses ''AppModel

data Event
  = ShowNext
  | Update

handleEvent ::
  WidgetEnv AppModel Event ->
  WidgetNode AppModel Event ->
  AppModel ->
  Event ->
  [AppEventResponse AppModel Event]
handleEvent _ _ model event =
  case event of
    ShowNext
      | not $ model ^. changing ->
          [ Model $ model & changing .~ True,
            Message (WidgetKey "animFadeIn0") AnimationStop,
            Message (WidgetKey "animFadeOut0") AnimationStart
          ]
      | otherwise -> []
    Update ->
      [ Model $ model & index -~ 1 & changing .~ False & visible .~ True & index %~ max 0,
        Message (WidgetKey "animFadeIn0") AnimationStart
      ]

buildUI :: WidgetEnv AppModel Event -> AppModel -> WidgetNode AppModel Event
buildUI wenv model = widgetTree
  where
    widgetTree =
      withKeys $ box (dualAnim 0 $ label (if model ^. index == 0 then "0♡" else showt $ model ^. index) `styleBasic` [textSize 128, height 128] `nodeVisible` (model ^. visible)) `styleBasic` [bgColor $ black & L.a .~ 0.5]
    withKeys = keystroke [("Enter", ShowNext)]
    dualAnim n c = outer
      where
        inner = animFadeIn c `nodeKey` pack ("animFadeIn" ++ show n)
        outer = animFadeOut_ [onFinished Update] inner `nodeKey` pack ("animFadeOut" ++ show n)

main :: IO ()
main = do
  let model = AppModel 5 False True
  startApp
    model
    handleEvent
    buildUI
    [appTheme darkTheme, appFontDef "Regular" "NotoSansJP-Light.otf"]
