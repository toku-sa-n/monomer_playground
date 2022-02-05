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
import TextShow (TextShow (showt))

data AppModel = AppModel
  {_index :: Int, _changing :: Bool, _visible :: Bool}
  deriving (Eq)

makeLenses ''AppModel

data Event
  = ShowNext
  | Update
  | Update2

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
      [ Event Update2,
        -- Model $ model & visible .~ False,
        Message (WidgetKey "animFadeIn0") AnimationStart
      ]
    Update2 -> [Model $ model & index +~ 1 & changing .~ False & visible .~ True]

buildUI :: WidgetEnv AppModel Event -> AppModel -> WidgetNode AppModel Event
buildUI wenv model = widgetTree
  where
    widgetTree =
      withKeys $ dualAnim 0 $ label (showt $ model ^. index) `styleBasic` [textSize 128, height 128] `nodeVisible` (model ^. visible)
    withKeys = keystroke [("Enter", ShowNext)]
    dualAnim n c = outer
      where
        inner = animFadeIn c `nodeKey` pack ("animFadeIn" ++ show n)
        outer = animFadeOut_ [onFinished Update] inner `nodeKey` pack ("animFadeOut" ++ show n)

clipSize :: V2 Int
clipSize = V2 100 100

main :: IO ()
main = do
  img <-
    Pic.convertRGBA8
      . ( \case
            Right x -> x
            Left _ -> error "Failed to load the image."
        )
      <$> Pic.readImage "./dog.jpg"
  let model = AppModel 0 False True
  startApp
    model
    handleEvent
    buildUI
    [appTheme darkTheme, appFontDef "Regular" "Roboto-Regular.ttf"]
