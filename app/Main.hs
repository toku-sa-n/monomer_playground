{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main
  ( main
  ) where

import qualified Codec.Picture                   as Pic
import           Codec.Picture.Extra             (crop)
import           Control.Lens                    (makeLenses, (&), (+~), (^.))
import qualified Data.ByteString                 as B
import qualified Data.Text                       as T
import           Data.Vector.Storable.ByteString (vectorToByteString)
import           Linear.V2                       (V2 (V2))
import           Monomer                         (AppEventResponse,
                                                  EventResponse (Model),
                                                  Size (Size), WidgetEnv,
                                                  WidgetNode, appFontDef,
                                                  appTheme, darkTheme, imageMem,
                                                  keystroke, label, startApp,
                                                  vstack)
import           TextShow                        (TextShow (showt))

data AppModel =
  AppModel
    { _position :: V2 Int
    , _name     :: Int
    , _dogImage :: Pic.Image Pic.PixelRGBA8
    }
  deriving (Eq)

makeLenses ''AppModel

data Event
  = MoveUp
  | MoveDown
  | MoveRight
  | MoveLeft

handleEvent ::
     WidgetEnv AppModel Event
  -> WidgetNode AppModel Event
  -> AppModel
  -> Event
  -> [AppEventResponse AppModel Event]
handleEvent _ _ model event =
  case event of
    MoveUp    -> [Model $ model & position +~ V2 0 1 & name +~ 1]
    MoveDown  -> [Model $ model & position +~ V2 0 (-1) & name +~ 1]
    MoveRight -> [Model $ model & position +~ V2 1 0 & name +~ 1]
    MoveLeft  -> [Model $ model & position +~ V2 (-1) 0 & name +~ 1]

buildUI :: WidgetEnv AppModel Event -> AppModel -> WidgetNode AppModel Event
buildUI wenv model = widgetTree
  where
    widgetTree =
      withKeys $
      vstack
        [ label $ T.pack $ "Position: " <> show (model ^. position)
        , imageMem (showt $ model ^. name) byteImage $
          Size (fromIntegral lenX) (fromIntegral lenY)
        ]
    withKeys =
      keystroke
        [ ("Up", MoveUp)
        , ("Down", MoveDown)
        , ("Right", MoveRight)
        , ("Left", MoveLeft)
        ]
    byteImage =
      vectorToByteString $
      Pic.imageData $ crop posX posY lenX lenY $ model ^. dogImage
    V2 posX posY = model ^. position
    V2 lenX lenY = clipSize

clipSize :: V2 Int
clipSize = V2 100 100

main :: IO ()
main = do
  img <-
    Pic.convertRGBA8 .
    (\case
       Right x -> x
       Left _  -> error "Failed to load the image.") <$>
    Pic.readImage "./dog.jpg"
  let model = AppModel (V2 100 100) 0 img
  startApp
    model
    handleEvent
    buildUI
    [appTheme darkTheme, appFontDef "Regular" "Roboto-Regular.ttf"]
