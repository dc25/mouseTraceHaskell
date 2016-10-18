{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, elems, insert, delete, empty)
import Data.Text (Text, pack)
import GHCJS.DOM.EventM (mouseOffsetXY) 

type Point = (Int,Int)

data Cmd = Trace Point | Expire Int

data Model = Model { nextIndex :: Int
                   , points ::  Map Int Point
                   }

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

update :: Cmd -> Model -> Model
update cmd (Model ni points)  = 
    case cmd of
        Trace location -> Model (ni+1) $ insert ni location points
        Expire index ->   Model ni     $ delete index points

pointAttrs :: Point -> Map Text Text
pointAttrs (x,y) =
    fromList [ ( "cx",     pack $ show x)
             , ( "cy",     pack $ show y)
             , ( "r",      "10.0")
             , ( "style",  "fill:green")
             ] 

showPoint :: MonadWidget t m => Int -> Dynamic t Point -> m (Event t Cmd)
showPoint index dPoint = do
    elStopPropagationNS svgns "g" Mousemove $ 
        elDynAttrNS' svgns "circle" (pointAttrs <$> dPoint) $ return ()
    db <- delay 0.3 =<< getPostBuild
    return $ (const $ Expire index) <$> db

view :: MonadWidget t m => Dynamic t Model -> m (Event t Cmd)
view model = do
    let attrs = constDyn $ 
                    fromList [ ("width" , "600")
                             , ("height", "400")
                             , ("style" , "border:solid; margin:8em")
                             ]

        pointMap = points <$> model

    (elm, dExpireEvents) <- elDynAttrNS' svgns "svg" attrs $ listWithKey pointMap showPoint

    traceEvent <- wrapDomEvent (_element_raw elm) (onEventName Mousemove) mouseOffsetXY

    return $ leftmost [ Trace <$> traceEvent 
                      , switch $ leftmost.elems <$> current dExpireEvents
                      ]

main = mainWidget $ do
    rec 
        model <- foldDyn update (Model 0 empty) =<< view model
    return ()
