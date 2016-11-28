{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, elems, insert, delete, empty)
import Data.Text (Text, pack)
import GHCJS.DOM.EventM (mouseOffsetXY) 

type Point = (Int,Int)

data Cmd = Trace (Int, Point) | Expire Int

type Model = Map Int Point

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

update :: Cmd -> Model -> Model
update cmd points  = 
    case cmd of
        Trace (index, location) -> insert index location points
        Expire index -> delete index points

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

main = mainWidget $ do
    let attrs = constDyn $ 
                    fromList [ ("width" , "600")
                             , ("height", "400")
                             , ("style" , "border:solid; margin:8em")
                             ]

    rec 
        (elm, dExpireEvents) <- elDynAttrNS' svgns "svg" attrs $ listWithKey model showPoint

        traceEvent <- wrapDomEvent (_element_raw elm) (onEventName Mousemove) mouseOffsetXY

        dTraceAdd <- foldDyn (\newPos present -> (1 + fst present,newPos)) (0, (0,0)) traceEvent

        let viewEvents = leftmost [ Trace <$> updated dTraceAdd
                          , switch $ leftmost.elems <$> current dExpireEvents
                          ]
        model <- foldDyn update mempty viewEvents
    return ()
