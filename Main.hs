{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, elems, insert, delete, empty)
import Data.Text (Text, pack)
import GHCJS.DOM.EventM (mouseOffsetXY) 

type Point = (Int,Int)

type Model = Map Int Point

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

pointAttrs :: Point -> Map Text Text
pointAttrs (x,y) =
    fromList [ ( "cx",     pack $ show x)
             , ( "cy",     pack $ show y)
             , ( "r",      "10.0")
             , ( "style",  "fill:green")
             ] 

showPoint :: MonadWidget t m => Int -> Point -> m (Event t (Map Int (Maybe Point)))
showPoint index point = do
    elStopPropagationNS svgns "g" Mousemove $ 
        elDynAttrNS' svgns "circle" (constDyn $ pointAttrs point) $ return ()
    db <- delay 0.3 =<< getPostBuild
    return $ ( (fromList $ [(index,Nothing)]) <$ db )

main = mainWidget $ do
    let attrs = constDyn $ 
                    fromList [ ("width" , "600")
                             , ("height", "400")
                             , ("style" , "border:solid; margin:8em")
                             ]

    rec 
        (elm, dExpireEvents) <- elDynAttrNS' svgns "svg" attrs $ listHoldWithKey mempty updateEvents showPoint

        traceEvent <- wrapDomEvent (_element_raw elm) (onEventName Mousemove) mouseOffsetXY

        dTraceAdd <- foldDyn (\newPos present -> (1 + fst present,newPos)) (0, (0,0)) traceEvent

        let dTraceAddMap = fmap (\(index, pos) -> fromList $ [(index, Just pos)]) dTraceAdd

        let updateEvents = leftmost [ updated dTraceAddMap
                          , switch $ leftmost.elems <$> current dExpireEvents
                          ]
    return ()
