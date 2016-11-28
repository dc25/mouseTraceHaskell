{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, elems)
import Data.Text (Text, pack)
import GHCJS.DOM.EventM (mouseOffsetXY) 

type Point = (Int,Int)

svgns :: Maybe Text
svgns = Just "http://www.w3.org/2000/svg"

boxAttrs :: Map Text Text
boxAttrs = 
    fromList [ ("width" , "600")
             , ("height", "400")
             , ("style" , "border:solid; margin:8em")
             ]

pointAttrs :: Point -> Map Text Text
pointAttrs (x,y) =
    fromList [ ( "cx",     pack $ show x)
             , ( "cy",     pack $ show y)
             , ( "r",      "10.0")
             , ( "style",  "fill:green")
             ] 

showPoint :: MonadWidget t m => Int -> Point -> m (Event t Int)
showPoint index point = do
    elStopPropagationNS svgns "g" Mousemove $ 
        elDynAttrNS' svgns "circle" (constDyn $ pointAttrs point) $ return ()
    db <- delay 0.3 =<< getPostBuild
    return $ index <$ db 

main = mainWidget $ do
    rec 
        let expireMapEv0 = listHoldWithKey mempty updateEvents showPoint
        (elm, expireMapEv) <- elDynAttrNS' svgns "svg" (constDyn boxAttrs) expireMapEv0

        mouseEv <- wrapDomEvent (_element_raw elm) (onEventName Mousemove) mouseOffsetXY
        mouseDyn <- foldDyn (\newPos present -> (1 + fst present,newPos)) (0, (0,0)) mouseEv

        let traceDyn = fmap (\(index, pos) -> fromList [(index, Just pos)]) mouseDyn
            traceEv = updated traceDyn
            expireIndexEv = switch $ leftmost.elems <$> current expireMapEv
            expireEv = fmap (\index -> fromList [(index, Nothing)]) expireIndexEv
            updateEvents = leftmost [ traceEv, expireEv ]
    return ()
