{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, elems, insert, delete, empty)
import Data.Text (Text, pack)
import GHCJS.DOM.EventM (mouseOffsetXY) 

type Point = (Int,Int)

data Cmd = Trace (Int, Int) | Expire Int

data Ball  = Ball { position :: Point } 

data Model = Model { nextIndex :: Int
                   , balls ::  Map Int Ball
                   }

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

update :: Cmd -> Model -> Model
update (Trace location) (Model ni cs)  = 
    Model (ni+1) (insert ni (Ball location) cs)

update (Expire index) model@(Model _ cs) = 
    model {balls = delete index cs}

ballToAttrs :: Ball -> Map Text Text
ballToAttrs (Ball (x,y) ) =
    fromList [ ( "cx",     pack $ show x)
             , ( "cy",     pack $ show y)
             , ( "r",      "10.0")
             , ( "style",  "fill:purple")
             ] 

showBall :: MonadWidget t m => Int -> Dynamic t Ball -> m (Event t Cmd)
showBall index dBall  = do
    elDynAttrNS' svgns "circle" (fmap ballToAttrs dBall) $ return ()
    db <- delay 0.5 =<< getPostBuild
    return $ fmap (const $ Expire index) db

view :: MonadWidget t m => Dynamic t Model -> m (Event t Cmd)
view model = do
    let attrs = constDyn $ 
                    fromList 
                        [ ("width" , "600")
                        , ("height", "400")
                        , ("style" , "border:solid; margin:8em")
                        ]

        ballMap = fmap balls model

    (elm, dExpireEventMap) <- elDynAttrNS' svgns "svg" attrs $ listWithKey ballMap showBall

    traceEvent <- wrapDomEvent 
                      (_element_raw elm) 
                      (onEventName Mousemove) 
                      mouseOffsetXY

    return $ leftmost [ fmap Trace traceEvent 
                      , switch $ (leftmost . elems) <$> current dExpireEventMap
                      ]

main = mainWidget $ do
    rec 
        model <- foldDyn update (Model 0 empty) =<< view model
    return ()
