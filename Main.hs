{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map as DM (Map, fromList, elems, insert, delete, empty)
import Data.Text as DT (Text, pack, append)
import GHCJS.DOM.EventM (mouseOffsetXY) 
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Control.Monad.Trans (liftIO)
import System.Random
import Control.Monad.Random

type Point = (Int,Int)

height = 400
width = 600

data Cmd = Trace (Int, Int) | Expire Int

data Ball  = Ball { position :: Point } 

data Model = Model { nextIndex :: Int
                   , balls ::  Map Int Ball
                   }

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

update :: Cmd -> Model -> Model
update (Trace location) (Model ni cs)  = 
    let ball = Ball location
    in Model (ni+1) (insert ni ball cs)

update (Expire index) model@(Model _ cs) = 
    model {balls = delete index cs}

ballToAttrs :: Ball -> Map Text Text
ballToAttrs (Ball (x,y) ) =
    DM.fromList [ ( "cx",     pack $ show x)
                , ( "cy",     pack $ show y)
                , ( "r",      "10.0")
                , ( "style",  "fill:grey")
                ] 

showBall :: MonadWidget t m => Int -> Dynamic t Ball -> m (Event t Cmd)
showBall index dBall  = do
    elStopPropagationNS svgns "g" Mousedown $ 
        elDynAttrNS' svgns "circle" (fmap ballToAttrs dBall) $ return ()

    db <- delay 1 =<< getPostBuild
    return $ fmap (const $ Expire index) db

view :: MonadWidget t m => Dynamic t Model -> m (Event t Cmd)
view model = do
    let attrs = constDyn $ 
                    DM.fromList 
                        [ ("width" , pack $ show width)
                        , ("height", pack $ show height)
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
    gen <- liftIO getStdGen
    rec 
        model <- foldDyn update (Model 0 empty) =<< view model
    return ()
