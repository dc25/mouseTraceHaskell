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

data Color = Red | Green | Blue | Orange | Purple deriving (Show, Bounded, Enum)

data Cmd = Pick (Int, Int) | Pop Int

data Ball  = Ball { position :: Point
                  } 

data Model = Model { nextIndex :: Int
                   , balls ::  Map Int Ball
                   }

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

update :: Cmd -> Model -> Model
update (Pick location) (Model ni cs)  = 
    let ball = Ball location
    in Model (ni+1) (insert ni ball cs)

update (Pop index) model@(Model _ cs) = 
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
    let dCircleAttrs = fmap ballToAttrs dBall

    (el,_) <- elStopPropagationNS svgns "g" Mousedown $ 
                 elDynAttrNS' svgns "circle" dCircleAttrs $ return ()

    db <- delay 1 =<< getPostBuild
    return $ fmap (const $ Pop index) db

view :: MonadWidget t m => Dynamic t Model -> m (Event t Cmd)
view model = do
    let attrs = constDyn $ 
                    DM.fromList 
                        [ ("width" , pack $ show width)
                        , ("height", pack $ show height)
                        , ("style" , "border:solid; margin:8em")
                        ]

        ballMap = fmap balls model

    (elm, dPopEventMap) <- elDynAttrNS' svgns "svg" attrs $ listWithKey ballMap showBall

    pickEvent <- wrapDomEvent 
                      (_element_raw elm) 
                      (onEventName Mousemove) 
                      mouseOffsetXY

    return $ leftmost [ fmap Pick pickEvent 
                      , switch $ (leftmost . elems) <$> current dPopEventMap
                      ]

main = mainWidget $ do
    gen <- liftIO getStdGen
    rec 
        model <- foldDyn update (Model 0 empty) =<< view model
    return ()
