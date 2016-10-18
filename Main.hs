{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map as DM (Map, fromList, elems)
import Data.Text as DT (Text, pack, append)
import GHCJS.DOM.EventM (mouseOffsetXY) 
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Control.Monad.Trans (liftIO)
import System.Random
import Control.Monad.Random

type Point = (Double,Double)

height = 400
width = 600

data Color = Red | Green | Blue | Orange | Purple deriving (Show, Bounded, Enum)

data Cmd = Pick (Int, Int) | Pop Int

data Ball  = Ball { position :: Point
                  , radius   :: Double
                  , color    :: Text
                  } 

data Model = Model { gen   ::  StdGen 
                   , balls ::  [Ball] 
                   }

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

newBall :: (RandomGen g) => (Int,Int) -> (Rand g Ball)
newBall (x,y) = do
    radius <- getRandomR (10.0, 30.0) 
    let minColor = fromEnum (minBound :: Color)
        maxColor = fromEnum (maxBound :: Color)
    colorIndex <- getRandomR (minColor, maxColor) 
    let position = (fromIntegral x, fromIntegral y)
        colorText = (pack.show) (toEnum colorIndex :: Color)
        ball = Ball position radius colorText
    return ball

update :: Cmd -> Model -> Model
update (Pick location) (Model gen cs)  = 
    let (ball, newGen) = runRand (newBall location) gen 
    in Model newGen (ball : cs)

update (Pop index) model@(Model _ cs) = 
    let (cs0, cs1) = splitAt index cs 
    in model {balls = cs0 ++ tail cs1}

ballToAttrs :: Ball -> Map Text Text
ballToAttrs (Ball (x,y) radius color) =
    DM.fromList [ ( "cx",     pack $ show x)
                , ( "cy",     pack $ show y)
                , ( "r",      pack $ show radius)
                , ( "style",  "fill:" `DT.append` color)
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

        ballMap = fmap (DM.fromList.(\b -> (zip [0..] b) ).balls) model

    (elm, dPopEventMap) <- elDynAttrNS' svgns "svg" attrs $ listWithKey ballMap showBall

    pickEvent <- wrapDomEvent 
                      (_element_raw elm) 
                      (onEventName Mousedown) 
                      mouseOffsetXY

    return $ leftmost [ fmap Pick pickEvent 
                      , switch $ (leftmost . elems) <$> current dPopEventMap
                      ]

main = mainWidget $ do
    gen <- liftIO getStdGen
    rec 
        model <- foldDyn update (Model gen []) =<< view model
    return ()
