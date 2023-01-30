module WorldRenderer where

import WorldParser
import RobotManipulator

import Graphics.Gloss as G
import Graphics.Gloss.Interface.IO.Simulate
import Control.Concurrent
import Debug.Trace

type Model = (World, Device)

render :: G.Picture -> G.Picture -> G.Picture -> Model -> IO G.Picture
render robotBMP muurBMP rasterBMP (wereld, connectie) = 
    do let bot       = G.Rotate (toDegrees $ rotatie $ robot wereld) robotWithLeds 
       let bot' = renderAt bot (locatie $ robot wereld)
       let omheining  = map renderOmheining (raster wereld)
       muren     <- return $ map renderMuren (muren wereld)
       lijnen    <- return $ map renderLijnen (lijnen wereld)
       return $ G.pictures $ lijnen ++ [bot'] ++ omheining ++ muren
    where cell                           = 32
          toDegrees                      = (*) (180/pi)
          led n i                        = G.translate 0 (-6 + (n-1)*15) $ color (ledColor i) $ G.circleSolid 5
          ledColor n                     = kleur (leds (robot wereld) !! n)
          kleur (r,g,b)                  = makeColor r g b 1
          robotWithLeds                  = G.Pictures [robotBMP, led 1 1, led 2 2]
          renderAt bmp (x, y)            = G.translate (toPix fst x) (toPix snd y) bmp
          toPix which                    = (+ (cell / 2 - cell * size which / 2)) . (* cell) 
          size which                     = maximum $ map which (raster wereld)
          renderOmheining               = renderAt rasterBMP 
          renderMuren                   = renderAt muurBMP 
          renderLijnen ((x,y), (x',y'))  = renderAt (line [(0,0), (32*x' - 32*x ,32*y' - 32*y)]) (x, y) -- we maken een bitmap van hoe de lijn eruit moet zien moest hij vertrekken vanaf de oorsprong

update :: ViewPort -> Float -> Model -> IO Model
update _ sec (w, d) = do c <- tryTakeMVar $ command d
                         let new = interpretCommand c w 
                         _       <- tryReadSensor c d w  -- sensoren naar MVar
                         let updated = drive new sec
                         return (updated, d)

-- een functie om de robot te laten bewegen afhankelijk van de motoinstellingen
drive :: World -> Float -> World
drive w sec = w {robot = moveBot (robot w)} 
    where moveBot r         
              | getCell (locatie r) `elem` (muren w ++ raster w ) = jumpBack r -- als er een botsing is
              | otherwise =  r {locatie = (translateX r, translateY r), rotatie = translateRot r}
          translateX r      = fst (locatie r) + translate cos r
          translateY r      = snd (locatie r) - translate sin r
          translate geom  r = sec * (wheelR/2) * (motorL + motorR) * geom (rotatie r) 
          translateRot r    = rotatie r + (sec * (wheelR/wheelA) * (motorR - motorL))
          wheelR = 0.3 --straal van het wiel (cm)
          wheelA = 1.15 --straal van het wiel (cm)
          motorL = fromIntegral (fst $ motoren (robot w)) * vertraging
          motorR = fromIntegral (snd $ motoren (robot w)) * vertraging
          vertraging = 1/25 -- onze robot een beetje trager laten gaan, anders is het niet realistisch
          getCell (x', y') = (fromIntegral $ round x', fromIntegral $ round y') 
          jumpBack r = r {locatie = (fst (locatie r) - cos(rotatie r), snd (locatie r) + sin (rotatie r))}

start wereldNaam connection = 
    do muurBMP      <- loadBMP "images/wall.bmp"
       robotBMP     <- loadBMP "images/car.bmp"
       omheiningBMP <- loadBMP "images/lava.bmp"
       wereld       <- readWorld wereldNaam
       simulateIO G.FullScreen
                  G.white
                  30 --fps
                  (wereld, connection) -- model
                  (render robotBMP muurBMP omheiningBMP)
                  update
