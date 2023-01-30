module SimulatorSensors where

import WorldParser

import Debug.Trace
import Data.List

data Line = LEFTB | RIGHTB | BOTHB | BOTHW deriving (Eq, Show)

-- wat is de afstand to een muur
readUltraSonicSim :: World -> Float
readUltraSonicSim (World robot muren lijnen raster) = fakePath (rotatie robot) (locatie robot) (muren ++ raster)

-- We kijken pixel per pixel voor de robot tot we een pixel tegenkomen die in een vakje ligt dat een muur is
fakePath :: Float -> Coord -> [Coord] -> Float
fakePath rot (x,y) matches =  firstHit suspectPixels
    where suspectPixels = map location [1..(15*32)]
          location i    = (x + cos rot * i, y - sin rot * i)
          getCell (x', y') = (fromIntegral $ floor x', fromIntegral $ floor y') -- een int index voor de cell
          firstHit [] = 16
          firstHit (k:ks)
            | getCell k `elem` matches = distanceTo k
            | otherwise = firstHit ks
          distanceTo (x', y') = (*4) $ sqrt $ ((x' - (x + 0.5))^2) + ((y' -(y-0.5))^2) -- +0.5 zodat de start in het midden van de robot is, niet linksboven

-- We gebruiken een beetje wiskunden om te kijken of locatie van de sensor tussen de start en einde van de lijn ligt
-- Hier kan er nog heel veel aan verbeterd worden
-- ik denk dat een beter manier is om echt de pixels van het scherm te lezen en  te kijken of die zwart zijn, zo werkt dit direct ook als de lijn niet rechtdoor gaat
readLineFollowSim :: World -> Line
readLineFollowSim w 
    | left && right = BOTHB
    | left = LEFTB
    | right = RIGHTB
    | otherwise = BOTHW
    where getCell (x',y') = (fromIntegral $ round x', fromIntegral $ round y') 
          left = correctSquare (x,y) && leftSensor
          right = correctSquare (x,y) && rightSensor
          (x,y) = locatie $ robot w
          correctSquare (x',y') = True `elem` map (ligtTussen (getCell (x', y'))) (lijnen w) -- staan we op het juiste vakje
          leftSensor = correctSquare (x + (0.5 * cos rot), y - (0.5 * sin rot))
          rightSensor = correctSquare (x - (0.5 * cos rot), y + (0.5 * sin rot))
          rot = rotatie $ robot w 
          opEenLijn c = map (ligtTussen c) (lijnen w)
          ligtTussen x (a, b) = (abs (kruisproduct a b x) == 0) && (dotproduct a b x >= 0) && (dotproduct a b x <= squaredlength a b)
          kruisproduct (ax, ay) (bx, by) (xx, xy) = (xy - ay) * (bx - ax) - (xx - ax) * (xy - by)
          dotproduct (ax, ay) (bx, by) (xx, xy) = (xy - ay) * (bx - ax) + (xx - ax) * (xy - by)
          squaredlength (ax, ay) (bx, by) = (bx - ax)*(bx - ax) + (by - ay)*(by - ay)
