module WorldParser where

import           Data.Char
import           Data.List.Split

-- voorstellen van een locatie op de wereld
-- inspiratie gehaald uit de opgave sobokan
type X = Float
type Y = Float
type Coord = (X,Y)
type Lijn = (Coord, Coord)
type RGBColor = (Float, Float, Float)

data Robot = Robot { locatie  :: Coord
                   , rotatie  :: Float
                   , motoren  :: (Int, Int) -- waarde van 0 tot 255
                   , leds     :: [RGBColor]
                   } deriving (Show)

data World = World { robot  :: Robot
                   , muren  :: [Coord]
                   , lijnen :: [Lijn]
                   , raster :: [Coord]
                   } deriving (Show)

-- een lege startwereld weergeven
emptyWorld :: World
emptyWorld = World newBot [] [] []

-- een lege robot
newBot :: Robot
newBot = Robot (0,0) 0 (0,0) emptyLeds

-- alle leds die uit staan
emptyLeds :: [RGBColor]
emptyLeds = [ledOff, ledOff, ledOff]
    where ledOff = (0.0,0.0,0.0)

-- linken van plekken met coordinaat
-- y coordinaat negatief want wereld is gedraaid
withCoord :: [String] -> [(Char, Coord)]
withCoord wereld = [(chr, (x,-y))
                   | (y, rij) <- zip [0..] wereld
                   , (x, chr) <- zip [0..] rij]

-- Lijst van karakters omzetten naar een gesorteerde wereld
makeWorld :: [String] -> World
makeWorld wereld = foldr (uncurry add) emptyWorld (withCoord wereld)
    where add 'X' coord wereld = wereld {muren = coord : muren wereld}
          add '+' coord wereld = wereld {raster = coord : raster wereld}
          add '-' coord wereld = wereld {raster = coord : raster wereld}
          add '|' coord wereld = wereld {raster = coord : raster wereld}
          add '>' coord wereld = wereld {robot = Robot coord 0 (0,0) emptyLeds}
          add '<' coord wereld = wereld {robot = Robot coord pi  (0,0) emptyLeds}
          add '^' coord wereld = wereld {robot = Robot coord (3*pi/2)   (0,0) emptyLeds}
          add 'v' coord wereld = wereld {robot = Robot coord (pi/2) (0,0) emptyLeds}
          add _ coord wereld = wereld

addLines :: [String] -> World -> World
addLines l wereld         =  wereld {lijnen = map naarLijnTuple l}
    where naarLijnTuple r = naarLijn (map (\s -> read s::Float) (getallen r))
          naarLijn g      = ((head g, -1 * (g !! 1)),(g !! 2, -1 * g !!3))
          getallen        = wordsBy (not . isDigit)

-- een string (pad naar bestand) omzetten naar een wereld
readWorld :: String -> IO World
readWorld file = do bestand <- lines <$> readFile file
                    let lijnen = filter (\n -> head n == '(') bestand
                    return $ addLines lijnen (makeWorld bestand)

