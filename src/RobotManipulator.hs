module RobotManipulator where 

import SimulatorSensors

import WorldParser
import Debug.Trace
import Control.Concurrent (newMVar, MVar, newEmptyMVar, putMVar, takeMVar, forkIO)

-- een connection tussen de simulator en het programma, een device en een lijst met argumenten
type Command = (Dev, [Int])

-- Device van de robot dat iets met de actie doet
data Dev = RGB | MOTOR | USSENSOR | LINESENSOR deriving (Show)

-- een device heeft 3 MVar's een commando dat uitgevoerd moet worden en de sensors
data Device = MBot {command :: MVar Command, usSensor :: MVar Float, lineSensor :: MVar Line}

-- Kijken of er al een nieuw commando is (is het Nothing of just)
interpretCommand :: Maybe Command -> World -> World
interpretCommand Nothing w = w
interpretCommand (Just c) w  = executeCommand c w 

-- Aan 'Dev' kunnen we afleiden wat er moet gebeuren
executeCommand :: Command -> World -> World
executeCommand (RGB, arg) w      = w {robot = setLed (robot w) arg}
executeCommand (MOTOR, arg) w    = w {robot = setMotors (robot w) arg }
executeCommand (_, arg) w = w -- voor de sensoren doen we hier niets

tryReadSensor :: Maybe Command -> Device -> World -> IO()
tryReadSensor (Just c) d w = readSensor c d w
tryReadSensor Nothing d w = return ()

readSensor :: Command -> Device -> World -> IO()
readSensor (USSENSOR, _) d w = putMVar (usSensor d) (readUltraSonicSim w)
readSensor (LINESENSOR, _) d w = putMVar (lineSensor d) (readLineFollowSim w)
readSensor _ d w = return () -- voor niet sensoren


setLed, setMotors :: Robot -> [Int] -> Robot
setLed robot arg = robot {leds = newLeds}
    where newLeds     = replaceNth index newLed (leds robot) 
          index       = head arg
          newLed      = (toPercent (arg !! 1), toPercent $ arg !! 2, toPercent $ arg !! 3)
          toPercent i = fromIntegral i/100.0
          replaceNth n newVal (x:xs)
             | n == 0 = newVal:xs
             | otherwise = x:replaceNth (n-1) newVal xs

setMotors robot arg = robot {motoren = (head arg, arg !! 1)}
