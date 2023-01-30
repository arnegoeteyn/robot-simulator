module SimulateCommands where

import WorldRenderer
import RobotManipulator
import SimulatorSensors
import Debug.Trace
import Control.Concurrent
import System.Exit

openMBot :: IO Device
openMBot = do m <- newEmptyMVar
              sensor <- newEmptyMVar
              line <- newEmptyMVar
              let device = MBot m sensor line
              _ <- forkIO (start world device)
              return device
    where world = "worlds/opgave.world"

-- het sluiten van de connectie == sluiten van het programma
closeMBot :: Device -> IO ()
closeMBot _ = exitSuccess

setRGB :: Int -> Int -> Int -> Int -> Command
setRGB index r g b = (RGB, [index,r,g,b])

setMotor :: Int -> Int -> Command
setMotor l r = (MOTOR, [l, r])

-- een commando sturen waardoor de sensorwaarde wordt uitgelezen
-- de uitgelezen waarden vinden we dan terug in het Device
readUltraSonic :: Device -> IO Float
readUltraSonic d = do sendCommand d (USSENSOR, [])
                      takeMVar (usSensor d) 

readLineFollower :: Device -> IO Line
readLineFollower d = do sendCommand d (LINESENSOR, [])
                        takeMVar (lineSensor d) 

sendCommand :: Device -> Command -> IO ()
sendCommand (MBot d s l) = putMVar d 
