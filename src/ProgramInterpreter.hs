{-
| Interpreter
-}
module Interpreter (runProgram) where

----------------------------
--        Imports         --
----------------------------
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Class      (MonadTrans (lift))
import           Control.Monad.Trans.Reader     (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.State.Lazy (StateT (..), get, modify, put)
import           Data.Map                       as M
import           Language.Haskell.TH            (runIO)
import           MBot
import           System.Environment
import           System.HIDAPI                  (Device)

-- Zelf gedefinieerd
import           Parser
import           Parserdata
import           ProgramParser

----------------------------
--       Datatypes        --
----------------------------
type Value = Float
type Variable = String
type Mem = Map Variable Value   -- Geheugen

type Interp a = ReaderT Device (StateT Mem IO) a

----------------------------
--  Evalueren Expressies  --
----------------------------
eval :: Exp -> Interp Value
eval (Lit n)     = return n
eval (Var x)     = do r <- lift get
                      case M.lookup x r of
                         Just v -> return v
eval (e1 :+: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 + v2)
eval (e1 :-: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 - v2)
eval (e1 :*: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 * v2)
eval (e1 :/: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return (v1 / v2)
eval (e1 :>: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return $ if v1 > v2 then 1 else 0
eval (e1 :<: e2) = do v1 <- eval e1
                      v2 <- eval e2
                      return $ if v1 < v2 then 1 else 0
eval (e1 :==: e2) = do v1 <- eval e1
                       v2 <- eval e2
                       return $ if v1 == v2 then 1 else 0
eval (e1 :!=: e2) = do v1 <- eval e1
                       v2 <- eval e2
                       return $ if v1 /= v2 then 1 else 0
eval (e1 :>=: e2) = do v1 <- eval e1
                       v2 <- eval e2
                       return $ if v1 >= v2 then 1 else 0
eval (e1 :<=: e2) = do v1 <- eval e1
                       v2 <- eval e2
                       return $ if v1 <= v2 then 1 else 0
eval (e1 :&&: e2) = do v1 <- eval e1
                       v2 <- eval e2
                       return $ if v1 /= 0 && v2 /= 0 then 1 else 0
eval (e1 :||: e2) = do v1 <- eval e1
                       v2 <- eval e2
                       return $ if v1 /= 0 || v2 /= 0 then 1 else 0
eval (Sensor s)   = do d <- ask
                       case s of
                         Distance -> liftIO $ readUltraSonic d
                         Line     -> do x <- liftIO $ readLineFollower d
                                        return $ evalLine x

-- Line waarden worden intern voorgesteld als floats
evalLine :: Line -> Float
evalLine x = case x of
               LEFTB  -> 11
               RIGHTB -> 21
               BOTHB  -> 31
               BOTHW  -> 30

----------------------------
--  Uitvoeren statements  --
----------------------------
exec :: Stmt -> Interp ()
exec (x := e)           = do v <- eval e
                             m <- lift get
                             lift $ put $ M.insert x v m
exec (Declare x e)      = exec (x := e)
exec (Condelse e s1 s2) = do v <- eval e
                             if v /= 0 then exec s1 else exec s2
exec (Cond e s)         = do v <- eval e
                             when (v /= 0) $ exec s
exec (While e s)        = do v <- eval e
                             when (v /= 0) (exec (Seq [s, While e s]))
exec (Seq [])           = return ()
exec (Seq (s : ss))     = do exec s
                             exec (Seq ss)
exec (Sleep s)          = do v <- eval s -- sleep voorgesteld in seconden
                             liftIO $ threadDelay $ round (v * 1000000)
exec (Command c)        = do d <- ask
                             case c of
                                 Light ls col    -> liftIO $ sendCommand d $ setRGBUncurry (evalLightSide ls) (evalLightColor col)
                                 Motor dir speed -> do s <- eval speed
                                                       liftIO $ sendCommand d $ evalMotor dir (round s)


-- Kleurzijde omzetten naar corresponderend nummer
evalLightSide :: LightSide -> Int
evalLightSide ls = case ls of
                     LLeft  -> 1
                     LRight -> 2

-- Kleur omzetten naar 3-tuple
evalLightColor :: Color -> (Int, Int, Int)
evalLightColor col = case col of
                       Red   -> (100, 0  , 0)
                       Green -> (0  , 100, 0)
                       Blue  -> (0  , 0  , 100)
                       White -> (100, 100, 100)

-- setRGB functie toepassen met 3-tuple als kleur argument
setRGBUncurry :: Int -> (Int, Int, Int) -> Command
setRGBUncurry ls (r, g, b) = setRGB ls r g b

-- Zet richting en snelheid om naar een (Motor) commando
evalMotor :: Direction -> Int -> Command
evalMotor dir speed = case dir of
                        MForward   -> setMotor speed speed
                        MBackwards -> setMotor (-speed) (-speed)
                        MLeft      -> setMotor 0 speed
                        MRight     -> setMotor speed 0
                        MStop      -> setMotor 0 0

----------------------------
--   Uitvoeren programma  --
----------------------------
-- Parse het programma met gegeven naam
parseProgram :: String -> IO Stmt
parseProgram s = do x <- readFile $ "programs/" ++ s ++ ".nls"
                    return $ parse stmt x

-- Interpreteer het programma
runInterp :: Stmt -> Mem -> Device -> IO ((), Mem)
runInterp p st d = runStateT (runReaderT (exec p) d) st

-- Parse het gegeven programma en interpreteer het daarna
runProgram :: String -> Device -> IO ((), Mem)
runProgram s d = do p <- parseProgram s
                    runInterp p M.empty d

-- Main functie
main :: IO ()
main =  do
 args <- getArgs
 d <- openMBot
 runProgram (head args) d
 closeMBot d
