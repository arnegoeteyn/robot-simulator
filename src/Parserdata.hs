{-
| Parser data
-}
module Parserdata where

-- Gereserveerde sleutelwoorden
reserved :: [String]
reserved = ["variabele", "en", "of", "Waar", "Vals", "zolang", "als"
            , "anders", "ga", "links", "rechts", "voorwaarts", "achterwaarts"
            , "stop", "lees", "afstandsensor", "lijnsensor", "zetled",
            "groen", "blauw", "wit", "rood"]

-- Expressies
data Exp = Lit Float          -- constante
          | Var String        -- variable
          | Exp :+: Exp
          | Exp :-: Exp
          | Exp :*: Exp
          | Exp :/: Exp
          | Exp :>: Exp
          | Exp :<: Exp
          | Exp :==: Exp
          | Exp :!=: Exp
          | Exp :>=: Exp
          | Exp :<=: Exp
          | Exp :&&: Exp      -- logische en
          | Exp :||: Exp      -- logische of
          | Sensor SensorType -- sensor opvraging
          deriving (Eq, Show)

-- Statements
data Stmt = String := Exp          -- toekenning
          | Declare String Exp     -- initialisatie variabele
          | Condelse Exp Stmt Stmt -- if else statement
          | Cond Exp Stmt          -- if statement
          | While Exp Stmt         -- while lus
          | Seq [Stmt]             -- meerdere statements
          | Command Cmd            -- robotcommando
          | Sleep Exp              -- slaapcommando
          deriving (Eq, Show)


-- Commando's
data Cmd = Light LightSide Color
         | Motor Direction Exp
         deriving (Eq, Show)

-- Kleuren
data Color = Red
           | Green
           | Blue
           | White
           deriving (Eq, Show)

-- Richtingen
data Direction = MForward
               | MBackwards
               | MLeft
               | MRight
               | MStop
               deriving (Eq, Show)

-- Zijden licht
data LightSide = LLeft
               | LRight
               deriving (Eq, Show)

-- Sensor types
data SensorType = Distance
                | Line
                deriving (Eq, Show)
