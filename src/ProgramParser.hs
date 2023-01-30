{-
| Program Parser
-}
module ProgramParser where

----------------------------
--        Imports         --
----------------------------
import           Control.Applicative

-- Zelf gedefinieerd
import           Parser
import           Parserdata

-----------------------------
-- Basic parsing functions --
-----------------------------
-- Parse natuurlijk getal
nat :: Parser Exp
nat = do n <- some digit
         return (Lit $ read n)

-- Parse integer
int :: Parser Exp
int = do _ <- char '-'
         Lit n <- nat
         return (Lit $ -n)
      <|> nat

-- Parse positieve float
posfloat :: Parser Exp
posfloat = do n <- some digit
              _ <- char '.'
              m <- some digit
              return (Lit $ read (n ++ "." ++ m))
              <|> int

-- Parse float
floatbasic :: Parser Exp
floatbasic = do _ <- some (char '-')
                Lit n <- posfloat
                return (Lit $ -n)
              <|> posfloat

------------------------------------------------
-- Basic parsing functions with space removal --
------------------------------------------------
natural :: Parser Exp
natural = token nat

integer :: Parser Exp
integer = token int

float :: Parser Exp
float = token floatbasic

-- Parse puntcomma
semicol :: Parser String
semicol = token $ symbol ";"

-- Parse variabele, rekening houdende met gereserveerde woorden
var :: Parser Exp
var = do x <- identifier reserved
         return (Var x)

-- Parse boolean
boolean :: Parser Exp
boolean = do b <- token upperlower
             case b of
               "Waar" -> return $ Lit 1
               "Vals" -> return $ Lit 0
               _      -> empty

-----------------------------
--  Parse Sensor Functies  --
-----------------------------
sensor :: Parser Exp
sensor = do _ <- symbol "lees"
            s <- token $ many lower
            case s of
               "afstandsensor" -> return $ Sensor Distance
               "lijnsensor"    -> return $ Sensor Line
               _               -> empty



sensorvalue :: Parser Exp
sensorvalue = do x <- token upperlower
                 y <- token upperlower
                 case x ++ y of
                    "LinksZwart"  -> return $ Lit 11
                    "RechtsZwart" -> return $ Lit 21
                    "BeideZwart"  -> return $ Lit 31
                    "BeideWit"    -> return $ Lit 30
                    _             -> empty

-----------------------------
--     Parse Expressies    --
-----------------------------
-- Parse volledige expressie, samengesteld uit expressies hieronder
fexp :: Parser Exp
fexp = rexp `chainl1` boolop

rexp :: Parser Exp
rexp = expr `chainl1` relop

expr :: Parser Exp
expr = term `chainl1` addop

term :: Parser Exp
term = factor `chainl1` mulop

factor :: Parser Exp
factor = boolean <|> sensorvalue <|> sensor <|> var <|> float <|> bracket (symbol "(") fexp (symbol ")")

-- Hulpfunctie om operatoren te parsen
ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 (<|>) [do {_ <- p; return op} | (p, op) <- xs]

-- Parsen van operatoren
addop :: Parser (Exp -> Exp -> Exp)
addop  = ops [(symbol "+", (:+:)), (symbol "-", (:-:))]

mulop :: Parser (Exp -> Exp -> Exp)
mulop  = ops [(symbol "*", (:*:)), (symbol "/", (:/:))]

relop :: Parser (Exp -> Exp -> Exp)
relop = ops [(symbol "==", (:==:)), (symbol "<=", (:<=:)), (symbol ">=", (:>=:)), (symbol ">", (:>:)), (symbol "<", (:<:)), (symbol "!=", (:!=:))]

boolop :: Parser (Exp -> Exp -> Exp)
boolop = ops [(symbol "of", (:||:)), (symbol "en", (:&&:))]

-----------------------------
--     Parse Statements    --
-----------------------------
-- Parse volledig statement, statements worden gescheiden door putncomma's
stmt :: Parser Stmt
stmt = do x <- fun <$> sepby1 stmt' semicol
          _ <- semicol
          return x
  where
    -- Wanneer er slechts 1 stmt als resultaat is, geef het terug zonder
    -- het in een seq [] te steken, indien meerdere steek ze in een seq []
    fun res = if length res == 1 then head res else Seq res

-- Parse 1 enkele statement
stmt' :: Parser Stmt
stmt' = assign <|> declare <|> conditional <|> while <|> cmd <|> sleep

-----------------------------
--    Parse Assignaties    --
-----------------------------
-- Parse variabele assignatie
assign :: Parser Stmt
assign = do v <- identifier reserved
            _ <- symbol "="
            e <- fexp
            return (v := e)

-- Parse variabele declaratie
declare :: Parser Stmt
declare = do _ <- symbol "variabele"
             (v := e) <- assign
             return $ Declare v e

---------------------------------
-- Parse Lussen & conditionals --
---------------------------------
-- Parse conditional
conditional :: Parser Stmt
conditional = condelse <|> cond

-- Parse if else
condelse :: Parser Stmt
condelse = do (Cond e c) <- cond
              _ <- symbol "anders"
              d <- bracket (symbol "{") stmt (symbol "}")
              return $ Condelse e c d

-- Parse if
cond :: Parser Stmt
cond = do _ <- symbol "als"
          e <- bracket (symbol "(") fexp (symbol ")")
          c <- bracket (symbol "{") stmt (symbol "}")
          return $ Cond e c

-- Parse while lus
while :: Parser Stmt
while = do _ <- symbol "zolang"
           e <- bracket (symbol "(") fexp (symbol ")")
           c <- bracket (symbol "{") stmt (symbol "}")
           return $ While e c


-----------------------------
--  Parse robotcommando's  --
-----------------------------
-- Parse slaapcommando
sleep :: Parser Stmt
sleep = do _ <- symbol "slaap"
           i <- fexp
           return $ Sleep i

-- Parse robotcommando's
cmd :: Parser Stmt
cmd = motorcmd <|> lightcmd

-- Parse motorcommando
motorcmd :: Parser Stmt
motorcmd = do _ <- symbol "motor"
              r <- parsemotordir
              do s <- fexp
                 return $ Command $ Motor r s
               <|> do return $ Command $ Motor r (Lit 0) -- Snelheid standaard 0

-- Parse richting voor motor
parsemotordir :: Parser Direction
parsemotordir = do r <- token (many lower)
                   case r of
                     "links"        -> return MLeft
                     "rechts"       -> return MRight
                     "voorwaarts"   -> return MForward
                     "achterwaarts" -> return MBackwards
                     "stop"         -> return MStop
                     _              -> empty

-- Parse lichtcommando
lightcmd :: Parser Stmt
lightcmd = do _ <- symbol "zetled"
              r <- parselightside
              c <- parsecolor
              return $ Command $ Light r c

-- Parse kant voor licht
parselightside :: Parser LightSide
parselightside = do r <- token (many lower)
                    case r of
                      "links"  -> return LLeft
                      "rechts" -> return LRight
                      _        -> empty

-- Parse kleur voor licht
parsecolor :: Parser Color
parsecolor = do j <- token (many lower)
                case j of
                   "rood"  -> return Red
                   "groen" -> return Green
                   "blauw" -> return Blue
                   "wit"   -> return White
                   _       -> empty
