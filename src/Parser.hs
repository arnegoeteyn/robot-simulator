{-
| Basic Parser
-}
module Parser where

----------------------------
--        Imports         --
----------------------------
import           Control.Applicative
import           Control.Monad
import           Data.Char

----------------------------
--      Basic parser      --
----------------------------
-- Parser definitie
newtype Parser a = Parser (String -> [(a,String)])

-- Pas een paser toe gegeven een string
apply :: Parser a -> String -> [(a, String)]
apply (Parser f) = f

-- Geef resultaat van parser terug indien succesvol
parse :: Parser a -> String -> a
parse f s = case apply f s of
   [(n, [])]  -> n
   [(_, out)] -> error ("Input not completely parsed: " ++ out)
   _          -> error "Parse failed"

-- Geeft lege lijst terug bij lege input, geeft anders het eerste karakter als resultaat
item :: Parser Char
item = Parser (\input -> case input of
                           []     -> []
                           (x:xs) -> [(x, xs)])

----------------------------
--      Parser Monad      --
----------------------------
instance Monad Parser where
   return a = Parser (\input -> [(a, input)])
   m >>= f  = Parser (\input -> case apply m input of
                                   [(a, inp')] -> apply (f a) inp'
                                   _           -> [])

instance Functor Parser where
   fmap = liftM

instance Applicative Parser where
   pure   = return
   (<*>)  = ap

----------------------------
--    Parser MonadPlus    --
----------------------------
instance MonadPlus Parser where
   -- Geeft parser terug die altijd faalt
   mzero       = Parser (const [])
   -- Probeert eerst eerste parser, en probeert tweede als eerste faalt
   p `mplus` q = Parser (\input -> case apply p input of
                                     [(a, inp')] -> [(a, inp')]
                                     _           -> apply q input)

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

 ----------------------------
 -- Extra Parser Functions --
 ----------------------------
-- Parse herhaaldelijk parser p, gescheiden door applicaties van parser sep waarvan
-- het resultaat wordt weggegooid
sepby           :: Parser a -> Parser b -> Parser [a]
p `sepby` sep    = (p `sepby1` sep) <|> return []

sepby1          :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep   = do a <- p
                      as <- many (do _ <- sep
                                     p)
                      return (a:as)

-- Parse herhaaldelijk parser p, gescheiden door applicaties van parser op wiens
-- waarde een operator is die links associatief is en gebruikt wordt om de resultaten
-- van de parsers te combineren
chainl          :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a    = (p `chainl1` op) <|> return a

chainl1         :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op   = do x <- p
                      rest x
                      where
                        rest x = do f <- op
                                    y <- p
                                    rest (f x y)
                                 <|> return x

-- Parse een karakter dat voldoet aan predicaat p
spot :: (Char -> Bool) -> Parser Char
spot p = do x <- item
            if p x then return x else empty

-- Parse p en negeer de openende en sluitende haakjes
bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do _ <- open
                          x <- p
                          _ <- close
                          return x

-----------------------------
-- Basic parsing functions --
-----------------------------
-- Parse een nummer
digit :: Parser Char
digit = spot isDigit

-- Parse een lowercase karakter
lower :: Parser Char
lower = spot isLower

-- Parse een uppercase karakter
upper :: Parser Char
upper = spot isUpper

-- Parse een letter
letter :: Parser Char
letter = spot isAlpha

-- Parse een nummer / letter
alphanum :: Parser Char
alphanum = spot isAlphaNum

-- Parse gegeven karakter
char :: Char -> Parser Char
char c = spot (== c)

-- Parse gegeven string
string :: String -> Parser String
string [] = return []
string (x:xs) = do _ <- char x
                   _ <- string xs
                   return (x:xs)

-- Parse variable
ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

-- Eerste karakter hoofdletter, rest kleine letter
upperlower :: Parser String
upperlower = do x <- upper
                xs <- many lower
                return (x:xs)

----------------------------
--         Spacing        --
----------------------------
isNewLine :: Char -> Bool
isNewLine '\n' = True
isNewLine _    = False

-- Verwijderd alle space (\n, \t, ...)
space :: Parser ()
space = do _ <- some (spot isSpace)
           return ()

-- Verwijderd single line commentaar
comment :: Parser ()
comment = do _ <- string "#" -- comment identifier
             _ <- many (spot (not . isNewLine))
             _ <- string "\n"
             return ()

-- Verwijderd multi line commentaar
multiLineComment :: Parser ()
multiLineComment = do _ <- bracket (string "@") (many (spot (/= '@'))) (string "@")
                      return ()

-- Verwijderd commentaar en spaces
junk :: Parser ()
junk = do _ <- many (space <|> comment <|> multiLineComment)
          return ()

-- Parse p en verwijder alle commentaar en spaces ervoor en erachter
token :: Parser a -> Parser a
token p = do _ <- junk
             v <- p
             _ <- junk
             return v

------------------------------------------
-- Parsing functions with space removal --
------------------------------------------
-- Parse een variabele
-- reserved: gereserveerde woorden die niet mogen gebruikt worden
identifier :: [String] -> Parser String
identifier reserved = token $ do x <- ident
                                 if x `notElem` reserved then return x else empty

-- Parse een string met junk (space) removal
symbol :: String -> Parser String
symbol xs = token (string xs)
