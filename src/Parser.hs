{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Parser where
import Control.Applicative
import Data.Monoid

data Unit = Spoon | Grams | Pincee | None deriving (Show, Eq, Enum)
data Quantity = Quantity Int Unit deriving (Show, Eq)
data Ingredient = Ingredient {quantity::Quantity, ingredient::String} deriving (Show)

tests :: [String]
tests =
  [ "3 cuillères de Fruits"
  , "3 morceaux de pains"
  , "5 bananes"
  , "une pincée de sel"
  ]

main :: IO ()
main = mapM_ print $ map (snd . run parseIngredient) tests

parseIngredient :: Parser Ingredient
parseIngredient = Ingredient
  <$> parseQuantity
  <*> anyString

parseUnit :: Parser Unit
parseUnit = spoon <|> grams <|> pincee <|> pure None
    where
        grams, spoon, pincee :: Parser Unit
        grams = matchOneOfAs ["gr", "grams", "grammes"] Grams
        spoon = matchOneOfAs ["gr", "grams", "grammes"] Spoon
        pincee = matchOneOfAs ["pincée de", "paincai"] Pincee

parseQuantity :: Parser Quantity
parseQuantity = Quantity <$> sp number' <*> sp parseUnit

---------- PARSER LIB

matchOneOfAs :: [String] -> a -> Parser a
matchOneOfAs l u = P $ \str -> if elem str l then ([], Right u) else ([], Left (str <> " not found"))

-- ^ number' can parse more litteral forms of numbers such as `une`
number' :: Parser Int
number' = readI <$> many1 (oneOf digits)
      <|> sp (matchOneOfAs ["un", "une", "one"] 1)
      <|> sp (matchOneOfAs ["deux", "dos", "two"] 2)

number :: Parser Int
number = readI <$> many1 (oneOf digits)

readI :: String -> Int
readI = read :: String -> Int

sp :: forall b. Parser b -> Parser b
sp = inside' spaces spaces

spaces :: Parser [Char]
spaces = many0 (oneOf " \n\t")

anyString :: Parser [Char]
anyString = many1 $ oneOf letters

enclosedBy :: forall b. Char -> Parser b -> Parser b
enclosedBy c = inside c c

inside' :: forall (f :: * -> *) a b a1.
             Applicative f =>
             f a1 -> f a -> f b -> f b
inside' a b x = (\_ g _ -> g) <$> a <*> x <*> b

inside :: Char -> Char -> Parser b -> Parser b
inside l r parser = (\_ x _ -> x) <$> char l <*> parser <*> char r

is :: (Char -> Bool) -> Parser Char
is check = P $ \str -> case str of
  [] -> ("", Left "premature end of input")
  (c:cs) | check c -> (cs, Right c)
         | otherwise -> (cs, Left "invalide condition")

letters :: [Char]
letters = ['A'..'z'] ++ digits ++ "()&\"@#?.;,:-_/=+$ \n\t"

digits :: [Char]
digits = ['0'..'9']

oneOf :: [Char] -> Parser Char
oneOf chars = is (`elem` chars)

int :: Parser Char
int = oneOf digits

char :: Char -> Parser Char
char c = P $ \str -> case str of
  (x:str1) -> if x == c then (str1, Right c) else (str1, Left "nope")
  _ -> ([], Left "empty string given")

string :: [Char] -> Parser [Char]
string txt = case txt of
  [] -> pure []
  (x:xs) -> (:) <$> char x <*> string xs

many0 :: Parser a -> Parser [a]
many0 p = P $ \str -> case run p str of
  (_, Left _) -> (str, Right [])
  (str1, Right a1) -> run ((:) <$> pure a1 <*> many0 p) str1

many1 :: forall a. Parser a -> Parser [a]
many1 p = P $ \str -> case run p str of
  (str1, Left _) -> (str1, Left "err")
  (str1, Right a1) -> run ((:) <$> pure a1 <*> many0 p) str1

type Error = String
newtype Parser a = P {run :: String -> (String, Either Error a)}
instance Show (Parser a) where show _ = "parser"

instance Functor Parser where
  fmap f parser = P $ \str -> case run parser str of
    (res, Right a) -> (res, Right $ f a)
    (res, Left e)  -> (res, Left e)

instance Applicative Parser where
  pure a = P $ \str -> (str, Right a)
  p1 <*> p2 = P $ \str -> case run p1 str of
    (res, Left e)  -> (res, Left e)
    (res, Right f) -> case run p2 res of
      (res2, Left e2) -> (res2, Left e2)
      (res2, Right val) -> (res2, Right $ f val)

instance Alternative Parser where
  empty = P $ \str -> (str, Left "error")
  p1 <|> p2 = P $ \str -> case run p1 str of
    r1@(_, Right _) -> r1
    (_, Left _) -> run p2 str