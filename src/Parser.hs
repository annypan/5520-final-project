module Parser
  ( Parser,
    doParse,
    get,
    eof,
    filter,
    parse,
    ParseError,
    satisfy,
    alpha,
    digit,
    upper,
    lower,
    space,
    char,
    string,
    int,
    chainl1,
    choice,
    between,
    sepBy1,
    sepBy,
    many,
    some,
    (<|>),
    parseFromFile,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (guard)
import Data.Char
import Data.Foldable (asum)
import System.IO qualified as IO
import System.IO.Error qualified as IO
import Text.Read (readMaybe)
import Prelude hiding (filter)

newtype Parser a = P {doParse :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = P $ \s -> do
    (c, cs) <- doParse p s
    return (f c, cs)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \s -> Just (x, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = P $ \s -> do
    (f, s') <- doParse p1 s
    (x, s'') <- doParse p2 s'
    return (f x, s'')

instance Alternative Parser where
  empty :: Parser a
  empty = P $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s

  many :: (Alternative f) => f a -> f [a]
  many p = some p <|> pure []

  some :: (Alternative f) => f a -> f [a]
  some p = (:) <$> p <*> many p

instance Monad Parser where
  return :: a -> Parser a
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (P p) >>= f = P $ \s -> do
    (a, s') <- p s
    doParse (f a) s'

-- | Combine two Maybe values together, producing the first
-- successful result
firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust Nothing y = y

-- | Return the next character from the input
get :: Parser Char
get = P $ \s -> case s of
  [] -> Nothing
  (c : cs) -> Just (c, cs)

-- | This parser *only* succeeds at the end of the input.
eof :: Parser ()
eof = P $ \s -> case s of
  [] -> Just ((), [])
  _ : _ -> Nothing

-- | Filter the parsing results by a predicate
filter :: (a -> Bool) -> Parser a -> Parser a
filter f p = P $ \s -> do
  (c, cs) <- doParse p s
  guard (f c)
  return (c, cs)

type ParseError = String

parse :: Parser a -> String -> Either ParseError a
parse parser str = case doParse parser str of
  Nothing -> Left "No parses"
  Just (a, _) -> Right a

-- | Return the next character if it satisfies the given predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = filter p get

-- | Parsers for specific sorts of characters
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace

-- | Parses and returns the specified character
-- succeeds only if the input is exactly that character
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | Parses and returns the specified string.
-- Succeeds only if the input is the given string
string :: String -> Parser String
string = foldr (\c p -> (:) <$> char c <*> p) (pure "")

int :: Parser Int
int = f <$> ((++) <$> string "-" <*> some digit <|> some digit)
  where
    f str = case readMaybe str of
      Just x -> x
      Nothing -> error $ "Bug: can't parse '" ++ str ++ "' as an int"

-- | Parses one or more occurrences of @p@ separated by binary operator
-- parser @pop@.  Returns a value produced by a /left/ associative application
-- of all functions returned by @pop@.
-- See the end of the `Parsers` lecture for explanation of this operator.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` pop = foldl comb <$> p <*> rest
  where
    comb x (op, y) = x `op` y
    rest = many ((,) <$> pop <*> p)

-- | Combine all parsers in the list (sequentially)
choice :: [Parser a] -> Parser a
choice = asum -- equivalent to: foldr (<|>) empty

-- | @between open close p@ parses @open@, followed by @p@ and finally
--   @close@. Only the value of @p@ is pureed.
between :: Parser open -> Parser a -> Parser close -> Parser a
between open p close = open *> p <* close

-- | @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile parser filename = do
  IO.catchIOError
    ( do
        handle <- IO.openFile filename IO.ReadMode
        str <- IO.hGetContents handle
        pure $ parse parser str
    )
    ( \e ->
        pure $ Left $ "Error:" ++ show e
    )
