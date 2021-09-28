{-# LANGUAGE FlexibleInstances, UndecidableInstances, IncoherentInstances #-}

module ParserCombinator where

import Parser (Parser(parse), anyOf, char, isMatch)


(|||) :: Parser a -> Parser a -> Parser a
(|||) p1 p2 = anyOf [p1, p2]

times :: Parser a -> Integer -> Parser [a]
times parser n = sequence $ parser <$ [1 .. n]

anyChar :: [Char] -> Parser Char
anyChar xs = anyOf $ isMatch char <$> xs


character :: Parser Char
character = char

characters :: Integer -> Parser [Char]
characters = times char

numeric :: Parser Char
numeric = anyChar ['0' .. '9']

numerics :: Integer -> Parser [Char]
numerics = times numeric

upper :: Parser Char
upper = anyChar ['A' .. 'Z']

uppers :: Integer -> Parser [Char]
uppers = times upper

lower :: Parser Char
lower = anyChar ['a' .. 'z']

lowers :: Integer -> Parser [Char]
lowers = times lower

alpha =  upper ||| lower
alpha :: Parser Char

alphas :: Integer -> Parser [Char]
alphas = times alpha

alphaNum = alpha ||| numeric
alphaNum :: Parser Char

alphaNums :: Integer -> Parser [Char]
alphaNums = times alphaNum

spaces :: Parser ()
spaces = (is ' ' >> spaces) ||| pure ()

digit :: (Num a, Read a, Show a) => Parser a
digit = read . (:[]) <$> numeric

digits :: (Num a, Read a, Show a) => Integer -> Parser a
digits n = collapse <$> times digit n where
  collapse ns = read $ foldl (\x y -> x ++ show y) "" ns


class IsMatch a where
  is :: a -> Parser a

instance IsMatch Char where
  is = isMatch char

instance IsMatch String where
  is = traverse (isMatch char)

instance (Num a, Read a, Show a) => IsMatch a where
  is n = read <$> (traverse (isMatch char) . show) n


myParser = do n <- digits 2
              lowers 3
              is 75
              pure n

result = parse myParser "56abc75"
