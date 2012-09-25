module ReadPUtils(
r, r_eof,

	Text.Read.Read, ReadPrec,
	readPrec, 
	prec, step,

	parens, (+++), (<++), choice, lchoice, option, reset, between, 

	many, many1, 
	sepBy, sepBy1,

	keyword, keywords, operator, symbol, char, skipSpaces, eof
) where

import Text.Read
import Text.Read.Lex
--import Text.ParserCombinators.ReadPrec
import qualified Text.ParserCombinators.ReadP as ReadP

import Data.Char

import Control.Monad
import Control.Applicative hiding (many)

instance Applicative ReadPrec where
	pure  = return
	(<*>) = ap

r p str = do
	ls <- return (readPrec_to_S (p <* eof) 0 str)
	putStrLn (show (length ls) ++ " result(s):")
	mapM_ (\(a,b) -> putStr ("  " ++ show a ++ "\t") >> print b) ls

r_eof p str = do
	ls <- return (readPrec_to_S p 0 str)
	putStrLn (show (length ls) ++ " result(s):")
	mapM_ (\(a,b) -> putStrLn ("  " ++ show a)) ls

foo = sepBy1 symbol (char '|') 

char c = do
	a <- skipSpaces >> get
	guard (a == c)
	return a

keyword kw = do
	sym <- symbol
	guard (map toLower sym == map toLower kw)
	return sym

keywords = mapM keyword

operator = do
	token <- lexP
	case token of
		(Punc   sym) -> return sym
		_            -> fail "Not an operator"

symbol = do
	token <- lexP
	case token of
		(Char   c)   -> return [c]
		(String str) -> return str
		(Ident  sym) -> return sym
--		(Punc   sym) -> return sym
		_            -> fail "Not a symbol"

many :: ReadPrec a -> ReadPrec [a]
-- ^ Parses zero or more occurrences of the given parser.
many p = return [] +++ many1 p

many1 :: ReadPrec a -> ReadPrec [a]
-- ^ Parses one or more occurrences of the given parser.
many1 p = liftM2 (:) p (many p)

sepBy :: ReadPrec a -> ReadPrec sep -> ReadPrec [a]
-- ^ @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy p sep = sepBy1 p sep +++ return []

sepBy1 :: ReadPrec a -> ReadPrec sep -> ReadPrec [a]
-- ^ @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

skipSpaces :: ReadPrec ()
-- ^ Skips all whitespace.
skipSpaces =
  do s <- look
     skip s
 where
  skip (c:s) | isSpace c = do _ <- get; skip s
  skip _                 = do return ()

eof = do
	cs <- look
	case cs of
		[] -> return ()
		_  -> fail ""

-- * left biased choice, returns first success 
lchoice ps = foldr (<++) pfail ps

option x p = p +++ return x

between open close p = do
	open
	a <- p
	close 
	return a

