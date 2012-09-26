module ReadPUtils(
r, r_eof,
	-- our own parser combinators
	keyword, keywords, operator, symbol, char, skipSpaces, eof,

	many, many1, 
	sepBy, sepBy1,

	parens, optionalParens,

	-- export parser combinators from Text.Read
	Text.Read.Read, ReadPrec,
	readPrec, 
	prec, step,

	(+++), (<++), choice, option, reset, between
) where

import Text.Read hiding (parens)
import Text.Read.Lex

import qualified Text.Read

import Data.Char

import Control.Monad
import Control.Applicative hiding (many)

instance Applicative ReadPrec where
	pure  = return
	(<*>) = ap

-- ***** USEFUL PARSERS AND PARSER COMBINATORS *************************************************************************

-- ^ checks if the next char in input is equal to a given char (skips leading whitespace)
char c = do
	a <- skipSpaces >> get
	guard (a == c)
	return a

-- ^ checks if the next word or string litereal in input is equal to a given string (skips leading whitespace)
keyword kw = do
	sym <- symbol
	guard (map toLower sym == map toLower kw)
	return sym

-- ^ checks if the next words/strings in input are equal to a given phrase (skips leading whitespace)
keywords = mapM keyword

-- ^ checks if the next operator symbol in input is equal to a given string (skips leading whitespace)
operator op = do
	token <- lexP
	guard (token == Punc op)
	return token

-- ^ parses a word, string literal or char literal
symbol = do
	token <- lexP
	case token of
		(Char   c)   -> return [c]
		(String str) -> return str
		(Ident  sym) -> return sym
		_            -> fail "Not a symbol"

-- ^ Parses zero or more occurrences of the given parser.
many :: ReadPrec a -> ReadPrec [a]
many p = return [] +++ many1 p

-- ^ Parses one or more occurrences of the given parser.
many1 :: ReadPrec a -> ReadPrec [a]
many1 p = liftM2 (:) p (many p)

-- ^ @sepBy p sep@ parses zero or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy :: ReadPrec a -> ReadPrec sep -> ReadPrec [a]
sepBy p sep = sepBy1 p sep +++ return []

-- ^ @sepBy1 p sep@ parses one or more occurrences of @p@, separated by @sep@.
--   Returns a list of values returned by @p@.
sepBy1 :: ReadPrec a -> ReadPrec sep -> ReadPrec [a]
sepBy1 p sep = liftM2 (:) p (many (sep >> p))

-- ^ Skips all whitespace.
skipSpaces :: ReadPrec ()
skipSpaces = look >>= skip
	where
		skip (c:s) | isSpace c = do _ <- get; skip s
		skip _                 = do return ()

-- ^ succeeds if input is empty, fails otherwise
eof = do
	cs <- look
	case cs of
		[] -> return ()
		_  -> fail ""

-- ^ Parses what given parser does but enclosed in parentheses
parens         p = between (char '(') (char ')') p

-- ^ Parses what given parser does but optionally enclosed in parentheses
optionalParens p = Text.Read.parens              p

-- ^ tries to parse with parser, if it fails returns a given default value
option :: a -> ReadPrec a -> ReadPrec a
option x p = p +++ return x

-- ^ parses what a given parser parser parses, 
--   enclosed in what the 'open' and 'close' parsers parse
between open close p = do
	open
	a <- p
	close 
	return a

-- ***** TESTING PARSERS ***********************************************************************************************

-- ^ try parser and print all results
r p str = do
	ls <- return (readPrec_to_S (p <* eof) 0 str)
	putStrLn (show (length ls) ++ " result(s):")
	mapM_ (\(a,b) -> putStr ("  " ++ show a ++ "\t") >> print b) ls

-- ^ try parser followed by EOF parser, and print all results
r_eof p str = do
	ls <- return (readPrec_to_S p 0 str)
	putStrLn (show (length ls) ++ " result(s):")
	mapM_ (\(a,b) -> putStrLn ("  " ++ show a)) ls

