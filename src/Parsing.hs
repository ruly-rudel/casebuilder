module Parsing
    (
        Parser(..),
        Expr(..),
        apply, parse,
        getc, sat, fails, char, string, lower, digit, (<|>),
        none, lowers, space, symbol, token, some, many, optional,
        natural, int, indent, ident, parseCase, parseCode, parseLine, parseExpr
    ) where
import Data.Char (isLower, isDigit, isSpace, isAlpha, isAlphaNum)


newtype Parser a = Parser (String -> [(a, String)])

apply :: Parser a -> String -> [(a, String)]
apply (Parser p) = p

parse :: Parser a -> String -> a
parse p = fst. head . apply p

instance Functor Parser where
    fmap = undefined

instance Applicative Parser where
    pure x = Parser (\cs -> [(x, cs)])
    (<*>) = undefined

instance Monad Parser where
    p >>= q = Parser (\cs -> [(y, cs'') |
        (x, cs') <- apply p cs,
        (y, cs'') <- apply (q x) cs'])


getc :: Parser Char
getc = Parser f where
    f [] = []
    f (c:cs) = [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- getc; if p c then return c; else fails}

fails :: Parser a
fails = Parser (\_ -> [])

char :: Char -> Parser ()
char x = do {_ <- sat (==x); return ()}

string :: [Char] -> Parser ()
string [] = return ()
string (x:xs) = do {char x; string xs; return ()}

lower :: Parser Char
lower = sat isLower

digit :: Parser Int
digit = do {d <- sat isDigit; return (cvt d)} where
    cvt d = fromEnum d - fromEnum '0'

bit :: Parser Int
bit = do {d <- sat (\c -> c == '0' || c == '1' || c == '_'); return (cvt d)} where
    cvt d = if d == '_' then 2 else fromEnum d - fromEnum '0'

(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = Parser f where
     f s = let ps = apply p s in
        if null ps then apply q s
        else ps

none :: Parser [a]
none = return []

lowers :: Parser [Char]
lowers = many lower

space :: Parser ()
space = many (sat (== ' ')) >> return ()

symbol :: [Char] -> Parser ()
symbol xs = space >> string xs

token :: Parser b -> Parser b
token p = space >> p

some :: Parser a -> Parser [a]
some p = do { x <- p; xs <- many p; return (x:xs)}

many :: Parser a -> Parser [a]
many p = optional (some p)

optional :: Parser [a] -> Parser [a]
optional p = p <|> none


natural :: Parser Int
natural = token nat

nat :: Parser Int
nat = do {ds <- some digit; return (foldl1 shiftl ds)}
        where shiftl m n = 10 * m + n

int :: Parser Int
int = do {symbol "-"; n <- nat; return (-n)}
            <|> natural

indent :: Parser Int
indent = do { ss <- many (sat isSpace); return $ length ss}

ident :: Parser [Char]
ident = do { a <- sat isAlpha; as <- many (sat isAlphaNum); return $ a:as}



bits :: Parser Int
bits = do {ds <- some bit; return (foldl1 shiftl ds)}
        where shiftl m n = if n == 2 then m else 2 * m + n

tillCrlf :: Parser [Char]
tillCrlf = many (sat (/= '\n'))

crlf :: Parser Char
crlf = sat (== '\n')


data Expr = CaseSel Int String (Int, Int) String |
            Code Int String
            deriving (Show, Eq)

parseCase :: Parser Expr
parseCase = do
    sp <- indent
    name <- ident
    w <- natural
    string "'b"
    num <- bits
    space
    com <- tillCrlf
    _ <- crlf
    return $ CaseSel sp name (w, num) com

parseCaseNoComment :: Parser Expr
parseCaseNoComment = do
    sp <- indent
    name <- ident
    w <- natural
    string "'b"
    num <- bits
    space
    _ <- crlf
    return $ CaseSel sp name (w, num) ""


parseCode :: Parser Expr
parseCode = do
    sp <- indent
    code <- tillCrlf
    _ <- crlf
    return $ Code sp code

parseLine :: Parser Expr
parseLine = parseCase <|> parseCaseNoComment <|> parseCode

parseExpr :: Parser [Expr]
parseExpr = many parseLine

-- apply parseExpr "op 5'b11001 [32bit instructions]\n    default: id_we = 1'b0;\n"