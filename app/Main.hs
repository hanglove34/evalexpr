module Main where

import Lib

import System.Environment
--import Control.Alternative
import Control.Monad
import Control.Applicative
import Data.Functor
import Data.Either
import System.Exit
import Text.Printf

-- main :: IO ()
-- main = do
--         args <- getArgs
--         case args of
--              [calc]  -> eval_expr calc
--              otherwise -> putStrLn("usage")

-- divideNumber :: Double -> Double -> Double
-- divideNumber n1 n2  = n1 / n2

-- addNumber :: Double -> Double -> Double
-- addNumber n1 n2  = n1 / n2

-- subNumber :: Double -> Double -> Double
-- subNumber n1 n2  = n1 / n2

-- mulNumber :: Double -> Double -> Double
-- mulNumber n1 n2  = n1 / n2

powerNumber :: Double -> Double -> Double -> Double
powerNumber num mult pow
    | pow == 0 = 1
    | pow == 1 = num
    | pow > 1 = powerNumber (num*mult) (mult) (pow-1)
    | pow < 0 = 1 / (powerNumber (num*mult) (mult) (-pow))

-- eval_expr :: String -> IO()
-- eval_expr s = do
--               let result = powerNumber (25) (25) 6
--               print result

main :: IO ()
main = do
    args <- getArgs
    case args of
      [string]  -> mainfunc string
      otherwise -> exitWith  (ExitFailure 84)


        -- print $ runParser (removeTabandSpaces (head args))
    -- case (res) of
    --     Left str -> print str
    --     Right (xs, s) -> print(xs, s)

mainfunc :: String -> IO()
mainfunc s = do
  when (null s) $ exitWith (ExitFailure 84)
  let cleaned = (removeTabandSpaces s)
  let test_illegal = (removeIllegalChar cleaned)
  when (test_illegal /= cleaned) $ exitWith (ExitFailure 84)
  let lastchar = (last cleaned)
  when (lastchar == '+' || lastchar == '-' || lastchar == '*' || lastchar == '/' || lastchar == '^') $ exitWith  (ExitFailure 84)
  let result = (runParser cleaned)
  if isInfinite result
    then exitWith  (ExitFailure 84)
    else printf "%.2f\n" result

removeTabandSpaces :: String -> String
removeTabandSpaces str = [ x | x <- str, not (x `elem` " \t") ]

removeIllegalChar :: String -> String
removeIllegalChar str = [ x | x <- str, (x `elem` "0123456789.+-*/^()") ]

data Expr = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Num Double

eval :: Expr -> Double
eval exp = case (exp) of
    (Add d1 d2) -> eval d1 + eval d2
    (Sub d1 d2) -> eval d1 - eval d2
    (Mul d1 d2) -> eval d1 * eval d2
    (Div d1 d2) -> eval d1 / eval d2
    (Pow d1 d2) -> powerNumber (eval d1) (eval d1) (eval d2)
    (Num d1) -> d1

newtype Parser a = Parser {runEvalExpr :: String -> Either String (String, a)}

pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy veriChar = Parser fun
    where
        fun [] = Left "Error empty string"
        fun (x:xs)
            | veriChar x = Right (xs, x)
            | otherwise = Left "Error bad caractere"

pChar :: Char -> Parser Char
pChar c = pSatisfy (== c)

pDigit :: Parser Char
pDigit = pSatisfy (\c -> elem c "1234567890.")

pDigits :: Parser String
pDigits = some pDigit

pDouble :: Parser Double
pDouble = fmap read pDigits

pNum :: Parser Expr
pNum = fmap Num pDouble

-- pAdd :: Parser Expr
-- pAdd = do
--     lv <- pMult
--     op <- pChar '+' <|> pChar '-'
--     rv <- pMult
--     case op of
--         '+' -> return (Add lv rv)
--         '-' -> return (Sub lv rv)
--     <|>
--     pMult

-- pMult :: Parser Expr
-- pMult = do
--     lv <- pPow
--     op <- pChar '*' <|> pChar '/'
--     rv <- pPow
--     case op of
--         '*' -> return (Mul lv rv)
--         '/' -> return (Div lv rv)
--     <|>
--     pPow

-- pPow :: Parser Expr
-- pPow = do
--     lv <- pNum
--     op <- pChar '^'
--     rv <- pNum
--     case op of
--         '^' -> return (Pow lv rv)
--     <|>
--     (pNum <|> (pChar '(' *> pExpr <* pChar ')'))

-- pExpr :: Parser Expr
-- pExpr = pAdd <|> pMult

pFactor :: Parser Expr
pFactor = do
    pNum
    <|>
    pChar '(' *> pExpr <* pChar ')'

pPow :: Parser Expr
pPow = do
    lv <- pFactor
    op <- pChar '^'
    rv <- pPow
    case (op) of
        '^' -> return (Pow lv rv)
    <|>
    pFactor

pTerm :: Parser Expr
pTerm = do
    lv <- pPow
    op <- pChar '*' <|> pChar '/'
    rv <- pTerm
    case (op) of
        '*' -> return (Mul lv rv)
        '/' -> return (Div lv rv)
    <|>
    pPow

pExpr :: Parser Expr
pExpr = do
    lv <- pTerm
    op <- pChar '+' <|> pChar '-'
    rv <- pExpr
    case (op) of
        '+' -> return (Add lv rv)
        '-' -> return (Sub lv rv)
    <|>
    pTerm

evalExpr :: String -> Double
evalExpr str = eval fun
    where
        fun = case (runEvalExpr (pExpr) str) of
            Right (str, a) -> a

pBind :: Parser a -> (a -> Parser b) -> Parser b
pBind pa apb = Parser fun
        where
            fun str = case (runEvalExpr pa str) of
                Left msg -> Left msg
                Right (str', a) -> runEvalExpr (apb a) str'

pBack :: a -> Parser a
pBack a = Parser fun
        where
            fun str = Right (str, a)

runParser :: String -> Double
runParser str = evalExpr str

pSequential :: Parser(a -> b) -> Parser a -> Parser b
pSequential pab pa = Parser fun
        where
            fun str = case (runEvalExpr pab str) of
                Left msg -> Left msg
                Right (str', ab) -> case (runEvalExpr pa str') of
                    Left msg -> Left msg
                    Right (str'', a) -> Right (str'', ab a)

pFmap :: (a -> b) -> Parser a -> Parser b
pFmap fab pa = Parser fun
    where
        fun str = case (runEvalExpr pa str) of
            Left msg -> Left msg
            Right (str', a) -> Right (str',(fab a))

pEmpty :: Parser a
pEmpty = Parser fun
    where
        fun str = Left "Data empty"

pOr :: Parser a -> Parser a -> Parser a
pOr pa1 pa2 = Parser fun
    where
        fun str = case (runEvalExpr pa1 str) of
            Left msg -> case (runEvalExpr pa2 str) of
                Left msg -> Left msg
                Right (str'', a') -> Right (str'', a')
            Right (str', a) -> Right (str', a)

instance Functor Parser where
    fmap = pFmap

instance Applicative Parser where
    pure = return
    (<*>) = pSequential

instance Alternative Parser where
    empty = pEmpty
    (<|>) = pOr

instance Monad Parser where
    return = pBack
    (>>=) = pBind
