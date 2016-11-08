module Shunty
    ( parse
    , tokenize
    , shuntingYard
    , Token(..)
    , Operator(..)
    , Bracket(..)
    ) where

import           Data.Char (isDigit)

data Token = Value Integer | Op Operator | Bracket Bracket | Noop
  deriving (Show, Eq)

data Operator = Plus | Minus | Multiply | Divide
  deriving (Show, Eq)

precedence :: Token -> Integer
precedence (Op Plus)     = 4
precedence (Op Minus)    = 4
precedence (Op Multiply) = 3
precedence (Op Divide)   = 3
precedence _             = 9999

data Bracket = Open | Close
  deriving (Show, Eq)

parse :: String -> [Token]
parse expr =
    shuntingYard tokens [] []
  where
    tokens = tokenize expr

shuntingYard :: [Token] -> [Token] -> [Token] -> [Token]
shuntingYard [] [] output        = output
shuntingYard [] stack output     = output ++ stack
shuntingYard (Value v : ts) stack output =
    shuntingYard ts stack (output ++ [Value v])
shuntingYard (Op op : ts) [] output =
    shuntingYard ts [Op op] output
shuntingYard (Op op : ts) stack output =
  let
    operatorsToOutput =
        takeWhile (\x -> precedence x < precedence (Op op)) stack

    remainingStack = length operatorsToOutput `drop` stack
  in
    shuntingYard ts (Op op : remainingStack) (output ++ operatorsToOutput)
shuntingYard (Bracket Open : ts) stack output =
    shuntingYard ts (Bracket Open : stack) output
shuntingYard (Bracket Close : ts) stack output =
  let
    operatorsToOutput = takeWhile (\x -> x /= Bracket Open) stack

    -- Drop bracket as well
    remainingStack = (length operatorsToOutput + 1) `drop` stack
  in
    shuntingYard ts remainingStack (output ++ operatorsToOutput)

shuntingYard _ _ _ = undefined


tokenize :: String -> [Token]
tokenize s =
    loop s []
  where
      loop :: String -> [Token] -> [Token]
      loop "" acumulator = acumulator
      loop (c:cs) acumulator
        | c == ' ' = loop cs acumulator
        | isDigit c =
          let
            tokenString = c : takeWhile isDigit cs
            token = read tokenString :: Integer
            remainingString = drop (length tokenString - 1) cs
          in
            loop remainingString (acumulator ++ [Value token])
        | isOperator c =
          let
            parseOperator c
              | c == '+' = Op Plus
              | c == '-' = Op Minus
              | c == '*' = Op Multiply
              | c == '/' = Op Divide
              | otherwise = Noop
          in
            loop cs (acumulator ++ [parseOperator c])
        | isBracket c =
          let
            parseBracket c
              | c == '(' = Bracket Open
              | c == ')' = Bracket Close
              | otherwise = Noop
          in
            loop cs (acumulator ++ [parseBracket c])
        | otherwise = loop cs acumulator


      isOperator :: Char -> Bool
      isOperator c
        | c == '+' || c == '*' || c == '-' || c == '/' = True
        | otherwise = False

      isBracket :: Char -> Bool
      isBracket c
        | c == '(' || c == ')' = True
        | otherwise = False
