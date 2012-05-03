{
module Patricia.Parser (parse) where

import Control.Applicative (liftA2)
import Data.Char (isDigit, isAlpha, isSpace)
import Data.Maybe (fromMaybe)

import Patricia.Actions (Action(..))
import Patricia.Tools.Types (Tool)
}

%name action
%tokentype { Token }
%error { parseError }
%monad { Either String }

%token
      '|>'    { TokenTryThen }
      '<|>'   { TokenParallel }
      '('     { TokenParOpen }
      ')'     { TokenParClose }
      name    { TokenName $$ }
      int     { TokenInt $$ }

%%

Act0 : Act0 '<|>' Act1      { liftA2 parallelize $1 $3 }
     | Act1                 { $1 }

Act1 : Act2 '|>' Act1       { liftA2 TryThen $1 $3 }
     | Act2                 { $1 }

Act2 : name '(' int ')'     { flip RunWithTimeout $3 . name2tool $1 }
     | name                 { Run . name2tool $1 }
     | '(' Act0 ')'         { $2 }


{

data Token = TokenTryThen
           | TokenParallel
           | TokenParOpen
           | TokenParClose
           | TokenName String
           | TokenInt Int
           deriving (Show, Eq, Ord)


lexer :: String -> [Token]
lexer str = case str of
    [] -> []
    '(':cs -> TokenParOpen : lexer cs
    ')':cs -> TokenParClose : lexer cs
    '|':('>':cs) -> TokenTryThen : lexer cs
    '<':('|':('>':cs)) -> TokenParallel : lexer cs
    c:cs | isSpace c -> lexer cs
         | isAlpha c -> let (string,rest) = span isAlpha (c:cs)
                        in  TokenName string : lexer rest
         | isDigit c -> let (num,rest) = span isDigit (c:cs)
                        in  TokenInt (read num) : lexer rest

name2tool :: String -> [(String, Tool)] -> Tool
name2tool name = fromMaybe (error ("Unknown tool " ++ name)) . lookup name


parallelize :: Action -> Action -> Action
parallelize a1 a2 = case a1 of
    Parallel actions -> Parallel (actions ++ [a2])
    _                -> Parallel [a1,a2]

parseError :: [Token] -> Either String a
parseError _ = Left "Parse Error!"

parse :: String -> Either String ([(String, Tool)] -> Action)
parse = action . lexer

}
