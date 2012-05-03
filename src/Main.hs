
module Main
( main
) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, second)
import Control.Monad (forM_)
import Data.Char (isAlphaNum, isSpace)
import System.Environment (getArgs)
import Text.Printf (printf)

import Patricia.Actions
import Patricia.Formulae
import Patricia.Nets
import Patricia.Tools
import Patricia.Tools.Types
import Patricia.Parser
import Patricia.Processes

main :: IO ()
main = do
    params <- mkParameters <$> getArgs
    case params of
        Nothing -> printf "./patricia <net> <strategy>\n"
        Just (net', action) -> do
            problems <- (map parseLine . lines) <$> getContents
            forM_ problems $ \(name', formula') ->
                perform (Problem name' net' formula') action >>= print
            waitForChildren
  where
    mkParameters :: [String] -> Maybe (Net, Action)
    mkParameters args = case args of
        [net', action] -> (,) <$> toNet net' <*> (either2maybe tools (parse action))
        _              -> Nothing
    
    parseLine :: String -> (String, Formula)
    parseLine = first (filter isAlphaNum) . second (dropWhile isSpace . drop 1) . span (/= '=')
    
    either2maybe :: b -> Either a (b -> c) -> Maybe c
    either2maybe b = either (const Nothing) (Just . flip ($) b)
        