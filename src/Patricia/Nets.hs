
module Patricia.Nets
( Net(..)
, fromNet
, toNet
) where

import Data.Char (toLower)
import System.FilePath (takeExtension)

data Net = Pnml FilePath
         | Lola FilePath
         | Owfn FilePath
         deriving (Show, Eq, Ord)

fromNet :: Net -> FilePath
fromNet n = case n of
    Pnml p -> p
    Lola p -> p
    Owfn p -> p

toNet :: FilePath -> Maybe Net
toNet path = case map toLower (takeExtension path) of
    ".lola" -> Just (Lola path)
    ".owfn" -> Just (Owfn path)
    ".pnml" -> Just (Pnml path)
    _       -> Nothing