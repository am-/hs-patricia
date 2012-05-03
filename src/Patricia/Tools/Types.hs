
module Patricia.Tools.Types
( Outcome
, Failure(..)
, Success(..)
, Problem(..)
, Tool(..)
) where

import Data.Function (on)
import System.Exit (ExitCode(..))
import System.Process (CreateProcess)

import Patricia.Nets
import Patricia.Formulae


type Outcome = Either Failure (String, Success)

data Failure = Timeout
             | Prepare
             | Undefined
             deriving (Show, Eq, Ord)

data Success = Verified
             | Refuted
             deriving (Show, Eq, Ord)



data Problem = Problem
    { name :: String
    , net :: Net
    , formula :: Formula
    } deriving (Show, Eq, Ord)



data Tool = Tool
          { identifier :: String
          , interpret :: (ExitCode, String, String) -> Either Failure Success
          , prepare :: Problem -> CreateProcess
          , run :: Problem -> CreateProcess
          }

instance Show Tool where
    show = identifier

instance Ord Tool where
    compare = compare `on` identifier

instance Eq Tool where
    (==) = (==) `on` identifier
