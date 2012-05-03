
module Patricia.Tools.Lola
( findpath
, statepredicate
) where

import System.Exit (ExitCode(..))
import System.FilePath (addExtension)
import System.Process (CreateProcess, shell)
import Text.Printf (printf)

import Patricia.Nets
import Patricia.Tools.Types

lola :: Tool
lola = Tool
    { identifier = undefined
    , prepare = prepare4lola
    , run = undefined
    , interpret = interpret4lola
    }

statepredicate :: Tool
statepredicate = lola
    { identifier = "lola-statepredicate"
    , run = \p@(Problem { net = net' }) -> shell $
        printf "lola-statepredicate %s -a %s" (fromNet net') (task4lola p)
    }

findpath :: Tool
findpath = lola
    { identifier = "lola-findpath"
    , run = \p@(Problem { net = net' }) -> shell $
        printf "lola-findpath %s -a %s" (fromNet net') (task4lola p)
    }

prepare4lola :: Problem -> CreateProcess
prepare4lola p = shell $ case net p of
    Owfn _ -> "echo \"lola-statepredicate does not support OWFN.\""
    Pnml _ -> "echo \"lola-statepredicate does not support PNML.\""
    _         -> printf "echo \"%s = %s\" | form" (name p) (formula p)

interpret4lola :: (ExitCode, String, String) -> Either Failure Success
interpret4lola (code, _, _) = case code of
    ExitSuccess -> Right Verified
    ExitFailure n | n == 1 -> Right Refuted
                  | otherwise -> Left Undefined

task4lola :: Problem -> FilePath
task4lola = flip addExtension "task" . name

