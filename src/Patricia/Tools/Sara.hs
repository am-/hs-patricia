
module Patricia.Tools.Sara
( sara
) where

import Data.List (isPrefixOf)
import System.Exit (ExitCode(..))
import System.FilePath (addExtension)
import System.Process (shell)
import Text.Printf (printf)

import Patricia.Nets
import Patricia.Tools.Types

sara :: Tool
sara = Tool
    { identifier = "sara"
    , prepare = \p@(Problem { name = name', net = net', formula = formula' }) -> shell $
        printf "echo \"%s = %s\" > %s.formula ; form2sara -n %s %s -f %s.formula > %s"
        name' formula' name' (fromNet net') (format net') name' (task4sara p)
    , run = shell . ("sara -i " ++) . task4sara
    , interpret = \(code, out, _) -> let out' = lines out in case code of
        ExitFailure _ -> Left Undefined
        ExitSuccess | any (isPrefixOf "sara: SOLUTION")   out' -> Right Verified
                    | any (isPrefixOf "sara: INFEASIBLE") out' -> Right Refuted
                    | otherwise -> Left Undefined
    }
  where
    task4sara :: Problem -> FilePath
    task4sara = flip addExtension "sara" . name
    
    format :: Net -> String
    format net' = case net' of
        Lola _ -> "-l"
        Owfn _ -> "-o"
        Pnml _ -> "-p"
