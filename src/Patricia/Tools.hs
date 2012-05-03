
module Patricia.Tools
( tools
) where

import Patricia.Tools.Types
import Patricia.Tools.Sara
import Patricia.Tools.Lola

tools :: [(String, Tool)]
tools = [ ("sara", sara)
        , ("statepredicate", statepredicate)
        , ("findpath", findpath)
        ]