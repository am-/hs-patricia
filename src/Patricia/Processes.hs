
module Patricia.Processes
( runProc
, forkProc
, startProc
, readProc
, forkChild
, waitForChildren
) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, newEmptyMVar, takeMVar, putMVar)
import Control.Exception (finally, bracketOnError)
import Control.Monad ((>=>))
import Data.Maybe (fromJust)
import System.Exit (ExitCode(..))
import System.IO (Handle, hGetContents)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess, ProcessHandle, StdStream(..))
import System.Process (createProcess, waitForProcess, terminateProcess, std_out, std_err)

children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = takeMVar children >>= \cs -> case cs of
    [] -> return ()
    (m:ms) -> do
        putMVar children ms
        takeMVar m
        waitForChildren

forkChild :: IO () -> IO ThreadId
forkChild io = do
    var <- newEmptyMVar
    vars <- takeMVar children
    putMVar children (var:vars)
    forkIO (io `finally` putMVar var ())

startProc :: CreateProcess -> IO (Handle, Handle, ProcessHandle)
startProc proc = extract <$> createProcess proc { std_out = CreatePipe, std_err = CreatePipe }
  where                                    
    extract :: (a, Maybe Handle, Maybe Handle, ProcessHandle) -> (Handle, Handle, ProcessHandle)
    extract (_, mout, merr, handle) = (fromJust mout, fromJust merr, handle)

readProc :: (Handle, Handle, ProcessHandle) -> IO (ExitCode, String, String)
readProc (out, err, handle) =
    (,,) <$> waitForProcess handle <*> hGetContents out <*> hGetContents err

terminateProc :: (Handle, Handle, ProcessHandle) -> IO ()
terminateProc = terminateProcess . third
  where
    third :: (a, b, c) -> c
    third (_, _, c) = c

forkProc :: CreateProcess -> IO (ExitCode, String, String)
forkProc p = bracketOnError (startProc p) terminateProc $ \proc -> do
    var <- newEmptyMVar
    forkChild (readProc proc >>= putMVar var)
    takeMVar var

runProc :: CreateProcess -> IO (ExitCode, String, String)
runProc = startProc >=> readProc
