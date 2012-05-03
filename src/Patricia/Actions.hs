
module Patricia.Actions
( Action(..)
, perform
) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (ThreadId, myThreadId, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Exception (bracketOnError)
import System.Exit (ExitCode(..))

import Patricia.Tools.Types
import Patricia.Processes

data Action = Run Tool
            | RunWithTimeout Tool Int
            | TryThen Action Action
            | Parallel [Action]
            deriving (Show, Eq, Ord)

perform :: Problem -> Action -> IO Outcome
perform problem action = case action of
    Run t -> runWithoutTimeout problem t
    RunWithTimeout t n -> runWithTimeout problem t n
    TryThen a1 a2 -> tryThen problem a1 a2
    Parallel actions -> parallel problem actions

runWithoutTimeout :: Problem -> Tool -> IO Outcome
runWithoutTimeout p t = forkProc (prepare t p) >>= \(code, _, _) -> case code of
    ExitSuccess -> (either Left (Right . (,) (identifier t)) . interpret t) <$> forkProc (run t p)
    ExitFailure _ -> return (Left Prepare)

runWithTimeout :: Problem -> Tool -> Int -> IO Outcome
runWithTimeout p t timeout = do
    var <- newEmptyMVar
    bracketOnError
        ((,) <$> threadedPerform p (Run t) var <*> forkChild (guard var timeout))
        (mapM_ killThread . pair2list)
        (waitOnce var . pair2list)
  where
    guard :: MVar (ThreadId, Outcome) -> Int -> IO ()
    guard var n = threadDelay (n*1000) >> myThreadId >>= putMVar var . flip (,) (Left Timeout)
    
    pair2list :: (a, a) -> [a]
    pair2list (x1, x2) = [x1, x2]

tryThen :: Problem -> Action -> Action -> IO Outcome
tryThen p a1 a2 = perform p a1 >>= either (const $ perform p a2) (return . Right)

parallel :: Problem -> [Action] -> IO Outcome
parallel p actions = do
    var <- newEmptyMVar
    tids <- mapM (flip (threadedPerform p) var) actions
    wait var tids

threadedPerform :: Problem -> Action -> MVar (ThreadId, Outcome) -> IO ThreadId
threadedPerform p a = forkChild . (>>=) ((,) <$> myThreadId <*> perform p a) . putMVar

wait :: MVar (ThreadId, Outcome) -> [ThreadId] -> IO Outcome
wait var tids
    | not (null tids) = do
        (tid, outcome) <- takeMVar var
        let tids' = filter (tid /=) tids
        either (const $ wait var tids') ((>>) (mapM_ killThread tids') . return . Right) outcome
    | otherwise = return (Left Undefined)

waitOnce :: MVar (ThreadId, Outcome) -> [ThreadId] -> IO Outcome
waitOnce var tids
    | not (null tids) = do
        (tid, outcome) <- takeMVar var
        mapM_ killThread (filter (tid /=) tids)
        return outcome
    | otherwise = return (Left Undefined)