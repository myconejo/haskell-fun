import System.IO
    -- for hSetBuffering
import Control.Concurrent
    -- for myThreadId 
-- Mvar: mutable locations in memory that are shared among threads
-- Mvar is mutable, where its value can be edited
-- Chan: FIFO queue of Mvars 

getTID :: IO String
getTID = do
    tid <- myThreadId               --a takes the current executing thread
    let x1 = "TID: " ++ show tid    -- builds a string
    return $! x1                    -- forcely evaluate getTID, using $!

threadID :: MVar () -> Chan () ->  IO ()
-- mutex: to block threads for safe computation
threadID mutex endFlags = do

    tid <- getTID
    -- tid :: String, "TID: ThreadID __"
    -- acquire lock the stdout
    -------------------------------------
    takeMVar mutex
    -- take and block MVar 'mutex' (similar to P(s) of the semaphore)
    putStrLn tid
    -- print the String tid
    putMVar mutex ()
    -- put MVar back as an empty tuple and let other threads to use
    -------------------------------------
    writeChan endFlags ()
    -- as the thread finishes, update the endFlags with ().
    -- when the endFlags are filled with all threads, program terminates

-- main: spawn threads 'threadID'
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let n = 10
    mutex <- newEmptyMVar
    -- mutex: new empty semaphore
    endFlags <- newChan
    -- endFlags: channel of end flags (Flag that current thread is finished)
    mapM_ (const $ forkIO $ threadID mutex endFlags) [1..n]
    -- mapM_: maps a list to a IO actions
    -- map 1 into (forkIO), map 2 into (forkIO), ... creating total 10 threads. 
    -- as the mutex is empty, all threads are blocked!

    putMVar mutex ()
    -- put an empty tuple into mutex, so any first thread can take the mutex
    -- when the 1st thread takes the mutex, it blocks the value.

    -- make main wait for threads
    mapM_ (const (readChan endFlags)) [1..n]
    -- map [1..10] to const((readChan endFlags),a[i]) and return endFlags to IO
    -- when the endFlags are filled with all threads, program terminates





