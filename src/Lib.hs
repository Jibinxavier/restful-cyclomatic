
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
--{-# CPP #-}

-- | use-haskell
-- The purpose of this project is to provide a baseline demonstration of the use of cloudhaskell in the context of the
-- code complexity measurement individual programming project. The cloud haskell platform provides an elegant set of
-- features that support the construction of a wide variety of multi-node distributed systems commuinication
-- architectures. A simple message passing abstraction forms the basis of all communication.
--
-- This project provides a command line switch for starting the application in master or worker mode. It is implemented
-- using the work-pushing pattern described in http://www.well-typed.com/blog/71/. Comments below describe how it
-- operates. A docker-compose.yml file is provided that supports the launching of a master and set of workers.

module Lib
    ( someFunc
    ) where

-- These imports are required for Cloud Haskell
import        Control.Distributed.Process
import        Control.Distributed.Process.Backend.SimpleLocalnet
import        Control.Distributed.Process.Closure
import        Control.Distributed.Process.Node                   (initRemoteTable)
import        Control.Monad
import        Network.Transport.TCP                              (createTransport, defaultTCPParameters)
import        PrimeFactors
import        System.Environment                                 (getArgs)
import        System.Exit
import        Data.List
import        System.Directory                                  -- (doesDirectoryExist,getDirectoryContents)
import        System.Process
import        System.IO          
import        Data.List.Split       
import        System.FilePath
import        System.Posix.Files                  
-- this is the work we get workers to do. It could be anything we want. To keep things simple, we'll calculate the
-- number of prime factors for the integer passed.


import        qualified Data.ByteString as B
import        qualified Data.ByteString.Lazy as L

import        Codec.Archive.Zip (extractFiles, withArchive, entryNames)
import        Network.HTTP.Conduit
import        Network.URI (parseURI)
import        Argon
import        Data.Either  


import Data.Traversable (traverse)
import System.Directory.Tree (
    AnchoredDirTree(..), DirTree(..),
    filterDir, readDirectoryWith
    )
import System.FilePath (takeExtension)

test (DirTree m) = m
listFilesDirQFiltered :: String -> IO ([DirTree FilePath])
listFilesDirFiltered dir = do
  _:/tree <- readDirectoryWith return dir
  return $ traverse  (:[])  $ (filterDir myPred tree)
 
   
  --return ()
  where myPred (Dir ('.':_) _) = False
        myPred (File n _) = takeExtension n  == ".hs"
        myPred _ = True


traverseDir :: FilePath -> (FilePath -> Bool) -> IO [FilePath]
traverseDir top exclude = do
  ds <- getDirectoryContents top
  paths <- forM (filter (not.exclude) ds) $ \d -> do
    let path = top </> d
    s <- getFileStatus path
    if isDirectory s
      then traverseDir path exclude
      else return [path]
  return (concat paths)
cloneRepo :: String -> IO()
cloneRepo url = do 
  let repo = last $ splitOn "/" url
  exists <- liftIO $ doesDirectoryExist ("/tmp/" ++ repo)
  case exists of 
    False -> do 
      liftIO $ createProcess (shell $ "/usr/bin/git clone " ++ url ++ " /tmp/" ++ repo ){ std_out = CreatePipe }
      liftIO $ putStrLn "Cloning complete"
      
    otherwise -> do 
      liftIO $ putStrLn "Repository was already cloned"
 

calcCyclomat :: String -> IO (Float)
calcCyclomat filePath  = do 
  let config = (Config 6 [] [] [] Colored) 
     
  
  (_, output) <- analyze config filePath

  case output of 
    (Left _) -> return 0
    (Right results) ->
      let sum1= (sum (map (\(CC (_, _, x)) -> x) results) ) 
          len = length results
          avg = fromIntegral sum1/ fromIntegral len   
      in return avg
 
  --liftIO $ putStrLn $ "complexity" ++  show avg
  


downloadFile :: String -> IO (Bool)
downloadFile url  = do
    jpg <- get url
    L.writeFile "master.zip" jpg
    return True 
  where
    get url = case parseURI url of
                Nothing -> error $ "Invalid URI: " ++ url
                Just _ ->  simpleHttp url   
unzipF :: String -> String -> IO()
unzipF fname dirPath=  withArchive fname $ do
    names <- entryNames
    extractFiles names dirPath
    
doWork :: Integer -> Integer
doWork = numPrimeFactors

-- | worker function.
-- This is the function that is called to launch a worker. It loops forever, asking for work, reading its message queue
-- and sending the result of runnning numPrimeFactors on the message content (an integer).
worker :: ( ProcessId  -- The processid of the manager (where we send the results of our work)
         , ProcessId) -- the process id of the work queue (where we get our work from)
       -> Process ()
worker (manager, workQueue) = do
    us <- getSelfPid              -- get our process identifier
    liftIO $ putStrLn $ "Starting worker: " ++ show us
    go us
  where
    go :: ProcessId -> Process ()
    go us = do

      send workQueue us -- Ask the queue for work. Note that we send out process id so that a message can be sent to us

      -- Wait for work to arrive. We will either be sent a message with an integer value to use as input for processing,
      -- or else we will be sent (). If there is work, do it, otherwise terminate
      receiveWait
        [ match $ \n  -> do
            liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] given work: " ++ show n
            send manager (doWork n)
            liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] finished work."
            go us -- note the recursion this function is called again!
        , match $ \ () -> do
            liftIO $ putStrLn $ "Terminating node: " ++ show us
            return ()
        ]

remotable ['worker] -- this makes the worker function executable on a remote node

manager :: Integer    -- The number range we wish to generate work for (there will be n work packages)
        -> [NodeId]   -- The set of cloud haskell nodes we will initalise as workers
        -> Process Integer
manager n workers = do
  us <- getSelfPid

  -- first, we create a thread that generates the work tasks in response to workers
  -- requesting work.
  workQueue <- spawnLocal $ do
    -- Return the next bit of work to be done
    forM_ [1 .. n] $ \m -> do
      pid <- expect   -- await a message from a free worker asking for work
      send pid m     -- send them work

    -- Once all the work is done tell the workers to terminate. We do this by sending every worker who sends a message
    -- to us a null content: () . We do this only after we have distributed all the work in the forM_ loop above. Note
    -- the indentiation - this is part of the workQueue do block.
    forever $ do
      pid <- expect
      send pid ()

  -- Next, start worker processes on the given cloud haskell nodes. These will start
  -- asking for work from the workQueue thread immediately.
  forM_ workers $ \ nid -> spawn nid ($(mkClosure 'worker) (us, workQueue))
  liftIO $ putStrLn $ "[Manager] Workers spawned"
  -- wait for all the results from the workers and return the sum total. Look at the implementation, whcih is not simply
  -- summing integer values, but instead is expecting results from workers.
  sumIntegers (fromIntegral n)

-- note how this function works: initialised with n, the number range we started the program with, it calls itself
-- recursively, decrementing the integer passed until it finally returns the accumulated value in go:acc. Thus, it will
-- be called n times, consuming n messages from the message queue, corresponding to the n messages sent by workers to
-- the manager message queue.
sumIntegers :: Int -> Process Integer
sumIntegers = go 0
  where
    go :: Integer -> Int -> Process Integer
    go !acc 0 = return acc
    go !acc n = do
      m <- expect
      go (acc + m) (n - 1)

rtable :: RemoteTable
rtable = Lib.__remoteTable initRemoteTable

-- | This is the entrypoint for the program. We deal with program arguments and launch up the cloud haskell code from
-- here.
someFunc :: IO ()
someFunc = do
  --all <- getDirectoryContents "/home/jibin/workspace/new_Disributed/restful-cyclomatic" 
  liftIO $ cloneRepo "https://github.com/rubik/argon.git"
  l <-calcCyclomat "/tmp/argon.git/src/Argon/Parser.hs"
  putStrLn $ "Result " ++ show l 
  res <- traverseDir "/tmp/argon.git/" (not.isPrefixOf ".hs")
  --liftIO $ listFilesDirFiltered "/tmp/argon.git" -- (isPrefixOf ".hs")
  putStrLn $ "Result " ++ show res
  res <-  listFilesDirFiltered "/tmp/argon.git"
  let b = map (\(DirTree file) -> file) res
  putStrLn $ b
  print res
  do 
    unzipF "./master" "."
  args <- getArgs

  case args of
    ["manager", host, port, n] -> do
      putStrLn "Starting Node as Manager"
      backend <- initializeBackend host port rtable
      startMaster backend $ \workers -> do
        result <- manager (read n) workers
        liftIO $ print result
    ["worker", host, port] -> do
      putStrLn "Starting Node as Worker"
      backend <- initializeBackend host port rtable
      startSlave backend
    _ -> putStrLn "Bad parameters"


  -- create a cloudhaskell node, which must be initialised with a network transport
  -- Right transport <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  -- node <- newLocalNode transport initRemoteTable

  -- runProcess node $ do
  --   us <- getSelfNode
  --   _ <- spawnLocal $ sampleTask (1 :: Int, "using spawnLocal")
  --   pid <- spawn us $ $(mkClosure 'sampleTask) (1 :: Int, "using spawn")
  --   liftIO $ threadDelay 2000000