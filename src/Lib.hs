
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
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
import        DatabaseConnector
import        System.Environment                                 (getArgs,lookupEnv)
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


import qualified Pipes.Prelude as P
import Pipes
import Pipes.Safe (runSafeT)
import System.IO.Silently

import        qualified Data.ByteString as B
import        qualified Data.ByteString.Lazy as L 

import        Codec.Archive.Zip (extractFiles, withArchive, entryNames)
-- import        Network.HTTP.Conduit
import        Network.URI (parseURI)
import        Argon

import Data.Traversable (traverse)
import System.Directory.Tree (
    AnchoredDirTree(..), DirTree(..),
    filterDir, readDirectoryWith, zipPaths, flattenDir
    )
import System.FilePath (takeExtension)


wait filepath = do 

  exists <- liftIO $ doesDirectoryExist filepath
  if exists then return ()
  else wait filepath

listFilesDirFiltered :: String -> Process ([DirTree FilePath])
listFilesDirFiltered dir = do
  _:/tree <- liftIO $ readDirectoryWith return dir
  let res = flattenDir (filterDir myPred tree)
      m = [ x | x@(File _ _) <- res ]
  return  m   -- removing all Dir data types as they are empty 
  where myPred (Dir ('.':_) _) = False
        myPred (File n _) = takeExtension n  == ".hs"
        myPred _ = True

cloneRepo :: String -> IO(String)
cloneRepo url = do 
  let repo = last $ splitOn "/" url
      folder = "/tmp/" ++ repo  
  exists <- liftIO $ doesDirectoryExist ("/tmp/" ++ repo)
  case exists of 
    False -> do 
      liftIO $ createProcess (shell $ "/usr/bin/git clone " ++ url ++ " /tmp/" ++ repo ){ std_out = CreatePipe }
      liftIO $ putStrLn "Cloning complete"
      return $ folder
      
    otherwise -> do 
      liftIO $ putStrLn "Repository was already cloned"
      return $ folder

gitCheckout :: String -> String -> Process()
gitCheckout folder commit = do
  liftIO $! putStrLn $ commit
  liftIO $ wait folder
  liftIO $ createProcess (shell $ "echo here && cd "++ folder ++ "&&" ++"/usr/bin/git checkout " ++ commit ){ std_out = CreatePipe }
  return ()
getAllCommits :: String -> Process([String])
getAllCommits url = do   
  let repo = last $ splitOn "/" url
      folder = "/tmp/" ++ repo 
      cloneCmd = "/usr/bin/git clone " ++ url ++ " "++folder ++ "&& echo end"
      commitsCmd = " cd "++ folder ++ "&&" ++"/usr/bin/git rev-list HEAD" 
  exists <- liftIO $ doesDirectoryExist folder
  case exists of 
    True -> do 
      liftIO $ putStrLn $ "cloning complete getting commits"  ++  show folder
      (_, Just hout, _, _) <- liftIO $ createProcess (shell $ commitsCmd ){ std_out = CreatePipe }
      commits <- liftIO $ hGetContents hout
      return $ filter (""/=) (splitOn "\n" commits)
     
    False -> do 
      liftIO $ putStrLn $ "Cloning" ++  show folder
      (_, Just hout, _, _) <-liftIO $ createProcess (shell $ cloneCmd ++ "&&" ++ commitsCmd){ std_out = CreatePipe } 
 
      commits <- liftIO $ hGetContents hout
       
      return $ filter (""/=) (splitOn "\n" $ (last (splitOn "end" commits)))  
     
      
 
  
 

calcCyclomat :: String -> Process (Integer)
calcCyclomat fpath  = do 
  let config = (Config 1 [] [] [] Colored) 
 
  
  (_, output) <-  liftIO $ analyze config fpath

  case output of 
    (Left _) -> return 0
    (Right results) -> do
      let sum1= (sum (map (\(CC (_, _, x)) -> x) results) ) 
          len = length results
          avg = fromIntegral sum1/ fromIntegral len   

      
      liftIO $ putStrLn $ "file path "++ fpath ++ "Sum " ++ show sum1
      return $ round avg
 
  --liftIO $ putStrLn $ "complexity" ++  show avg
  

 
 
doWork :: (String, String, String, String) ->  Process (Integer)
doWork (commitId, fpath, _, url) = do
  folder <- liftIO $ cloneRepo url 
  liftIO $ putStrLn "folder cloned"  
  gitCheckout folder commitId 
  let config = (Config 1 [] [] [] Colored) 
  
   
  (_, output) <-  liftIO $ analyze config fpath

  case output of 
    (Left _) -> return 0
    (Right results) -> do
      let sum1= (sum (map (\(CC (_, _, x)) -> x) results) ) 
          len = length results
          avg = fromIntegral sum1/ fromIntegral len   

      
      liftIO $ putStrLn $ "file path "++ fpath ++ "Sum " ++ show sum1
      return $ fromIntegral sum1
 

-- | worker function.
-- This is the function that is called to launch a worker. It loops forever, asking for work, reading its message queue
-- and sending the result of runnning numPrimeFactors on the message content (an integer).
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
          liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] given work: " -- ++ show n
          res <- (doWork n)

          send manager res
          liftIO $ putStrLn $ "[Node " ++ (show us) ++ "] finished work. result " ++ show res
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
  let url = "https://github.com/rubik/argon.git"
      folder = "/tmp/argon.git" 
  allCommits <- getAllCommits url 
  let number = length allCommits 
  liftIO $ putStrLn $ "number of commits " ++ show number

  workQueue <- spawnLocal $ do
    -- Return the next bit of work to be done
    

    forM_ (allCommits) $ \commitId -> do
   
      --gitCheckout folder  commitId

      files <- listFilesDirFiltered folder
      forM_ files $ \f -> do
        let (File _ fpath) = f
        pid <- expect   -- await a message from a free worker asking for work
        send pid $ (commitId, fpath, folder, url)    -- send them work
       
     
    

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
  sumIntegers (fromIntegral number)

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

