{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram 

    where

import Control.Exception
import Control.Monad.State    
import Data.Aeson
import Network.HTTP.Simple
import System.Exit
import Control.Concurrent (threadDelay)

import Bot
import Config
import Drop
import Log
import MapR
import Telegram.Data
import Telegram.Internal


import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M
import qualified Data.Text as T


newHandle :: Config -> Log.Handle-> IO Bot.Handle
newHandle conf handl = do
    return $ Bot.Handle
        { config               = conf
        , handlerLog           = handl
        , initSession          = initSessionTl
        , getUpdates           = getUpdatesTl
        , copyMessages         = copyMessagesTl
        , sendMessagesWithKb   = sendMessagesWithKbTl
        , sendMessagesWithHelp = sendMessagesWithHelpTl
        , newDict              = newDictTl  
        }
        where
          logLevel = сonfigLogg conf
          debM = (debugM handl) logLevel
          infM = (infoM handl) logLevel
          warM = (warnM handl) logLevel
          errM = (errorM handl) logLevel

          initSessionTl handle = do
            loopBot handle (Session "" "" "0") M.empty "0"
            return ()    

          getUpdatesTl handle sess ts = do  
            let titleM  = "-- Telegram.getUpdates" 
                handlog = handlerLog handle
            resEither <- try (httpLBS  $ eventBuildRequest sess conf ts)
            res' <- testException resEither handle
            res  <- messageOK res' conf handlog 
            let upds = listUpd ((decode $ getResponseBody res))
            debM titleM (" List of Updates received = \n " ++ (show upds))
            return (map Tl upds, newoffs upds)

          copyMessagesTl upds dict = mapM_ copyMessage $ forCopy upds conf dict
            where 
              copyMessage (Tl x) =  do
                infM "-- Telegram.copyMessage" (" to user " ++ (show $ usId x))
                httpLBS  $ echoBuildRequest conf x
  
          sendMessagesWithKbTl upds dict = mapM_ sendMessageWithKb $ forKb upds
            where
              sendMessageWithKb (Tl x) = do
                infM "-- Telegram.sendMessageWithKb" (" to user " ++ (show $ usId x))
                httpLBS  $ kbBuildRequest conf dict x

          sendMessagesWithHelpTl upds = mapM_ sendMessageWithHelp $ forHelp upds
            where
              sendMessageWithHelp (Tl x) = do
                infM "-- Telegram.sendMessageWithHelp" (" to user " ++ (show $ usId x))
                httpLBS  $ helpBuildRequest conf x

          newDictTl upds dict = execState ( mapM_ changeMapInt
                                          $ getUserAndNumRep
                                          $ listUpdWithKey upds) dict

messageOK :: (Response LBC.ByteString)
          -> Config
          -> Log.Handle
          -> IO (Response LBC.ByteString)        
messageOK res conf handl = do
            let rsc = getResponseStatusCode res
            when ( not (rsc == 200)) $ do
              (errorM handl) (сonfigLogg conf) "-- messageOK  "
                                       ("-- status code of response " ++ show rsc)
              exitFailure
            return res
                                        
testException :: (Either SomeException (Response LBC.ByteString))
              ->  Bot.Handle
              ->  IO (Response LBC.ByteString)
testException rese handle = do
    case rese of
        Right val -> return val
        Left ex   -> do 
            (errorM  $ handlerLog handle) (сonfigLogg $ config handle)
                "-- Connection Failure" "-- Trying to initialize the session"
            threadDelay 25000000
            (initSession handle) handle
            httpLBS defaultRequest        