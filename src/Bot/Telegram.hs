{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Bot.Telegram 

    where

import Control.Exception
import Control.Monad.State    
import Data.Aeson
import Network.HTTP.Simple
import System.Exit
import Control.Concurrent (threadDelay)

import Bot
import Drop
import MapR
import Servises.Config
import Bot.Telegram.Data
import Servises.Logger
import Bot.Telegram.Internal


import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M
import qualified Data.Text as T


newHandle :: Config -> IO Bot.Handle
newHandle conf = do
    return $ Bot.Handle
        { initSession          = initSessionTl
        , getUpdates           = getUpdatesTl
        , copyMessages         = copyMessagesTl
        , sendMessagesWithKb   = sendMessagesWithKbTl
        , sendMessagesWithHelp = sendMessagesWithHelpTl
        , newDict              = newDictTl  
        }
        where
          initSessionTl handle hLogger = do
            loopBot handle hLogger (DataLoop (Session "" "" "0") [] M.empty "0")
            return ()    
          getUpdatesTl handle hLogger dl = do  
            let sess = session dl
                ts = offset dl
            resEither <- try (httpLBS  $ eventBuildRequest sess conf ts)
            res' <- testException resEither handle hLogger
            res  <- messageOK res' conf hLogger 
            let upds = listUpd ((decode $ getResponseBody res))
            logDebug hLogger(" List of Updates received = \n " ++ (show upds))
            return dl {updates = map Tl upds, offset = newoffs upds}
          copyMessagesTl handle hLogger dl = do
            mapM_ copyMessage $ forCopy upds conf dict
            return dl
              where
                upds = updates dl
                dict = dictionary dl
                copyMessage (Tl x) =  do
                  logInfo hLogger (" to user " ++ (show $ usId x))
                  httpLBS  $ echoBuildRequest conf x  
          sendMessagesWithKbTl handle hLogger dl= do
            mapM_ sendMessageWithKb $ forKb upds
            return dl
              where
                upds = updates dl
                dict = dictionary dl
                sendMessageWithKb (Tl x) = do
                  logInfo hLogger(" to user " ++ (show $ usId x))
                  httpLBS  $ kbBuildRequest conf dict x
          sendMessagesWithHelpTl handle hLogger dl = do 
            mapM_ sendMessageWithHelp $ forHelp upds
            return dl
              where
                upds = updates dl
                sendMessageWithHelp (Tl x) = do
                  logInfo hLogger (" to user " ++ (show $ usId x))
                  httpLBS  $ helpBuildRequest conf x
          newDictTl handle hLogger dl = do
            let upds = updates dl
                dict = dictionary dl
                newdict = execState ( mapM_ changeMapInt
                                    $ getUserAndNumRep
                                    $ listUpdWithKey upds) dict
            return dl {dictionary = newdict}
