{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram 

    where

import Control.Exception
import Control.Monad.State    
import Data.Aeson
-- import Network.HTTP.Client
import Network.HTTP.Simple
import System.Exit
import Control.Concurrent (threadDelay)

import Bot
import Config
import Telegram.Data
import Drop
import Log
import MapR

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M
import qualified Data.Text as T


newHandle :: Config -> Log.Handle-> IO Bot.Handle
newHandle conf handl = do
    return $ Bot.Handle
        {Bot.config = conf
        ,Bot.handlerLog = handl        
        -- ,Bot.initSession = initSession
        }
        -- where 
          -- initSession handle = do
            -- return (Session "" "" "0")     

-- loopTelegram ::  Config -> MapInt -> Int -> IO ()
-- loopTelegram  conf dict offs = do
    -- debugM (сonfigLogg conf) "-- loopTelegram"
                             -- (" -- dict = " ++ show dict)
    -- resEither <- try (httpLBS  $ eventBuildRequest conf offs)                             
    -- res' <- testException resEither conf dict offs
    -- res  <- messageOK res'    

    -- let rsc = getResponseStatusCode res
    -- when ( not (rsc == 200)) $ do
        -- errorM (сonfigLogg conf) "-- -- loopTelegram"
                                       -- (" -- status code of response " ++ show rsc)
        -- exitFailure
    -- let listUpd = upds ((decode $ getResponseBody res))        

    -- debugM (сonfigLogg conf) "-- loopTelegram" 
                                    -- (" -- List of Updates received:\n" ++ show listUpd)
    -- let listUpdWithMessage = filter (\x -> not ((message x) == Nothing)) listUpd
    -- let fc  = forCopy conf dict listUpdWithMessage
    -- mapM_ copyMessage fc
    -- infoM (сonfigLogg conf) "-- loopTelegram" (" -- " ++ show (length fc) ++ 
                                               -- " returns to addressees")
    -- let fkb = forKb listUpdWithMessage
    -- mapM_ sendMessageWithKeyboard fkb
    -- infoM (сonfigLogg conf) "-- loopTelegram"
                           -- (" -- " ++ show (length fkb)  ++ 
                            -- " requests sent to change the number of retries")
    -- let fh = forHelp listUpdWithMessage
    -- mapM_ helpMessage fh
    -- infoM (сonfigLogg conf) "-- loopTelegram"
                           -- (" -- " ++ show (length fh) ++ " help")
    
    -- let lcb = listUpdWithCallbackQuery listUpd
    -- infoM (сonfigLogg conf) "-- loopTelegram"
                                    -- (" -- " ++ show (length lcb) ++ 
                                     -- " change the number of retries")
    -- let newdict = execState (mapChangeMapInt  
                             -- $ getUsidAndCbdata lcb) dict
    -- loopTelegram conf newdict $ newoffs listUpd
      -- where
        -- copyMessage x =  do
            -- res <- httpLBS  $ echoBuildRequest conf x
-- !            resEither <- try (httpLBS  $ echoBuildRequest conf (getVk_ItemMessage x))
-- !            res <- testException resEither conf
            -- messageOK res
        -- sendMessageWithKeyboard x = do
            -- res <- httpLBS  $ kbBuildRequest conf dict x
-- !            resEither <- try (httpLBS  $ kbBuildRequest conf dict (getVk_ItemMessage x))
-- !            res <- testException resEither conf
            -- messageOK res
        -- helpMessage x =  do
            -- res <- httpLBS  $ helpBuildRequest conf  x
-- !            resEither <- try (httpLBS  $ helpBuildRequest conf (getVk_ItemMessage x))
-- !            res <- testException resEither conf
            -- messageOK res
        -- messageOK res = do
            -- let rsc = getResponseStatusCode res
            -- when ( not (rsc == 200)) $ do
              -- errorM (сonfigLogg conf) "-- messageOK  "
                                       -- ("-- status code of response " ++ show rsc)
              -- exitFailure
            -- return res    

-- newoffs :: [Update] -> Int
-- newoffs [] = 0
-- newoffs x = upId (last x) + 1

-- upds :: Maybe UpdatesResponse -> [Update]
-- upds (Just (Tl_Response x)) = x
-- upds Nothing = []

-- usId :: Update  -> Int
-- usId x = let us = case message x of
                      -- Just y -> from y
                      -- Nothing -> case callback_query x of
                        -- Just y -> Just (cq_from  y)
         -- in case us of
                -- Just z -> user_id z
                     
-- upId :: Update -> Int
-- upId x = update_id x

-- mesId :: Update -> Int
-- mesId x = case message x of
              -- Just y -> message_id y
                                   
-- txt :: Update -> T.Text
-- txt x = let t = case message x of
             -- Just y -> text y             
        -- in case t of
            -- Just z -> z
            -- Nothing -> ""
            
-- cbData :: Update -> String
-- cbData x = let cq = case callback_query x of
                    -- Just y -> cq_data y
           -- in case cq of
            -- Just z -> z
            -- Nothing -> ""

-- listUpdWithCallbackQuery :: [Update] -> [Update]
-- listUpdWithCallbackQuery xs = filter (\x -> not ((callback_query x) == Nothing)) xs
            
-- forKb :: [Update] -> [Update]
-- forKb xs = filter (\x -> (txt x) == "/repeat") xs

-- forHelp :: [Update] -> [Update]
-- forHelp xs = filter (\x -> (txt x) == "/help") xs

-- forCopy :: Config -> MapInt -> [Update] -> [Update]
-- forCopy conf dict xs = concat (map  repeating (filtred xs))
    -- where filtred xxs = filter (\x ->  not ((txt x) == "/repeat" ||
                                            -- (txt x) == "/help" )) xxs
          -- repeating x = take (numRepeat x) $ repeat x
          -- numRepeat x = M.findWithDefault (сonfigNumberRepeat conf) (usId x) dict 
                                          
-- eventBuildRequest :: Config -> Int-> Request
-- eventBuildRequest conf offs = setRequestHost appTelegram
                       -- $ setRequestPath (BC.pack $ сonfigToken conf ++ "/getUpdates")
                       -- $ setRequestQueryString qi
                       -- $ defaultRequest
    -- where
      -- qi = [ ("offset",  Just $ BC.pack (show offs))
           -- , ("timeout", Just $ BC.pack (show $ myTimeout conf))]
           
-- echoBuildRequest :: Config -> Update-> Request
-- echoBuildRequest conf upd = setRequestHost appTelegram
                     -- $ setRequestPath (BC.pack $ сonfigToken conf ++ "/copyMessage")
                     -- $ setRequestQueryString qi
                     -- $ defaultRequest
    -- where
      -- qi = [ ("chat_id", Just $ BC.pack (show $ usId upd))
           -- , ("from_chat_id", Just $ BC.pack (show $ usId upd))
           -- , ("message_id", Just $ BC.pack (show $ mesId upd))]
      
-- helpBuildRequest :: Config -> Update -> Request
-- helpBuildRequest conf upd = setRequestHost appTelegram
                     -- $ setRequestPath (BC.pack $ сonfigToken conf ++ "/sendMessage")
                     -- $ setRequestQueryString qi
                     -- $ defaultRequest
    -- where
      -- qi = [ ("chat_id", Just $ BC.pack (show $ usId upd))
           -- , ("text", Just $ BC.pack $ messageForHelp conf)]
          
-- kbBuildRequest :: Config -> MapInt-> Update -> Request
-- kbBuildRequest conf dict upd = setRequestHost appTelegram
                     -- $ setRequestPath (BC.pack $ сonfigToken conf ++ "/sendMessage")
                     -- $ setRequestQueryString qi
                     -- $ defaultRequest
    -- where
      -- qi = [ ("chat_id", Just $ BC.pack (show $ usId upd))
           -- , ("text", Just $ BC.pack $ textForRepeat $ usId upd)
           -- , ("reply_markup", Just $ BC.pack $ LBC.unpack $ encode myKeyboard)]
      -- buttonsForMyKb = [ InlineKeyboardButton "1" (Just "1")
                       -- , InlineKeyboardButton "2" (Just "2")
                       -- , InlineKeyboardButton "3" (Just "3")
                       -- , InlineKeyboardButton "4" (Just "4")
                       -- , InlineKeyboardButton "5" (Just "5")]
      -- myKeyboard =  InlineKeyboardMarkup [buttonsForMyKb]

      -- textForRepeat x = (show $ M.findWithDefault
                        -- (сonfigNumberRepeat conf) x dict)
                         -- ++ (messageForRepeat conf)

-- getUsidAndCbdata :: [Update] -> [(Int, Int)]
-- getUsidAndCbdata xs = map fgets xs 
    -- where fgets x = ((usId x), (read (cbData x)::Int))        
                         
-- testException :: (Either SomeException (Response LBC.ByteString))
              -- -> Config -> MapInt -> Int
              -- ->  IO (Response LBC.ByteString)
-- testException rese conf dict off = do
    -- case rese of
        -- Right val -> return val
        -- Left ex   -> do
            -- errorM  (сonfigLogg conf) "-- Connection Failure"  "-- Try again"
            -- threadDelay 25000000
            -- loopTelegram  conf dict off
            -- httpLBS  $ eventBuildRequest conf off
 