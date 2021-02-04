{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Telegram 
    where
    
import Data.Aeson
import Network.HTTP.Simple
import Control.Monad.State
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Class

import Config
import DataTelegram
import MapR

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M
import qualified Data.Text as T


{- | Trying to get another Update. Processing. We go to the next. 
-- Parameters - сonfig and offset
-}
loopTelegram ::  Config -> MapInt -> Int -> IO ()
loopTelegram  conf dict offs = do
-- https://api.telegram.org/bot<token>/getUpdates
    debugM (сonfigLogg conf) "-- loopTelegram" 
                                    (" -- dict = " ++ show dict)

    listUpdJons <- fetchJSON conf "/getUpdates" [offset offs, timeout 5]
    let listUpd = upds $ updatesResponseFromJSON listUpdJons
    debugM (сonfigLogg conf) "-- loopTelegram" 
                                    (" -- List of Updates received:\n" ++ show listUpd)
    let listUpdWithMessage = filter (\x -> not ((message x) == Nothing)) listUpd
        listUpdWithCallbackQuery = 
            filter (\x -> not ((callback_query x) == Nothing)) listUpd
        forKb = filter (\x -> (txt x) == "/repeat") listUpdWithMessage 
        forC  = forCopy listUpdWithMessage
    infoM (сonfigLogg conf) "-- loopTelegram"
                                    (" -- " ++ show (length forKb)  ++
                                     " requests sent to change the number of retries\n")
    infoM (сonfigLogg conf) "-- loopTelegram" (" -- " ++ show (length forC) ++ 
                                               " returns to addressees\n")
    infoM (сonfigLogg conf) "-- loopTelegram"
                                    (" -- " ++ show (length listUpdWithCallbackQuery) ++ 
                                     " change the number of retries\n")
   
    mapM_ copyMessage forC
    
    mapM_ sendMessageWithKeyboard forKb
    
    let newdict = execState (mapChangeMapInt  
                             $ getUsidAndCbdata listUpdWithCallbackQuery) dict
    
    
    loopTelegram conf newdict $ newoffs listUpd
    
      where
        -- copyMessage :: Update -> IO LBC.ByteString
        copyMessage x =  fetchJSON conf "/copyMessage" [chatId userId
                                                       ,fromChatId userId
                                                       ,messageId (mesId x)]
                          where userId = usId x
                -- sendMessageWithKeyboard :: Update -> IO LBC.ByteString
        sendMessageWithKeyboard x =  fetchJSON conf 
                                        "/sendMessage" [chatId (usId x)
                                                       ,messageText (textForSend x)        
                                                       ,keyboardForRepeats
                                                       ]

        getUsidAndCbdata :: [Update] -> [(Int, Int)]
        getUsidAndCbdata xs = map fgets xs 
           where fgets x = ((usId x), (read (cbData x)::Int))

        
        textForSend x = (show $ M.findWithDefault (сonfigNumberRepeat conf) (usId x) dict) ++
                        " repetitions are set for you\n" ++
                        "Choose how much you need in the future"        
        forCopy xs = concat (map  repeating (filtred xs))
          where filtred xxs = filter (\x ->  not ((txt x) == "/repeat")) xxs
                repeating x = take (numRepeat x) $ repeat x
                numRepeat x = M.findWithDefault (сonfigNumberRepeat conf) (usId x) dict 
        
buttonForMyKb :: [InlineKeyboardButton]
buttonForMyKb = [InlineKeyboardButton {ikb_text = "1", ikb_callback_data = Just "1"}
                ,InlineKeyboardButton {ikb_text = "2", ikb_callback_data = Just "2"}
                ,InlineKeyboardButton {ikb_text = "3", ikb_callback_data = Just "3"}
                ,InlineKeyboardButton {ikb_text = "4", ikb_callback_data = Just "4"}
                ,InlineKeyboardButton {ikb_text = "5", ikb_callback_data = Just "5"}
                ]
                
myKeyboard :: InlineKeyboardMarkup
myKeyboard = InlineKeyboardMarkup {inline_keyboard = [buttonForMyKb]}
            
path ::  Config -> String -> BC.ByteString
path conf meth = BC.pack $ сonfigToken conf ++ meth
                         
newoffs :: [Update] -> Int
newoffs [] = 0
newoffs x = upId (last x) + 1

chatId :: Int -> QueryItem
chatId chid = ("chat_id", Just $ BC.pack (show chid))

messageText :: String -> QueryItem
messageText s = ("text", Just $ BC.pack s)

keyboardForRepeats :: QueryItem
keyboardForRepeats  = ("reply_markup", Just $ inlineKeyboardMarkupToJSON myKeyboard)



fromChatId :: Int -> QueryItem
fromChatId chid = ("from_chat_id", Just $ BC.pack (show chid))

messageId :: Int -> QueryItem
messageId mid = ("message_id", Just $ BC.pack (show mid))

offset :: Int -> QueryItem
offset os = ("offset", Just $ BC.pack (show os))

timeout :: Int -> QueryItem
timeout to = ("timeout", Just $ BC.pack (show to))

upds :: Maybe UpdatesResponse -> [Update]
upds (Just (Response x)) = x
upds Nothing = []

usId :: Update  -> Int
usId x = let us = case message x of
                      Just y -> from y
                      Nothing -> case callback_query x of
                        Just y -> Just (cq_from  y)
         in case us of
                Just z -> user_id z
                     
upId :: Update -> Int
upId x = update_id x

mesId :: Update -> Int
mesId x = case message x of
              Just y -> message_id y
                                   
txt :: Update -> T.Text
txt x = let t = case message x of
             Just y -> text y             
        in case t of
            Just z -> z
            Nothing -> ""
            
cbData :: Update -> String
cbData x = let cq = case callback_query x of
                    Just y -> cq_data y
           in case cq of
            Just z -> z
            Nothing -> ""
                                          
fetchJSON :: Config -> String -> [QueryItem] -> IO LBC.ByteString
fetchJSON conf meth qi = do
    res <- httpLBS  $ buildRequest conf meth qi
    return (getResponseBody res)
            
buildRequest :: Config -> String -> [QueryItem] -> Request
buildRequest conf p querys = setRequestHost appTelegram
                        $ setRequestPath (path conf p)
                        $ setRequestQueryString querys
                        $ defaultRequest

updatesResponseFromJSON :: LBC.ByteString -> Maybe UpdatesResponse
updatesResponseFromJSON = decode
                                
inlineKeyboardMarkupToJSON :: InlineKeyboardMarkup -> BC.ByteString
inlineKeyboardMarkupToJSON x = BC.pack (LBC.unpack (encode x))


    

    



    

