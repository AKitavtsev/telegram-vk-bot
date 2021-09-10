{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  TlTest where

import Test.Hspec
import Control.Monad.State
import Network.HTTP.Client.Internal

import qualified Data.Map as M

import Bot
import Services.Config
import Services.Types (Priority(..))
import Session
import Services.Logger
import Dictionary
import Bot.Telegram.Types
import Bot.Telegram.Internal


tlTest :: IO ()
tlTest = hspec $ do
  let mess = (Message 261
                      (Just (User 484189456 False "Андрей")) (Just "222"))
      upMessage = Update 146878330 (Just mess) Nothing
      upCallbackQuery = Update 146878330
                Nothing
                (Just (CallbackQuery "2079577882558908621" 
                      (User 484189457 False "Андрей")  (Just "3")))
      uPDR = Update 146878330 (Just mess {text = (Just "/repeat")}) Nothing
      uPDН = Update 146878330 (Just mess {text = (Just "/help")}) Nothing
      conf = Config INFO "" "123" "456"  1 "RepeatMe" "Help me!" 25
      sess = Session "" "" "0"

  describe "Telegram.Internal" $ do
      describe "newoffs" $ do
        it "returns offset for next request Update" $
          newoffs [upMessage] `shouldBe` "146878331"
      describe "usId" $ do
        it "returns user_id from Message" $
          usId upMessage `shouldBe` 484189456
        it "returns user_id from CallbackQuery" $
          usId upCallbackQuery `shouldBe` 484189457 
      describe "txt" $ do
        it "returns text from Message" $
          txt upMessage `shouldBe` "222"          
      describe "mesId" $ do
        it "returns message_id from Message" $
          mesId upMessage `shouldBe` 261
      describe "forKb" $ do
        it "queries with command /repeat" $ 
          forKb [upMessage, uPDR, uPDН, upCallbackQuery] `shouldBe` [uPDR]
      describe "forHelp" $ do
        it "queries with command /help" $ 
          forHelp [upMessage, uPDR, uPDН, upCallbackQuery] `shouldBe` [uPDН]          
      describe "listUpdWithKey" $ do
        it "answers to the reactions to the requests with the command /repeat" $
              listUpdWithKey [upMessage, uPDR, uPDН, upCallbackQuery] 
                `shouldBe` [upCallbackQuery]
      describe "forCopy" $ do
        it "all other requests including retries from the config" $
          forCopy [upMessage, uPDR, uPDН, upCallbackQuery] conf M.empty
            `shouldBe` [upMessage]
        it "all other requests including retries from the dictionary" $
          forCopy [upMessage, uPDR, uPDН, upCallbackQuery] 
                  conf (M.fromList [(484189456,2)])
                    `shouldBe` [upMessage, upMessage]     
      describe "eventBuildRequest" $ do
        it "returns host" $
          host  (eventBuildRequest conf "999") `shouldBe` appTelegram
        it "returns path" $
          path  (eventBuildRequest conf "999") `shouldBe` "456/getUpdates"
        it "returns queryString" $
          queryString  (eventBuildRequest conf "999") `shouldBe` 
               "?offset=999&timeout=25"
      describe "echoBuildRequest" $ do
        it "returns host" $
          host  (echoBuildRequest conf upMessage) `shouldBe` appTelegram
        it "returns path" $
          path  (echoBuildRequest conf upMessage) `shouldBe` "456/copyMessage"
        it "returns queryString" $
          queryString  (echoBuildRequest conf upMessage) `shouldBe`
                "?chat_id=484189456&from_chat_id=484189456&message_id=261"                
      describe "helpBuildRequest" $ do
        it "returns host" $
          host  (helpBuildRequest conf upMessage) `shouldBe` appTelegram
        it "returns path" $
          path  (helpBuildRequest conf upMessage) `shouldBe` "456/sendMessage"
        it "returns queryString" $
          queryString  (helpBuildRequest conf upMessage) `shouldBe`
               "?chat_id=484189456&text=Help%20me%21"              
      describe "kbBuildRequest" $ do
        it "returns host" $
          host  (kbBuildRequest conf M.empty upCallbackQuery) `shouldBe` appTelegram
        it "returns path" $
          path  (kbBuildRequest conf M.empty upCallbackQuery) `shouldBe` "456/sendMessage"
        it "returns queryString with number retries from the config" $
          queryString  (kbBuildRequest conf (M.fromList [(1,2)]) upCallbackQuery) `shouldBe`
            "?chat_id=484189457&text=1RepeatMe&reply_markup=%7B%22inline_keyboard%22%3A%5B%5B%7B%22text%22%3A%221%22%2C%22callback_data%22%3A%221%22%7D%2C%7B%22text%22%3A%222%22%2C%22callback_data%22%3A%222%22%7D%2C%7B%22text%22%3A%223%22%2C%22callback_data%22%3A%223%22%7D%2C%7B%22text%22%3A%224%22%2C%22callback_data%22%3A%224%22%7D%2C%7B%22text%22%3A%225%22%2C%22callback_data%22%3A%225%22%7D%5D%5D%7D"
        it "returns queryString with number retries from the dictionary" $
          queryString  (kbBuildRequest conf M.empty upCallbackQuery) `shouldBe`
            "?chat_id=484189457&text=1RepeatMe&reply_markup=%7B%22inline_keyboard%22%3A%5B%5B%7B%22text%22%3A%221%22%2C%22callback_data%22%3A%221%22%7D%2C%7B%22text%22%3A%222%22%2C%22callback_data%22%3A%222%22%7D%2C%7B%22text%22%3A%223%22%2C%22callback_data%22%3A%223%22%7D%2C%7B%22text%22%3A%224%22%2C%22callback_data%22%3A%224%22%7D%2C%7B%22text%22%3A%225%22%2C%22callback_data%22%3A%225%22%7D%5D%5D%7D"
 
      describe "getUserAndNumRep" $ do
        it "returns [(User Id, number retries)]" $
               getUserAndNumRep [upCallbackQuery] `shouldBe` [(484189457, 3)]
