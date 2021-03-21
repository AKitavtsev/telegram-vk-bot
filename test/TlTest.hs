{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  TlTest where

import Test.Hspec
import Control.Monad.State
import Network.HTTP.Client.Internal

import qualified Data.Map as M

import Bot
import Config
import Log
import MapR
import Telegram.Data
import Telegram.Internal


tlTest :: IO ()
tlTest = hspec $ do
  let mess = (Message 261
                      (Just (User 484189456 False "Андрей")) (Just "222"))
      upMessage = Update 146878330 (Just mess) Nothing
      upCallbackQuery = Update 146878330
                Nothing
                (Just (CallbackQuery "2079577882558908621" 
                      (User 484189457 False "Андрей")  (Just "333")))
      uPDR = Tl (Update 146878330 (Just mess {text = (Just "/repeat")}) Nothing)
      uPDН = Tl (Update 146878330 (Just mess {text = (Just "/help")}) Nothing)
      conf = Config "" "123" "456" DEBUG 1 "" "Help me!" 25

-- vkIM = Vk_ItemMessage 1 0 2 "1" Nothing
        -- event =   (Event "message_new" (Vk_Message vkIM) 123456789)
        -- eventR = VK (event {e_object = (Vk_Message (vkIM {m_text = "/repeat"}))})
        -- eventH = VK (event {e_object = (Vk_Message (vkIM {m_text = "/help"}))})
        -- eventP = VK (event {e_object = (Vk_Message (vkIM {m_payload = Just "2"}))}) 
        -- conf = Config "" "123" "456" DEBUG 1 "" "Help me!" 25
        -- sess = Session "1fb"
                       -- "https://lp.vk.com/wh202551745" "1000"

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
      describe "cbData" $ do
        it "returns data from CallbackQuery" $
          cbData upCallbackQuery `shouldBe` "333"
          
      describe "forKb" $ do
        it "queries with command /repeat" $ 
          forKb [Tl upMessage, uPDR, uPDН, Tl upCallbackQuery] `shouldBe` [uPDR]
      describe "forHelp" $ do
        it "queries with command /help" $ 
          forHelp [Tl upMessage, uPDR, uPDН, Tl upCallbackQuery] `shouldBe` [uPDН]          
      describe "listUpdWithKey" $ do
        it "answers to the reactions to the requests with the command /repeat" $
              listUpdWithKey [Tl upMessage, uPDR, uPDН, Tl upCallbackQuery] 
                `shouldBe` [Tl upCallbackQuery]
      describe "forCopy" $ do
        it "all other requests including retries from the config" $
          forCopy [Tl upMessage, uPDR, uPDН, Tl upCallbackQuery] conf M.empty
            `shouldBe` [Tl upMessage]
        it "all other requests including retries from the dictionary" $
          forCopy [Tl upMessage, uPDR, uPDН, Tl upCallbackQuery] 
                  conf (M.fromList [(484189456,2)])
                    `shouldBe` [Tl upMessage, Tl upMessage]

     -- return ()      


