{-# LANGUAGE OverloadedStrings #-}

module  VKTest where

import Test.Hspec
import Network.HTTP.Client.Internal

import qualified Data.Map as M

import Config
import Services.Types (Priority(..))
import Bot.VK.Types
import Bot
import Bot.VK.Internal


vkTest :: IO ()
vkTest = hspec $ do
    let vkIM = VKItemMessage 1 0 2 "1" Nothing
        event =   (Event "message_new" (VKMessage vkIM) 123456789)
        eventR = event {e_object = (VKMessage (vkIM {m_text = "/repeat"}))}
        eventH = event {e_object = (VKMessage (vkIM {m_text = "/help"}))}
        eventP = event {e_object = (VKMessage (vkIM {m_payload = Just "2"}))} 
        conf = Config INFO "" "123" "456" 1 "" "Help me!" 25
        sess = Session "1fb"
                       "https://lp.vk.com/wh202551745" "1000"
    describe "VK.Internal" $ do                  
        describe "getVkItemMessage" $ do
            it "get message" $ 
              getVkItemMessage event `shouldBe` vkIM
            it "get from_id" $ 
              m_from_id  (getVkItemMessage event) `shouldBe` 1
            it "get text" $ 
              m_text  (getVkItemMessage event) `shouldBe` "1"
            it "get payload" $ 
              m_payload  (getVkItemMessage event) `shouldBe` Nothing
        describe "forKb" $ do
            it "queries with command /repeat" $ 
              forKb [event, eventR, eventH, eventP] `shouldBe` [eventR]
        describe "forHelp" $ do
            it "queries with command /help" $ 
              forHelp [event, eventR, eventH, eventP] `shouldBe` [eventH]          
        describe "forCopy" $ do
            it "all other requests including retries from the config" $
              forCopy [event, eventR, eventH, eventP] conf M.empty `shouldBe` [event]
            it "all other requests including retries from the dictionary" $
              forCopy [event, eventR, eventH, eventP] conf (M.fromList [(1,2)])
              `shouldBe` [event, event]
        describe "initBuildRequest" $ do
            it "returns host" $
               host (initBuildRequest conf) `shouldBe` "api.vk.com"
            it "returns path" $
               path  (initBuildRequest conf) `shouldBe` "/method/groups.getLongPollServer"
            it "returns queryString" $
               queryString  (initBuildRequest conf)`shouldBe`
               "?group_id=123&access_token=456&v=5.126"
        describe "eventBuildRequest" $ do
            it "returns host" $
               host  (eventBuildRequest sess conf "999") `shouldBe` "lp.vk.com"
            it "returns path" $
               path  (eventBuildRequest sess conf "999") `shouldBe` "/wh202551745"
            it "returns queryString" $
               queryString  (eventBuildRequest sess conf "999") `shouldBe` 
               "?act=a_check&key=1fb&ts=999&wait=25"
        describe "echoBuildRequest" $ do
            it "returns host" $
               host  (echoBuildRequest conf vkIM) `shouldBe` "api.vk.com"
            it "returns path" $
               path  (echoBuildRequest conf vkIM) `shouldBe` "/method/messages.send"
            it "returns queryString" $
               queryString  (echoBuildRequest conf vkIM) `shouldBe`
               "?user_id=1&forward_messages=2&random_id=0&access_token=456&v=5.126"
        describe "helpBuildRequest" $ do
            it "returns host" $
               host  (helpBuildRequest conf vkIM) `shouldBe` "api.vk.com"
            it "returns path" $
               path  (helpBuildRequest conf vkIM) `shouldBe` "/method/messages.send"
            it "returns queryString" $
               queryString  (helpBuildRequest conf vkIM) `shouldBe`
               "?user_id=1&random_id=0&message=Help%20me%21&access_token=456&v=5.126"
        describe "kbBuildRequest" $ do
            it "returns host" $
               host  (kbBuildRequest conf M.empty vkIM) `shouldBe` "api.vk.com"
            it "returns path" $
               path  (kbBuildRequest conf M.empty vkIM) `shouldBe` "/method/messages.send"
            it "returns queryString with number retries from the config" $
               queryString  (kbBuildRequest conf M.empty vkIM) `shouldBe`
               "?user_id=1&random_id=0&message=1&keyboard=%7B%22buttons%22%3A%5B%5B%7B%22color%22%3A%22primary%22%2C%22action%22%3A%7B%22payload%22%3A%221%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%221%22%7D%7D%2C%7B%22color%22%3A%22primary%22%2C%22action%22%3A%7B%22payload%22%3A%222%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%222%22%7D%7D%2C%7B%22color%22%3A%22primary%22%2C%22action%22%3A%7B%22payload%22%3A%223%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%223%22%7D%7D%2C%7B%22color%22%3A%22primary%22%2C%22action%22%3A%7B%22payload%22%3A%224%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%224%22%7D%7D%2C%7B%22color%22%3A%22primary%22%2C%22action%22%3A%7B%22payload%22%3A%225%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%225%22%7D%7D%5D%5D%2C%22inline%22%3Atrue%2C%22one_time%22%3Afalse%7D&access_token=456&v=5.126"
            it "returns queryString with number retries from the dictionary" $
               queryString  (kbBuildRequest conf (M.fromList [(1,2)]) vkIM) `shouldBe`
               "?user_id=1&random_id=0&message=2&keyboard=%7B%22buttons%22%3A%5B%5B%7B%22color%22%3A%22primary%22%2C%22action%22%3A%7B%22payload%22%3A%221%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%221%22%7D%7D%2C%7B%22color%22%3A%22primary%22%2C%22action%22%3A%7B%22payload%22%3A%222%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%222%22%7D%7D%2C%7B%22color%22%3A%22primary%22%2C%22action%22%3A%7B%22payload%22%3A%223%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%223%22%7D%7D%2C%7B%22color%22%3A%22primary%22%2C%22action%22%3A%7B%22payload%22%3A%224%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%224%22%7D%7D%2C%7B%22color%22%3A%22primary%22%2C%22action%22%3A%7B%22payload%22%3A%225%22%2C%22type%22%3A%22text%22%2C%22label%22%3A%225%22%7D%7D%5D%5D%2C%22inline%22%3Atrue%2C%22one_time%22%3Afalse%7D&access_token=456&v=5.126"
    describe "VK.Types (instance Upd)" $ do
      describe "usId" $ do
        it "returns user_id from VKItemMessage" $
          usId event `shouldBe` 1
      describe "txt" $ do
        it "returns text from VKItemMessage" $
          txt event `shouldBe` "1"          
      describe "mesId" $ do
        it "returns message_id from VKItemMessage" $
          mesId event `shouldBe` 2
      describe "listUpdWithKey" $ do
            it "answers to the reactions to the requests with the command /repeat" $
              listUpdWithKey [event, eventR, eventH, eventP] `shouldBe` [eventP]
      describe "getUserAndNumRep" $ do
            it "returns [(User Id, number retries)]" $
               getUserAndNumRep [eventP] `shouldBe` [(1, 2)]

