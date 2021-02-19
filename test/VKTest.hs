{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  VKTest where

import Test.Hspec
-- import Control.Monad.State
import Network.HTTP.Client

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M

import Config
import DataVK
import MapR
import VK


vkTest :: IO ()
vkTest = hspec $ do
    let vkIM = Vk_ItemMessage 1 0 222222222 "1" Nothing
        event =  Event "message_new" (Vk_Message vkIM) 123456789
        eventR = event {e_object = (Vk_Message (vkIM {m_text = "/repeat"}))}
        eventH = event {e_object = (Vk_Message (vkIM {m_text = "/help"}))}
        eventP = event {e_object = (Vk_Message (vkIM {m_payload = Just "1"}))} 
        conf = Config "" "" "" DEBUG 1 "" "" 25
        sess = Session "1fb"
                       "https://lp.vk.com/wh202551745" "1000"
    describe "getVk_ItemMessage" $ do
        it "get message" $ 
          getVk_ItemMessage event `shouldBe` vkIM
        it "get from_id" $ 
          m_from_id  (getVk_ItemMessage event) `shouldBe` 1
        it "get text" $ 
          m_text  (getVk_ItemMessage event) `shouldBe` "1"
        it "get payload" $ 
          m_payload  (getVk_ItemMessage event) `shouldBe` Nothing
    describe "forKb" $ do
        it "queries with command /repeat" $ 
          forKb (Just [event, eventR, eventH, eventP]) `shouldBe` [eventR]
    describe "forHelp" $ do
        it "queries with command /help" $ 
          forHelp (Just [event, eventR, eventH, eventP]) `shouldBe` [eventH]          
    describe "listUpdWithPayload" $ do
        it "answers to the reactions to the requests with the command /repeat" $
          listUpdWithPayload (Just [event, eventR, eventH, eventP]) `shouldBe` [eventP]
    describe "forCopy" $ do
        it "all other requests including retries from the config" $
          forCopy (Just [event, eventR, eventH, eventP]) conf M.empty `shouldBe` [event]
        it "all other requests including retries from the dictionary" $
          forCopy (Just [event, eventR, eventH, eventP]) conf (M.fromList [(1,2)])
          `shouldBe` [event, event]
    describe "eventBuildRequest" $ do
        it "returns host" $
           host  (eventBuildRequest sess conf "999") `shouldBe` "lp.vk.com"
        it "returns path" $
           path  (eventBuildRequest sess conf "999") `shouldBe` "/wh202551745"
        it "returns queryString" $
           queryString  (eventBuildRequest sess conf "999")`shouldBe` "?act=a_check&key=1fb&ts=999&wait=25"
