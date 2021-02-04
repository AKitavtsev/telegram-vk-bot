{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  Main where

import Test.Hspec
import Control.Monad.State

import qualified Data.Map as M

import Config
import DataTelegram
import MapR
import Telegram

main :: IO ()
main = hspec $ do
  let upMessage = Update 146878330
                (Just (Message 261
                      (Just (User 484189456 False "Андрей")) (Just "222"))) 
                Nothing
      upCallbackQuery = Update 146878330
                Nothing
                (Just (CallbackQuery "2079577882558908621" 
                      (User 484189457 False "Андрей")  (Just "333")))      

  describe "newoffs" $ do
    it "returns offset for next request Update" $
      newoffs [upMessage] `shouldBe` 146878331
  describe "usId" $ do
    it "returns user_id from Message" $
      usId upMessage `shouldBe` 484189456
    it "returns user_id from CallbackQuery" $
      usId upCallbackQuery `shouldBe` 484189457 
  describe "cbData" $ do
    it "returns data from CallbackQuery" $
      cbData upCallbackQuery `shouldBe` "333"   
  describe "chatId" $ do
    it "returns QueryItem chat_id" $
      chatId 1 `shouldBe` ("chat_id", Just $ "1")
  describe "fromChatId" $ do
    it "returns QueryItem from_chat_id" $
      fromChatId 1 `shouldBe` ("from_chat_id", Just $ "1")
  describe "messageId" $ do
    it "returns QueryItem message_id" $
      messageId 1 `shouldBe` ("message_id", Just $ "1")
  describe "offset" $ do
    it "returns QueryItem offset" $
      offset 1 `shouldBe` ("offset", Just $ "1")
  describe "timeout" $ do
    it "returns QueryItem timeout" $
      timeout 5 `shouldBe` ("timeout", Just $ "5")      
  describe "changeMapInt" $ do
    it "add in empty" $ 
      runState (changeMapInt (5, 5))  M.empty `shouldBe` ((), M.singleton 5 5)
    it "add in not empty" $ 
      runState (changeMapInt (2, 22))  (M.fromList [(1,2)]) `shouldBe` ((), (M.fromList [(1,2), (2,22)]))
    it "replace " $ 
      runState (changeMapInt (1, 22))  (M.fromList [(1,2)]) `shouldBe` ((), (M.fromList [(1,22)]))
