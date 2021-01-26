{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  Main where

import Test.Hspec
import LoopR
import Types

main :: IO ()
main = hspec $ do
  let us = Update 146878273
                   (Just (Message 25 
                                 (Just (User 484189456 False "Андрей"))
                                 1610909106
                                 (Chat 484189456 Private)
                        ))
  describe "newoffs" $ do
    it "returns offset for next request Update" $
      newoffs [us] `shouldBe` 146878274
  describe "usId" $ do
    it "returns user_id" $
      usId us `shouldBe` 484189456
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
      offset 5 `shouldBe` ("offset", Just $ "5")

