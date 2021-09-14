{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  BotTest where

import Test.Hspec

import qualified Data.Map as M

import Bot
import Bot.Telegram.Types
import Bot.VK.Types

botTest :: IO ()
botTest = hspec $ do

  let upCallbackQuery = Update 146878330
                        Nothing
                        (Just (CallbackQuery "2079577882558908621" 
                        (User 484189457 False "Андрей") 
                        (Just "3")))
      upCallbackQuery' = Update 146878330
                        Nothing
                        (Just (CallbackQuery "2079577882558908621" 
                        (User 484189458 False "Андрей") 
                        (Just "5")))
      dl1 = DataLoop (Session "" "" "0") [upCallbackQuery] M.empty "0"
      dl2 = DataLoop (Session "" "" "0") [upCallbackQuery, upCallbackQuery'] M.empty "0"
      
  let vkIM1 = VKItemMessage 1 0 2 "1" Nothing
      vkIM2 = VKItemMessage 2 0 2 "1" Nothing
      event1 =   (Event "message_new" (VKMessage (vkIM1 {m_payload = Just "2"})) 123456789)
      event2 =   (Event "message_new" (VKMessage (vkIM2 {m_payload = Just "5"})) 123456790)
        -- eventR = event {e_object = (VKMessage (vkIM {m_text = "/repeat"}))}
        -- eventH = event {e_object = (VKMessage (vkIM {m_text = "/help"}))}
      -- eventP = event {e_object = (VKMessage (vkIM {m_payload = Just "2"}))} 
      dv1 = DataLoop (Session "" "" "0") [event1] M.empty "0"
      dv2 = DataLoop (Session "" "" "0") [event1, event2] M.empty "0"
      
  describe "Bot" $ do
    describe "newDict" $ do
      describe "newDict for Telegram" $ do
        it "one update" $ 
          dictionary (newDict dl1) `shouldBe` M.fromList [(484189457,3)]
        it "a few updates" $ 
          dictionary (newDict dl2) `shouldBe` M.fromList [(484189457,3), (484189458,5)]
      describe "newDict for VK" $ do
        it "one update" $ 
          dictionary (newDict dv1) `shouldBe` M.fromList [(1,2)]
        it "a few updates" $ 
          dictionary (newDict dv2) `shouldBe` M.fromList [(1,2), (2,5)]
