{-# LANGUAGE OverloadedStrings #-}

--
module Bot.VK.Internal where

import Data.Maybe (isJust, isNothing)
import Network.HTTP.Simple

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.Map as M

import Bot
import Bot.VK.Types
import Dictionary
import Services.Config
import Session

forCopy :: [UPD] -> Config -> MapInt -> [UPD]
forCopy upds conf dict = map VK $ concatMap repeating (filtred upds)
  where
    filtred =
      filter
        (\(VK x) ->
           (m_text (getVkItemMessage x) /= "/repeat" &&
            m_text (getVkItemMessage x) /= "/help") &&
           isNothing (m_payload (getVkItemMessage x)))
    repeating ~(VK x) = replicate (numRepeat x) x
    numRepeat x =
      M.findWithDefault
        (сonfigNumberRepeat conf)
        (m_from_id (getVkItemMessage x))
        dict

forHelp, forKb, listUpdWithKey :: [UPD] -> [UPD]
forHelp = filter (\(VK x) -> m_text (getVkItemMessage x) == "/help")

forKb = filter (\(VK x) -> m_text (getVkItemMessage x) == "/repeat")

listUpdWithKey = filter (\(VK x) -> isJust (m_payload (getVkItemMessage x)))

initBuildRequest :: Config -> Request
initBuildRequest conf =
  setRequestQueryString qi $
  parseRequest_ "https://api.vk.com/method/groups.getLongPollServer"
  where
    qi =
      [ ("group_id", Just (BC.pack $ groupId conf))
      , ("access_token", Just (BC.pack $ сonfigToken conf))
      , ("v", Just "5.126")
      ]

eventBuildRequest :: Session -> Config -> String -> Request
eventBuildRequest sess conf offs =
  setRequestQueryString qi $ parseRequest_ $ server sess
  where
    qi =
      [ ("act", Just "a_check")
      , ("key", Just (BC.pack $ key sess))
      , ("ts", Just (BC.pack offs))
      , ("wait", Just (BC.pack $ show (myTimeout conf)))
      ]

echoBuildRequest :: Config -> VKItemMessage -> Request
echoBuildRequest conf event = setRequestQueryString qi $ parseRequest_ appVK
  where
    qi =
      [ ("user_id", Just (BC.pack $ show (m_from_id event)))
      , ("forward_messages", Just (BC.pack $ show (m_id event)))
      , ("random_id", Just (BC.pack $ show (m_random_id event)))
      , ("access_token", Just (BC.pack $ сonfigToken conf))
      , ("v", Just "5.126")
      ]

kbBuildRequest :: Config -> MapInt -> VKItemMessage -> Request
kbBuildRequest conf dict event = setRequestQueryString qi $ parseRequest_ appVK
  where
    qi =
      [ ("user_id", Just (BC.pack $ show (m_from_id event)))
      , ("random_id", Just (BC.pack $ show (m_random_id event)))
      , ("message", Just (BC.pack $ textForRepeat (m_from_id event)))
      , ("keyboard", Just (BC.pack $ LBC.unpack $ encode myKeyboard))
      , ("access_token", Just (BC.pack $ сonfigToken conf))
      , ("v", Just "5.126")
      ]
    buttonsForMyKb =
      [ Button (Action "text" "1" "1") "primary"
      , Button (Action "text" "2" "2") "primary"
      , Button (Action "text" "3" "3") "primary"
      , Button (Action "text" "4" "4") "primary"
      , Button (Action "text" "5" "5") "primary"
      ]
    myKeyboard = Keyboard [buttonsForMyKb] True False
    textForRepeat x =
      show (M.findWithDefault (сonfigNumberRepeat conf) x dict) ++
      messageForRepeat conf

helpBuildRequest :: Config -> VKItemMessage -> Request
helpBuildRequest conf event = setRequestQueryString qi $ parseRequest_ appVK
  where
    qi =
      [ ("user_id", Just (BC.pack $ show (m_from_id event)))
      , ("random_id", Just (BC.pack $ show (m_random_id event)))
      , ("message", Just (BC.pack $ messageForHelp conf))
      , ("access_token", Just (BC.pack $ сonfigToken conf))
      , ("v", Just "5.126")
      ]

getUserAndNumRep :: [UPD] -> [(Int, Int)]
getUserAndNumRep = map fgets
  where
    fgets ~(VK x) = (m_from_id $ getVkItemMessage x, payload x)
    payload x = read $ fromJust $ m_payload $ getVkItemMessage x :: Int
    fromJust ~(Just x) = x

getVkItemMessage :: Event -> VKItemMessage
getVkItemMessage e = m_message $ e_object e
