module VK.Internal where


import qualified Data.Map as M

import Bot
import Config
import VK.Data
import MapR

forCopy :: [UPD] -> Config -> MapInt -> [UPD]
forCopy xs conf dict = map VK $ concat (map repeating (filtred xs))
    where
      filtred xs = filter (\(VK x) -> (not 
        (m_text (getVk_ItemMessage x) == "/repeat" ||
         m_text (getVk_ItemMessage x) == "/help")) && 
         m_payload (getVk_ItemMessage x) == Nothing) xs
      repeating (VK x) = take (numRepeat x) $ repeat x
      numRepeat x = M.findWithDefault (сonfigNumberRepeat conf)
                                      (m_from_id (getVk_ItemMessage x)) dict
                                      
getVk_ItemMessage :: Event -> Vk_ItemMessage
getVk_ItemMessage e = m_message $ e_object e
