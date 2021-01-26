{-# LANGUAGE OverloadedStrings #-}

module VK where

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import           Data.Aeson

-- import DataVk

loopVk :: IO () 
loopVk= do 
    print "Bot vk in development"
