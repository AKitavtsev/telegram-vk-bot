{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  Main where

import TlTest
import VKTest
import DictionaryTest

main :: IO ()
main = do
    dictionaryTest
    tlTest
    vkTest
    
