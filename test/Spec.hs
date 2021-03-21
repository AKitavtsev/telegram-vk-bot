{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns #-}

module  Main where

import TlTest
import VKTest
import MapRTest

main :: IO ()
main = do
    mapRTest
    tlTest
    vkTest
    
