module  Main where

import TlTest
import VKTest
import BotTest

main :: IO ()
main = do
    botTest
    tlTest
    vkTest
    
