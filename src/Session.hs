{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

--
module Session where

import Data.Aeson
import GHC.Generics

data Session =
  Session
    { key :: String
    , server :: String
    , ts :: String
    }
  deriving (FromJSON, Show, Generic)
