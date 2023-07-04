{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module BasicSchema where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Database.Persist.TH as PTH
import           Data.Text
import           GHC.Generics (Generic)

-- THis will help us create the schema, it will automatically create the tables for us when we run migrate command, table name will be users
PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User sql=users
    name Text
    email Text
    age Int
    occupation Text
    UniqueEmail email
    deriving Show Read
|]

--  to json from json
instance ToJSON User where
  toJSON :: User -> Value
  toJSON user = object 
    [ "name" .= userName user
    , "email" .= userEmail user
    , "age" .= userAge user
    , "occupation" .= userOccupation user
    ]


instance FromJSON User where
  parseJSON :: Value -> Parser User
  parseJSON = withObject "User" parseUser

parseUser :: Object -> Parser User
parseUser o = do
  uName <- o .: "name"
  uEmail <- o .: "email"
  uAge <- o .: "age"
  uOccupation <- o .: "occupation"
  return User
    { userName = uName
    , userEmail = uEmail
    , userAge = uAge
    , userOccupation = uOccupation
    }

-- response type
data SuccessResponse = SuccessResponse
  {
    message :: String
  } deriving (Generic, Show)

instance ToJSON SuccessResponse
instance FromJSON SuccessResponse

data UserRequest = UserRequest
  {
     users :: [User]
  } 
  deriving (Generic, Show)

instance ToJSON UserRequest
instance FromJSON UserRequest

logValue :: (Show a) => a -> IO ()
logValue value = print $ show value
