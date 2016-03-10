{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric, DeriveAnyClass #-}

-- | 

module TestApi where

import WebApi
import GHC.Generics
import Data.Text
import Data.Aeson
import Data.Proxy
import WebApi.Console

data TestApp

instance WebApi TestApp where
  type Version TestApp = MajorMinor '(0, 1)
  type Apis TestApp = '[ Route '[GET] UserR
                       , Route '[GET] ProfileR
                       ]
type UserR = "api":/"user":/Int
type ProfileR = "api":/Int:/"profile":/Bool

data UserQuery = UserQuery
  { qUserId :: Maybe Int
  , qUserName :: Text
  , qUserBool :: Bool
  } deriving (Show, Eq, Generic, ToJSON)

data User = User
  { userId :: Int
  , userName :: Text
  , userAge :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToParam UserQuery 'QueryParam
instance ToParam User 'QueryParam
  
instance ApiContract TestApp GET UserR where
  type QueryParam GET UserR = UserQuery
  type ApiOut GET UserR = [User]

instance ApiContract TestApp GET ProfileR where
  type QueryParam GET ProfileR = User
  type ApiOut GET ProfileR = ()

instance ToWidget UserQuery
instance ToWidget User

consoleApp :: IO ()
consoleApp = apiConsole (ConsoleConfig undefined) (Proxy :: Proxy TestApp)
