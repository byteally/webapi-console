{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric, DeriveAnyClass #-}

-- |

module TestApi where

import WebApi
import GHC.Generics
import Data.Text
import Data.Aeson
import Data.Proxy
import WebApi.Console
import Network.URI

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
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data User = User
  { userId :: Int
  , userName :: [Text]
  , userAge :: Int
  , userType :: UserType
  , userQuery :: UserQuery
  , location :: LatLng
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data LatLng = LatLng
  { lat :: Int
  , lng :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data UserType = Nobody Int | Normal Int | Admin Int
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToParam UserType 'QueryParam
instance ToParam LatLng 'QueryParam

instance ToParam UserQuery 'QueryParam
instance ToParam User 'QueryParam

instance ApiContract TestApp GET UserR where
  type QueryParam GET UserR = UserQuery
  type ApiOut GET UserR = User

instance ApiContract TestApp GET ProfileR where
  type QueryParam GET ProfileR = User
  type ApiOut GET ProfileR = ()

instance ToWidget UserType
instance AssertWidget UserType
instance SelectorName UserType
instance ToWidget UserQuery
instance AssertWidget UserQuery
instance SelectorName UserQuery
instance ToWidget User
instance AssertWidget User
instance SelectorName User
instance ToWidget LatLng
instance AssertWidget LatLng
instance SelectorName LatLng

consoleApp :: IO ()
consoleApp = apiConsole (ConsoleConfig (URI "http:" (Just (URIAuth "" "192.168.1.14" ":9000")) "" "" "")) (Proxy :: Proxy TestApp)
