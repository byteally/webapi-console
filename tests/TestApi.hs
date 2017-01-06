{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, FlexibleInstances, MultiParamTypeClasses, DeriveGeneric, DeriveAnyClass, TemplateHaskell, OverloadedStrings #-}

-- |

module TestApi where

import WebApi
import GHC.Generics
import Data.Text
import Data.Aeson
import Data.Proxy
import WebApi.Console hiding (functions)
import Network.URI
import WebApi.Console.TH

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
  , userName :: [Locations]
  , userAge :: Int
  , userType :: UserType
  , userQuery :: UserQuery
  , location :: LatLng
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Locations = Locations
  { locations :: [LatLng]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data LatLng = LatLng
  { lat :: Int
  , lng :: Int
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data UserType = Nobody {nbId :: Int} | Normal {normalId :: Int} | Admin {adminId :: Int}
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance ToParam 'QueryParam UserType
instance ToParam 'QueryParam LatLng

instance ToParam 'QueryParam UserQuery
instance ToParam 'QueryParam User

instance ToParam 'QueryParam Locations

instance ApiContract TestApp GET UserR where
  type QueryParam GET UserR = UserQuery
  type ApiOut GET UserR = User

instance ApiContract TestApp GET ProfileR where
  type QueryParam GET ProfileR = User
  type ApiOut GET ProfileR = ()

instance ToWidget UserType
instance AssertWidget UserType
instance SelectorInfo UserType
instance ToWidget UserQuery
instance AssertWidget UserQuery
instance SelectorInfo UserQuery
instance ToWidget User
instance AssertWidget User
instance SelectorInfo User
instance ToWidget LatLng
instance AssertWidget LatLng
instance SelectorInfo LatLng
instance ToWidget Locations
instance AssertWidget Locations
instance SelectorInfo Locations

consoleApp :: IO ()
-- consoleApp = apiConsole (ConsoleConfig (URI "http:" (Just (URIAuth "" "localhost" ":9000")) "" "" "") $(functions [('(&&), Just "And")]) ) (Proxy :: Proxy TestApp)
consoleApp = apiConsole (ConsoleConfig (URI "http:" (Just (URIAuth "" "localhost" ":9000")) "" "" "") (ConsoleFunctions funTable) ) (Proxy :: Proxy TestApp)

{-
and_ :: (GName, PrjFnInfo)
and_ = $(getFunInfoQ' (Just "And") '(&&))

foo = $(assertFunctionsE [ ('(&&), Just "And")
                         , ('(&&), Just "Or")
                         ])
-}

{-
base : Data.Bool
--------------
(&&) :: Bool -> Bool -> Bool
(||) :: Bool -> Bool -> Bool

base : Data.Eq
--------------
(==) :: a -> a -> Bool
(/=) :: a -> a -> Bool

base: Data.Ord
---------------
(<) :: a -> a -> Bool
(<=) :: a -> a -> Bool
(>) :: a -> a -> Bool
(>=) :: a -> a -> Bool

bytestring : Data.ByteString[.Lazy]
----------------------------
isPrefixOf :: ByteString -> ByteString -> Bool
isSuffixOf :: ByteString -> ByteString -> Bool
isInfixOf :: ByteString -> ByteString -> Bool

text :  Data.Text[.Lazy]
-----------------------
isPrefixOf :: Text -> Text -> Bool
isSuffixOf :: Text -> Text -> Bool
isInfixOf :: Text -> Text -> Bool
null :: Text -> Bool

vector : Data.Vector
-------------------
null :: Vector -> Bool
elem :: Eq a => a -> Vector a -> Bool
notElem :: Eq a => a -> Vector a -> Bool
and :: Vector Bool -> Bool
or :: Vector Bool -> Bool
-}
