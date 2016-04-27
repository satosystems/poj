{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    ) where

import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Text as ST
import Debug.Trace
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

data Entry = Entry
  { prefecture :: String
  , time       :: Float
  , session    :: String
  } deriving (Eq, Show)

data EnterResult = EnterResult
  { result :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Entry)
$(deriveJSON defaultOptions ''EnterResult)

type API = "users" :> Get '[JSON] [User]
      :<|> "entry" :> ReqBody '[JSON] Entry :> Post '[JSON] EnterResult

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users
    :<|> entry'
 where
  entry' e = liftIO . atomically $ do
    pure $ entry e

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]

entry :: Entry -> EnterResult
entry e | time e > 10.0                   = EnterResult "timeout"
        | prefecture e `elem` prefectures = EnterResult "right"
        | otherwise                       = EnterResult "wrong"

prefectures :: [String]
prefectures =
  [ "北海道"
  , "青森県"
  , "岩手県"
  , "宮城県"
  , "秋田県"
  , "山形県"
  , "福島県"
  , "茨城県"
  , "栃木県"
  , "群馬県"
  , "埼玉県"
  , "千葉県"
  , "東京都"
  , "神奈川県"
  , "新潟県"
  , "富山県"
  , "石川県"
  , "福井県"
  , "山梨県"
  , "長野県"
  , "岐阜県"
  , "静岡県"
  , "愛知県"
  , "三重県"
  , "滋賀県"
  , "京都府"
  , "大阪府"
  , "兵庫県"
  , "奈良県"
  , "和歌山県"
  , "鳥取県"
  , "島根県"
  , "岡山県"
  , "広島県"
  , "山口県"
  , "徳島県"
  , "香川県"
  , "愛媛県"
  , "高知県"
  , "福岡県"
  , "佐賀県"
  , "長崎県"
  , "熊本県"
  , "大分県"
  , "宮崎県"
  , "鹿児島県"
  , "沖縄県"
  ]

