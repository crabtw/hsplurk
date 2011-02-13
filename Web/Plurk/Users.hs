module Web.Plurk.Users ( LoginOpt (..)
                       , login
                       , logout
                       ) where

import Text.JSON

import Web.Plurk.Client
import Web.Plurk.Types

data LoginParam = LoginParam { loginUsername :: String
                             , loginPassword :: String   
                             , loginOpt :: LoginOpt
                             }

data LoginOpt = LoginOpt { loginNoData :: Maybe Bool }

instance Param LoginParam where
    toStrList LoginParam { loginUsername = user
                         , loginPassword = pw
                         , loginOpt = opt
                         } = [("username", user), ("password", pw)] ++ noData
        where noData = maybe [] (\n -> [("no_data", show $ fromEnum n)]) $ loginNoData opt

instance Opt LoginOpt where
    opt = LoginOpt Nothing

login :: JSON a => String -> String -> LoginOpt -> PM (PReturn a)
login user pw opt = do
    let url = apiSSL "/API/Users/login"
    let ps = toStrList $ LoginParam user pw opt
    resp <- postData url ps
    return resp

logout :: JSON a => PM (PReturn a)
logout = do
    let url = api "/API/Users/logout"
    postData url [] >>= return
