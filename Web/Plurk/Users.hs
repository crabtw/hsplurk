module Web.Plurk.Users ( login
                       , logout
                       ) where

import Text.JSON

import Web.Plurk.Client
import Web.Plurk.Types

login :: JSON a => String -> String -> Maybe Bool -> PM (PReturn a)
login user pw noData = do
    let url = apiSSL "/API/Users/login"
        ps = [("username", user), ("password", pw)] ++ no
        no = maybe [] (\n -> [("no_data", show $ fromEnum n)]) noData
    resp <- postData url ps :: PM JSValue
    return resp

logout :: JSON a => PM (PReturn a)
logout = do
    let url = api "/API/Users/logout"
    postData url [] >>= return
