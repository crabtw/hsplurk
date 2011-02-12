module Web.Plurk.Users ( login
                       , logout
                       ) where

import Text.JSON

import Web.Plurk.Client

login :: JSON a => String -> String -> PM (PReturn a)
login user pw = do
    let url = apiSSL "/API/Users/login"
    let ps = [("username", user), ("password", pw)]
    resp <- postData ps url
    return resp

logout :: JSON a => PM (PReturn a)
logout = do
    let url = api "/API/Users/logout"
    postData [] url >>= return
