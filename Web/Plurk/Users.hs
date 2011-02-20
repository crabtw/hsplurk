module Web.Plurk.Users ( login
                       , logout
                       , register
                       , getKarmaStats
                       , update
                       ) where

import Data.Time

import Web.Plurk.Client
import Web.Plurk.Types
import Web.Plurk.Utils

register :: String -> String -> String -> Gender -> Day
         -> Maybe String -> PM (Return User)
register nick full pw gender birth email = do
    let url = apiSSL "API/Users/register"
        email' = optParam id "email" email
        params = [ ("nick_name", nick)
                 , ("full_name", full)
                 , ("password", pw)
                 , ("gender", show gender)
                 , ("date_of_birth", show birth)
                 ] ++ email'
    result <- postData url params
    retMapJs jsToUser result

login :: String -> String -> Maybe Bool -> PM (Return Profile)
login user pw noData = do
    let url = apiSSL "/API/Users/login"
        noData' = optParam (show . fromEnum) "no_data" noData
        params = [("username", user), ("password", pw)] ++ noData'
    result <- postData url params
    if maybe False id noData
        then retUnit result
        else retMapJs (jsToProfile True) result

logout :: PM (Return ())
logout = do
    let url = api "/API/Users/logout"
    result <- postData url []
    retUnit result

update :: String -> Maybe String -> Maybe String -> Maybe String
        -> Maybe String -> Maybe Privacy -> Maybe Day -> PM (Return User)
update pw full newPw email display priv birth = do
    let url = apiSSL "/API/Users/update"
        full' = optParam id "full_name" full
        newPw' = optParam id "new_password" newPw
        email' = optParam id "email" email
        display' = optParam id "display_name" display
        priv' = optParam show "privacy" priv
        birth' = optParam show "date_of_birth" birth
        params = [("password", pw)] ++ full'
                                    ++ newPw'
                                    ++ email'
                                    ++ display'
                                    ++ priv'
                                    ++ birth'
    result <- postData url params
    retMapJs jsToUser result

getKarmaStats :: PM (Return KarmaStat)
getKarmaStats = do
    let url = api "/API/Users/getKarmaStats"
    result <- postData url []
    retMapJs jsToKarmaStat result
