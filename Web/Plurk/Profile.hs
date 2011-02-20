module Web.Plurk.Profile ( getOwnProfile
                         , getPublicProfile
                         ) where

import Web.Plurk.Client
import Web.Plurk.Types
import Web.Plurk.Utils

getOwnProfile :: PM (Return Profile)
getOwnProfile = do
    let url = api "/API/Profile/getOwnProfile"
    result <- postData url []
    retMapJs (jsToProfile True) result

getPublicProfile :: Either String Int -> PM (Return Profile)
getPublicProfile user = do
    let url = api "/API/Profile/getPublicProfile"
        user' = either id show user
        params = [("user_id", user')]
    result <- postData url params
    retMapJs (jsToProfile False) result
