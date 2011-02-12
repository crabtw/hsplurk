module Web.Plurk.Profile ( getOwnProfile
                         ) where

import Text.JSON

import Web.Plurk.Client

getOwnProfile :: JSON a => PM (PReturn a)
getOwnProfile = do
    let url = api "/API/Profile/getOwnProfile"
    postData [] url >>= return
