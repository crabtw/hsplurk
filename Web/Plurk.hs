module Web.Plurk ( PM
                 , APIKey
                 , runPlurk
                 , login
                 , logout
                 ) where

import qualified Data.ByteString as B
import Network.Curl

type APIKey = String

type Resp = CurlResponse_ [(String, String)] String

data PEnv = PEnv { apiKey :: APIKey
                 , handle :: Curl
                 }

newtype PM a = PM {unPM :: PEnv -> IO a}

instance Monad PM where
    return a = PM $ \_ -> return a
    m >>= f = PM $ \env -> do
        v <- unPM m env
        unPM (f v) env

api p = "https://www.plurk.com" ++ p

runPlurk :: FilePath -> APIKey -> PM a -> IO a
runPlurk path key act = withCurlDo $ do
                            h <- initialize
                            let env = PEnv key h
                            setopts h opts
                            unPM act env
    where opts = method_POST ++ misc
          misc = [ CurlCookieJar path
                 , CurlSSLVerifyPeer False
                 ]

login :: String -> String -> PM ()
login user pw = do
    let url = api "/API/Users/login"
    let ps = [("username", user), ("password", pw)]
    postData ps url
    return ()

logout :: PM ()
logout = do
    let url = api "/API/Users/logout"
    postData [] url
    return ()

postData :: [(String, String)] -> URLString -> PM String
postData ps url = PM $ \env -> do
    let fields = genFields $ ("api_key", apiKey env) : ps
    resp <- do_curl_ (handle env) url [CurlPostFields fields] :: IO Resp
    return $ respBody resp

genFields :: [(String, String)] -> [String]
genFields = map (\(k, v) -> k ++ "=" ++ v)
