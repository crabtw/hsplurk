module Web.Plurk.Client ( PM
                        , PReturn (..)
                        , APIKey
                        , liftIO
                        , runPlurk
                        , postData
                        , api
                        , apiSSL
                        ) where

import Network.Curl
import Text.JSON

type APIKey = String

type Resp = CurlResponse_ [(String, String)] String

data PReturn a = PSucc a | PErr a | PDecErr String

data PEnv = PEnv { apiKey :: APIKey
                 , handle :: Curl
                 }

newtype PM a = PM {unPM :: PEnv -> IO a}

instance Monad PM where
    return a = PM $ \_ -> return a
    m >>= f = PM $ \env -> do
        v <- unPM m env
        unPM (f v) env

liftIO :: IO a -> PM a
liftIO a = PM $ \_ -> a

apiBase = "www.plurk.com"
apiSSL s = "https://" ++ apiBase ++ s
api s = "http://" ++ apiBase ++ s

runPlurk :: APIKey -> PM a -> IO a
runPlurk key act = withCurlDo $ do
                    h <- initialize
                    let env = PEnv key h
                    setopts h opts
                    unPM act env
    where opts = method_POST ++ misc
          misc = [ CurlCookieFile ""
                 , CurlSSLVerifyPeer False
                 ]

postData :: URLString -> [(String, String)] -> PM (PReturn JSValue)
postData url ps = PM $ \env -> do
    let fields = genFields $ ("api_key", apiKey env) : ps
    resp <- do_curl_ (handle env) url [CurlPostFields fields] :: IO Resp
    let result = decodeStrict $ respBody resp
        retVal = if respStatus resp == 200
                    then PSucc else PErr
    return $ checkResult retVal result

genFields :: [(String, String)] -> [String]
genFields = map (\(k, v) -> k ++ "=" ++ v)

checkResult :: (JSValue -> PReturn JSValue) -> Result JSValue
                                            -> PReturn JSValue
checkResult retVal result = case result of
                                Ok json -> retVal json
                                Error msg -> PDecErr msg
