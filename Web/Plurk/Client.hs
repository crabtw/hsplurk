module Web.Plurk.Client ( PM
                        , PostResult (..)
                        , APIKey
                        , liftIO
                        , runPlurk
                        , postData
                        , api
                        , apiSSL
                        ) where

import Data.Bits
import Data.Word
import qualified Codec.Binary.UTF8.String as U
import Network.Curl
import Text.JSON

type APIKey = String

type Resp = CurlResponse_ [(String, String)] String

data PostResult = Succ JSValue
                | PlurkError JSValue
                | DecodeError String

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
    where opts = [ CurlCookieFile ""
                 , CurlSSLVerifyPeer False
                 ] ++ method_POST

postData :: URLString -> [(String, String)] -> PM PostResult
postData url ps = PM $ \env -> do
    let fields = genFields $ ("api_key", apiKey env) : ps
        opts = [CurlPostFields fields]
    resp <- do_curl_ (handle env) url opts :: IO Resp
    let result = decodeStrict $ respBody resp
        retVal = case result of
                    Ok res -> if respStatus resp == 200
                                then Succ res
                                else PlurkError res
                    Error msg -> DecodeError msg
    return retVal

genFields :: [(String, String)] -> [String]
genFields = map (\(k, v) -> k ++ "=" ++ urlEncode v)

urlEncode :: String -> String
urlEncode = U.decode . go  . U.encode
    where go [] = []
          go (32:cs) = 43 : go cs
          go (c:cs) = if unreserved c
                        then c : go cs
                        else let x = shiftR c 4
                                 y = c .&. 15
                             in 37 : hex x : hex y : go cs
          hex c | c < 10 = c + 48
                | c < 16 = c + 55
                | otherwise = error $ "hex: " ++ show c

unreserved :: Word8 -> Bool
unreserved  45 = True
unreserved  46 = True
unreserved  95 = True
unreserved 126 = True
unreserved c
    | 48 <= c && c <= 57  = True
    | 65 <= c && c <= 90  = True
    | 97 <= c && c <= 122 = True
unreserved _ = False
