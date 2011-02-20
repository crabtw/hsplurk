module Web.Plurk.Types ( Return (..)
                       , Message (..)
                       , User (..)
                       , Privacy (..)
                       , Gender (..)
                       , Relationship (..)
                       , Qualifier (..)
                       , ReadStat (..)
                       , MsgType (..)
                       , CommentStat (..)
                       , KarmaStat (..)
                       , Profile (..)
                       , jsToInt
                       , jsToDouble
                       , jsToStr
                       , jsToBool
                       , jsToEnum
                       , jsToDateTime
                       , jsToList
                       , jsToMap
                       , jsToIntMap
                       , jsToErrorMsg
                       , jsToUser
                       , jsToMessage
                       , jsToKarmaStat
                       , jsToProfile
                       ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe
import Data.Ratio

import Data.Time
import Data.Time.Clock.POSIX

import System.Locale

import Text.JSON

data Return a = PData a | PSucc | PError String

data Message = Message { msg_plurk_id :: Int
                       , msg_qualifier :: Qualifier
                       , msg_is_unread :: ReadStat
                       , msg_plurk_type :: MsgType
                       , msg_user_id :: Int
                       , msg_owner_id :: Int
                       , msg_posted :: UTCTime
                       , msg_no_comments :: CommentStat
                       , msg_content :: String
                       , msg_content_raw :: String
                       , msg_response_count :: Int
                       , msg_responses_seen :: Int
                       , msg_limited_to :: [Int]
                       }
    deriving Show

data User = User { user_id :: Int
                 , user_nick_name :: String
                 , user_display_name :: String
                 , user_has_profile_image :: Bool
                 , user_avatar :: Int
                 , user_location :: String
                 , user_date_of_birth :: Day
                 , user_full_name :: String
                 , user_gender :: Gender
                 , user_page_title :: String
                 , user_karma :: Double
                 , user_recruited :: Int
                 , user_relationship :: Relationship
                 }
    deriving Show

data KarmaStat = KarmaStat { karma_stat_karma_trend :: [(UTCTime, Double)]
                           , karma_stat_karma_fall_reason :: String
                           , karma_stat_current_karma :: Double
                           , karma_stat_karma_graph :: String
                           }
    deriving Show

data Profile = Profile { profile_friends_count :: Int
                       , profile_fans_count :: Int
                       , profile_unread_count :: Int
                       , profile_alerts_count :: Int
                       , profile_are_friends :: Bool
                       , profile_is_fan :: Bool
                       , profile_is_following :: Bool
                       , profile_has_read_permission :: Bool
                       , profile_user_info :: User
                       , profile_privacy :: Privacy
                       , profile_plurks_users :: IntMap User
                       , profile_plurks :: [Message]
                       }
    deriving Show

data Privacy = World | OnlyFriends | OnlyMe

instance Show Privacy where
    show World = "world"
    show OnlyFriends = "only_friends"
    show OnlyMe = "only_me"

instance Read Privacy where
    readsPrec 0 "world" = [(World, "")]
    readsPrec 0 "only_friends" = [(OnlyFriends, "")]
    readsPrec 0 "only_me" = [(OnlyMe, "")]
    readsPrec _ _ = [(World, "")]

data Gender = Female | Male | Other
    deriving Enum

instance Show Gender where
    show Female = "female"
    show Male = "male"
    show Other = "other"

data Relationship = NotSaying
                  | Single
                  | Married
                  | Divorced
                  | Engaged
                  | InRelationship
                  | Complicated
                  | Widowed
                  | OpenRelationship

instance Show Relationship where
    show NotSaying = "not_saying"
    show Single = "single"
    show Married = "married"
    show Divorced = "divorced"
    show Engaged = "engaged"
    show InRelationship = "in_relationship"
    show Complicated = "complicated"
    show Widowed = "widowed"
    show OpenRelationship = "open_relationship"

instance Read Relationship where
    readsPrec 0 "not_saying" = [(NotSaying, "")]
    readsPrec 0 "single" = [(Single, "")]
    readsPrec 0 "married" = [(Married, "")]
    readsPrec 0 "divorced" = [(Divorced, "")]
    readsPrec 0 "engaged" = [(Engaged, "")]
    readsPrec 0 "in_relationship" = [(InRelationship, "")]
    readsPrec 0 "complicated" = [(Complicated, "")]
    readsPrec 0 "widowed" = [(Widowed, "")]
    readsPrec 0 "open_relationship" = [(OpenRelationship, "")]
    readsPrec _ _ = [(NotSaying, "")]

data Qualifier = Loves
               | Likes
               | Shares
               | Gives
               | Hates
               | Wants
               | Has
               | Will
               | Asks
               | Wishes
               | Was
               | Feels
               | Thinks
               | Says
               | Is
               | Colon
               | Freestyle
               | Hopes
               | Needs
               | Wonders

instance Show Qualifier where
    show Loves = "loves"
    show Likes = "likes"
    show Shares = "shares"
    show Gives = "gives"
    show Hates = "hates"
    show Wants = "wants"
    show Has = "has"
    show Will = "will"
    show Asks = "asks"
    show Wishes = "wishes"
    show Was = "was"
    show Feels = "feels"
    show Thinks = "thinks"
    show Says = "says"
    show Is = "is"
    show Colon = ":"
    show Freestyle = "freestyle"
    show Hopes = "hopes"
    show Needs = "needs"
    show Wonders = "wonders"

instance Read Qualifier where
    readsPrec 0 "loves" = [(Loves, "")]
    readsPrec 0 "likes" = [(Likes, "")]
    readsPrec 0 "shares" = [(Shares, "")]
    readsPrec 0 "gives" = [(Gives, "")]
    readsPrec 0 "hates" = [(Hates, "")]
    readsPrec 0 "wants" = [(Wants, "")]
    readsPrec 0 "has" = [(Has, "")]
    readsPrec 0 "will" = [(Will, "")]
    readsPrec 0 "asks" = [(Asks, "")]
    readsPrec 0 "wishes" = [(Wishes, "")]
    readsPrec 0 "was" = [(Was, "")]
    readsPrec 0 "feels" = [(Feels, "")]
    readsPrec 0 "thinks" = [(Thinks, "")]
    readsPrec 0 "says" = [(Says, "")]
    readsPrec 0 "is" = [(Is, "")]
    readsPrec 0 ":" = [(Colon, "")]
    readsPrec 0 "freestyle" = [(Freestyle, "")]
    readsPrec 0 "hopes" = [(Hopes, "")]
    readsPrec 0 "needs" = [(Needs, "")]
    readsPrec 0 "wonders" = [(Wonders, "")]
    readsPrec _ _ = [(Colon, "")]

data ReadStat = Read | UnRead | Muted
    deriving (Enum, Show)

data MsgType = Public | Private | PubLogged | PrivLogged
    deriving (Enum, Show)

data CommentStat = EnResp | DisResp | OnlyFriendResp
    deriving (Enum, Show)

jsToDouble :: JSValue -> Double
jsToDouble JSNull = 0
jsToDouble (JSRational _ n) = fromRational n
jsToDouble v = error $ "jsToDouble: " ++ show v

jsToInt :: JSValue -> Int
jsToInt JSNull = 0
jsToInt (JSRational _ n) = fromInteger $ numerator n
jsToInt v = error $ "jsToInt: " ++ show v

jsToStr :: JSValue -> String
jsToStr JSNull = ""
jsToStr (JSString s) = fromJSString s
jsToStr v = error $ "jsToStr: " ++ show v

jsToBool :: JSValue -> Bool
jsToBool JSNull = False
jsToBool (JSBool b) = b
jsToBool v = error $ "jsToBool: " ++ show v

jsToList :: JSValue -> [JSValue]
jsToList JSNull = []
jsToList (JSArray a) = a
jsToList v = error $ "jsToList: " ++ show v

jsToMap :: JSValue -> Map String JSValue
jsToMap JSNull = M.empty
jsToMap (JSObject o) = M.fromList $ fromJSObject o
jsToMap v = error $ "jsToMap: " ++ show v

jsToIntMap :: JSValue -> IntMap JSValue
jsToIntMap JSNull = IM.empty
jsToIntMap (JSObject o) = IM.fromList $ map (\(k, v) -> (read k, v))
                                      $ fromJSObject o
jsToIntMap v = error $ "jsToIntMap: " ++ show v

jsToEnum :: Enum a => JSValue -> a
jsToEnum = toEnum . jsToInt

jsToDateTime :: ParseTime a => JSValue -> Maybe a
jsToDateTime = parseTime defaultTimeLocale fmt . jsToStr
    where fmt = "%a, %d %b %Y %H:%M:%S %Z"

jsToErrorMsg :: JSValue -> String
jsToErrorMsg = jsToStr . M.findWithDefault JSNull "error_text" . jsToMap

getMapVal :: String -> Map String JSValue -> JSValue
getMapVal = M.findWithDefault JSNull

nullMsg = Message { msg_plurk_id = 0
                  , msg_qualifier = Colon
                  , msg_is_unread = Read
                  , msg_plurk_type = Public
                  , msg_user_id = 0
                  , msg_owner_id = 0
                  , msg_posted = UTCTime (ModifiedJulianDay 0)
                                         (secondsToDiffTime 0)
                  , msg_no_comments = EnResp
                  , msg_content = ""
                  , msg_content_raw = ""
                  , msg_response_count = 0
                  , msg_responses_seen = 0
                  , msg_limited_to = []
                  }

jsToMessage :: JSValue -> Message
jsToMessage obj = msg
    where msg = nullMsg { msg_plurk_id = jsToInt $ val "plurk_id"
                        , msg_qualifier = read $ jsToStr $ val "qualifier"
                        , msg_is_unread = jsToEnum $ val "is_unread"
                        , msg_plurk_type = jsToEnum $ val "plurk_type"
                        , msg_user_id = jsToInt $ val "user_id"
                        , msg_owner_id = jsToInt $ val "owner_id"
                        , msg_posted = maybeTime $ jsToDateTime $ val "posted"
                        , msg_no_comments = jsToEnum $ val "no_comments"
                        , msg_content = jsToStr $ val "content"
                        , msg_content_raw = jsToStr $ val "content_raw"
                        , msg_response_count = jsToInt $ val "response_count"
                        , msg_responses_seen = jsToInt $ val "responses_seen"
                        , msg_limited_to = toList $ jsToStr $ val "limited_to"
                        }
          val n = getMapVal n m
          m = jsToMap obj
          maybeTime = maybe (msg_posted nullMsg) id
          toList = go []
            where go ls [] = ls
                  go ls (_:ss) = let (uid, rest) = span (/= '|') ss
                                 in go (read uid : ls) (tail rest)

nullUser = User { user_id = 0
                , user_nick_name = ""
                , user_display_name = ""
                , user_has_profile_image = False
                , user_avatar = 0
                , user_location = ""
                , user_date_of_birth = ModifiedJulianDay 0
                , user_full_name = ""
                , user_gender = Female
                , user_page_title = ""
                , user_karma = 0
                , user_recruited = 0
                , user_relationship = NotSaying
                }

jsToUser :: JSValue -> User
jsToUser (JSObject o) = user
    where user = nullUser
                    { user_id = jsToInt $ val "id"
                    , user_nick_name = jsToStr $ val "nick_name"
                    , user_display_name = jsToStr $ val "display_name"
                    , user_has_profile_image = toEnum
                                                $ jsToInt
                                                $ val "has_profile_image"
                    , user_avatar = jsToInt $ val "avatar"
                    , user_gender = jsToEnum $ val "gender"
                    , user_location = jsToStr $ val "location"
                    , user_date_of_birth =
                        maybeDay $ jsToDateTime $ val "date_of_birth"
                    , user_full_name = jsToStr $ val "full_name"
                    , user_page_title = jsToStr $ val "page_title"
                    , user_karma = jsToDouble $ val "karma"
                    , user_recruited = jsToInt $ val "recruited"
                    , user_relationship = read $ jsToStr
                                               $ val "relationship"
                    }
          val n = getMapVal n m
          m = M.fromList $ fromJSObject o
          maybeDay = maybe (user_date_of_birth nullUser) id
jsToUser v = error $ "jsToUser: " ++ show v

nullKarmaStat = KarmaStat { karma_stat_karma_trend = []
                          , karma_stat_karma_fall_reason = ""
                          , karma_stat_current_karma = 0
                          , karma_stat_karma_graph = ""
                          }

jsToKarmaStat (JSObject o) = stat
    where stat = nullKarmaStat
                    { karma_stat_karma_trend = toStats $ val "karma_trend"
                    , karma_stat_karma_fall_reason =
                        jsToStr $ val "karma_fall_reason"
                    , karma_stat_current_karma =
                        jsToDouble $ val "current_karma"
                    , karma_stat_karma_graph = jsToStr $ val "karma_graph"
                    }
          toStats = map (strToStat . span (/= '-') . jsToStr) . jsToList
          strToStat (t, k) = (intToTime t, read $ tail k)
          intToTime = posixSecondsToUTCTime . fromInteger . read
          val n = getMapVal n m
          m = M.fromList $ fromJSObject o
jsToKarmaStat v = error $ "jsToKarmaStat: " ++ show v

nullProfile = Profile { profile_friends_count = 0
                      , profile_fans_count = 0
                      , profile_unread_count = 0
                      , profile_alerts_count = 0
                      , profile_are_friends = False
                      , profile_is_fan = False
                      , profile_is_following = False
                      , profile_has_read_permission = False
                      , profile_user_info = nullUser
                      , profile_privacy = World
                      , profile_plurks_users = IM.empty
                      , profile_plurks = []
                      }

jsToProfile own obj = if own then owner else public
    where owner = common { profile_unread_count = jsToInt $ val "unread_count"
                         , profile_alerts_count = jsToInt $ val "alerts_count"
                         , profile_plurks_users = IM.map jsToUser
                                                    $ jsToIntMap
                                                    $ val "plurks_users"
                         }
          public = common
                    { profile_are_friends = jsToBool $ val "are_friends"
                    , profile_is_fan = jsToBool $ val "is_fan"
                    , profile_is_following = jsToBool $ val "is_following"
                    , profile_has_read_permission =
                        jsToBool $ val "has_read_permission"
                    }
          common = nullProfile
                    { profile_friends_count = jsToInt $ val "friends_count"
                    , profile_fans_count = jsToInt $ val "fans_count"
                    , profile_user_info = jsToUser $ val "user_info"
                    , profile_privacy = read $ jsToStr $ val "privacy"
                    , profile_plurks = map jsToMessage $ jsToList
                                                       $ val "plurks"
                    }
          val n = getMapVal n m
          m = jsToMap obj
