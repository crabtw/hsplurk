module Web.Plurk.Types ( PData (..)
                       , PPriv (..)
                       , PGender (..)
                       , PRelation (..)
                       , PQualifier (..)
                       , PReadStat (..)
                       , PMsgType (..)
                       , PNoComment (..)
                       , jsonToPData
                       ) where

import Data.Array
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import System.Locale
import Text.JSON

data PData = PInt Int
           | PStr String
           | PBool Bool
           | PDouble Double
           | PDate Day
           | PTime UTCTime
           | PMap (Map String PData)
           | PArray [PData]
           | PNoData PNoData
           | PPriv PPriv
           | PGender PGender
           | PReadStat PReadStat
           | PMsgType PMsgType
           | PCommentStat PCommentStat

data PPriv = World | OnlyFriends | OnlyMe

instance Show PPriv where
    show World = "world"
    show OnlyFriends = "only_friends"
    show OnlyMe = "only_me"

instance Read PPriv where
    readsPrec 0 "world" = [(World, "")]
    readsPrec 0 "only_friends" = [(OnlyFriends, "")]
    readsPrec 0 "only_me" = [(OnlyMe, "")]
    readsPrec _ _ = []

data PGender = Female | Male | Other

instance Enum PGender where
    fromEnum Female = 0
    fromEnum Male = 1
    fromEnum Other = 2
    toEnum 0 = Female
    toEnum 1 = Male
    toEnum 2 = Other

instance Show PGender where
    show Female = "female"
    show Male = "male"
    show Other = "other"

data PRelation = NotSaying
               | Single
               | Married
               | Divorced
               | Engaged
               | InRelationship
               | Complicated
               | Widowed
               | OpenRelationship

instance Show PRelation where
    show NotSaying = "not_saying"
    show Single = "single"
    show Married = "married"
    show Divorced = "divorced"
    show Engaged = "engaged"
    show InRelationship = "in_relationship"
    show Complicated = "complicated"
    show Widowed = "widowed"
    show OpenRelationship = "open_relationship"

instance Read PRelation where
    readsPrec 0 "not_saying" = NotSaying
    readsPrec 0 "single" = Single
    readsPrec 0 "married" = Married
    readsPrec 0 "divorced" = Divorced
    readsPrec 0 "engaged" = Engaged
    readsPrec 0 "in_relationship" = InRelationship
    readsPrec 0 "complicated" = Complicated
    readsPrec 0 "widowed" = Widowed
    readsPrec 0 "open_relationship" = OpenRelationship
    readsPrec _ _ = []

data PQualifier = Loves
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

instance Show PQualifier where
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

instance Read PQualifier where
    readsPrec 0 "loves" = Loves
    readsPrec 0 "likes" = Likes
    readsPrec 0 "shares" = Shares
    readsPrec 0 "gives" = Gives
    readsPrec 0 "hates" = Hates
    readsPrec 0 "wants" = Wants
    readsPrec 0 "has" = Has
    readsPrec 0 "will" = Will
    readsPrec 0 "asks" = Asks
    readsPrec 0 "wishes" = Wishes
    readsPrec 0 "was" = Was
    readsPrec 0 "feels" = Feels
    readsPrec 0 "thinks" = Thinks
    readsPrec 0 "says" = Says
    readsPrec 0 "is" = Is
    readsPrec 0 ":" = Colon
    readsPrec 0 "freestyle" = Freestyle
    readsPrec 0 "hopes" = Hopes
    readsPrec 0 "needs" = Needs
    readsPrec 0 "wonders" = Wonders
    readPrec _ _ = []

data PReadStat = Read | UnRead | Muted

instance Enum PReadStat where
    fromEnum Read = 0
    fromEnum UnRead = 1
    fromEnum Muted = 2
    toEnum 0 = Read
    toEnum 1 = UnRead
    toEnum 2 = Muted

data PMsgType = Public | Private | PubLogged | PrivLogged

instance Enum PMsgType where
    fromEnum Public = 0
    fromEnum Private = 1
    fromEnum PubLogged = 2
    fromEnum PrivLogged = 3
    toEnum 0 = Public
    toEnum 1 = Private
    toEnum 2 = PubLogged
    toEnum 3 = PrivLogged

data PCommentStat = EnResp | DisResp | OnlyFriendResp

instance Enum PNoComment where
    fromEnum EnResp = 0
    fromEnum DisResp = 1
    fromEnum OnlyFriendResp = 2
    toEnum 0 = EnResp
    toEnum 1 = DisResp
    toEnum 2 = OnlyFriendResp

jsonToPData :: JSValue -> PData
jsonToPData JSNull = PNull
jsonToPData (JSBool b) = PBool b
jsonToPData (JSRational _ r) = if denominator r == 1
                                then PInt $ fromInteger $ numerator r
                                else PDouble $ fromRational r
jsonToPData (JSString s) = PStr $ fromJSString s
jsonToPData (JSArray a) = PArray $ map jsonToPData a
jsonToPData (JSObject o) = PMap $ M.fromList $ map conv $ fromJSObject o
    where conv (k, v) = func v
        where func = findWithDefault (\_ -> PNull) k convAttr

convAttr :: Map String (JSValue -> PData)
convAttr = fromList [ ("plurk_id", jsonToPData)
                    , ("qualifier", PQualifier . read)
                    , ("is_unread", PReadStat . jsonToEnum)
                    , ("plurk_type", PMsgType . jsonToEnum)
                    , ("user_id", jsonToPData)
                    , ("owner_id", jsonToPData)
                    , ("posted", PTime jsonToTime)
                    , ("no_comments", PCommentStat . jsonToEnum)
                    , ("content", jsonToPData)
                    , ("content_raw", jsonToPData)
                    , ("response_count", jsonToPData)
                    , ("response_seen", jsonToPData)
                    , ("limit_to", jsonToPData)
                    , ("id", jsonToPData)
                    , ("nick_name", jsonToPData)
                    , ("display_name", jsonToPData)
                    , ("has_profile_image", PBool . jsonToEnum)
                    , ("avatar", jsonToPData)
                    , ("location", jsonToPData)
                    , ("date_of_birth", PDate . jsonToDate)
                    , ("full_name", jsonToPData)
                    , ("gender", PGender . jsonToEnum)
                    , ("page_title", jsonToPData)
                    , ("karma", jsonToPData)
                    , ("recruited", jsonToPData)
                    , ("relationship", PRelation . jsonToEnum)
                    , ("success_text", jsonToPData)
                    , ("error_text", jsonToPData)
                    ]
    where jsonToEnum = toEnum . (\PInt i -> i) . jsonToPData
          jsonToTime = jsonToDateTime "%a, %d %b %Y %H:%M:%S %Z"
          jsonToDate = jsonToDateTime "%Y-%m-%d"
          jsonToDateTime fmt = readTime defaultTimeLocale fmt . (\PStr s -> s)
                                                              . jsonToPData
