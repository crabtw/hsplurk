module Web.Plurk.Types where

class Param a where
    toStrList :: a -> [(String, String)]

class Opt a where
    opt :: a

