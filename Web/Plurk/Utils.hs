module Web.Plurk.Utils ( retMapJs
                       , retUnit
                       , optParam
                       ) where

import Text.JSON

import Web.Plurk.Client
import Web.Plurk.Types

retMapJs :: (JSValue -> a) -> PostResult -> PM (Return a)
retMapJs f (Succ js) = return $ PData $ f js
retMapJs _ (PlurkError js) = return $ PError $ jsToErrorMsg js
retMapJs _ (DecodeError msg) = return $ PError msg

retUnit :: PostResult -> PM (Return a)
retUnit (Succ _) = return PSucc
retUnit (PlurkError js) = return $ PError $ jsToErrorMsg js
retUnit (DecodeError msg) = return $ PError msg

optParam :: (a -> String) -> String -> (Maybe a) -> [(String, String)]
optParam f n = maybe [] (\a -> [(n, f a)])
