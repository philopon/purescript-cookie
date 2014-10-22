module Web.Cookie
    ( Cookie(), SetCookie(..)
    , setCookie, defaultSetCookie
    , renderSetCookie, setCookieString
    , getCookie
    ) where

import Control.Monad.Eff

import Data.Maybe
import Data.Date
import Data.Function

type SetCookie =
    { setCookieName     :: String
    , setCookieValue    :: String
    , setCookiePath     :: Maybe String
    , setCookieExpires  :: Maybe Date
    , setCookieMaxAge   :: Maybe Number
    , setCookieDomain   :: Maybe String
    , setCookieHttpOnly :: Boolean
    , setCookieSecure   :: Boolean
    }

defaultSetCookie :: SetCookie
defaultSetCookie = 
    { setCookieName:     "name"
    , setCookieValue:    "value"
    , setCookiePath:     Nothing
    , setCookieExpires:  Nothing
    , setCookieMaxAge:   Nothing
    , setCookieDomain:   Nothing
    , setCookieHttpOnly: false
    , setCookieSecure:   false
    }

foreign import jsDateToUTCString "\
    \function jsDateToUTCString (d) {\
    \   return d.toUTCString();\
    \}" :: JSDate -> String

foreign import data Cookie :: !

renderSetCookie :: SetCookie -> String
renderSetCookie s =
    httpOnly $ secure $ domain $ maxAge $ expires $ path $
    s.setCookieName ++ "=" ++ s.setCookieValue

  where
    path = case s.setCookiePath of
        Nothing -> id
        Just p  -> \i -> i ++ "; path=" ++ p
    expires = case s.setCookieExpires of
        Nothing -> id
        Just e  -> \i -> i ++ "; expires=" ++ jsDateToUTCString (toJSDate e)
    maxAge  = case s.setCookieMaxAge of
        Nothing -> id
        Just a  -> \i -> i ++ "; max-age=" ++ show (a :: Number)
    domain = case s.setCookieDomain of
        Nothing -> id
        Just d  -> \i -> i ++ "; domain=" ++ d
    httpOnly = case s.setCookieHttpOnly of
        true  -> \i -> i ++ "; httpOnly"
        false -> id
    secure   = case s.setCookieSecure of
        true  -> \i -> i ++ "; secure"
        false -> id

foreign import setCookieImpl "\
    \function setCookieImpl (s) {\
    \   return function () {\
    \       document.cookie = s; \
    \   }\
    \}" :: forall eff. String -> Eff (cookie :: Cookie | eff) Unit

setCookieString :: forall eff. String -> Eff (cookie :: Cookie | eff) Unit
setCookieString = setCookieImpl

setCookie :: forall eff. SetCookie -> Eff (cookie :: Cookie | eff) Unit
setCookie sc = setCookieString (renderSetCookie sc)

foreign import getCookieImpl "\
    \function getCookieImpl(conf) {\
    \   function sanitizeCookieValueImpl(conf, s) {\
    \       if (s.indexOf('\"') === 0) {\
    \           s = s.slice(1, -1).replace(/\\\\\"/g, '\"').replace(/\\\\\\\\/g, '\\\\');\
    \       }\
    \\
    \       try {\
    \           s = decodeURIComponent(s.replace(/\\+/g, ' '));\
    \           return conf.just(s);\
    \       } catch (e) {return conf.nothing}\
    \   }\
    \\
    \   return function () {\
    \       var result = conf.nothing;\
    \       var cookies = document.cookie ? document.cookie.split('; ') : [];\
    \\
    \       for (var i = 0; i < cookies.length; i++) {\
    \           var parts  = cookies[i].split('=');\
    \           var name   = parts.shift();\
    \           var cookie = parts.join('=');\
    \           if (conf.key == name) {\
    \               result = sanitizeCookieValueImpl(conf, cookie);\
    \               break;\
    \           }\
    \       }\
    \       return result;\
    \   }\
    \}" :: forall eff. { key     :: String
                       , nothing :: Maybe String
                       , just    :: String -> Maybe String
                       } -> Eff (cookie :: Cookie | eff) (Maybe String)

getCookie :: forall eff. String -> Eff (cookie :: Cookie | eff) (Maybe String)
getCookie key = getCookieImpl {key: key, nothing: Nothing, just: Just}
