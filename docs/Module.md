# Module Documentation

## Module Web.Cookie

### Types

    data Cookie :: !

    type SetCookie  = { setCookieSecure :: Boolean, setCookieHttpOnly :: Boolean, setCookieDomain :: Maybe String, setCookieMaxAge :: Maybe Number, setCookieExpires :: Maybe Date, setCookiePath :: Maybe String, setCookieValue :: String, setCookieName :: String }


### Values

    defaultSetCookie :: SetCookie

    getCookie :: forall eff. String -> Eff (cookie :: Cookie | eff) (Maybe String)

    renderSetCookie :: SetCookie -> String

    setCookie :: forall eff. SetCookie -> Eff (cookie :: Cookie | eff) Unit

    setCookieString :: forall eff. String -> Eff (cookie :: Cookie | eff) Unit