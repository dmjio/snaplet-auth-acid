# snaplet-auth-acid

An AcidState backend for Snap's Auth Snaplet

```text
cabal update && cabal install snaplet-auth-acid
```

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

-- imports..
import           Snap.Snaplet.Auth.Backends.Acid             (initAcidAuthManager)

data App = App { _heist :: Snaplet (Heist App)
               , _sess  :: Snaplet SessionManager
               , _auth  :: Snaplet (AuthManager App)
               }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

-- handles, forms and routes go here...

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $ initAcidAuthManager defAuthSettings sess 
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing app
  quickHttpServe site

```
