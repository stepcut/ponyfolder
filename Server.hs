{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, EmptyDataDecls, TemplateHaskell #-}
module Server (
        PonyServerPartT, PonyServerPart, runPonyServer,
        lookUserId, authenticated, logoutUser,
        UserId, throwError, catchError, mplus, msum, mzero
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.RWS
import Control.Monad.State
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Char8 (pack, unpack)
import Data.Data (Data, Typeable)
import Data.Default
import Data.SafeCopy (base, deriveSafeCopy, SafeCopy(..))
import Happstack.Server
import Happstack.Server.Error
import Happstack.Server.Compression
import System.IO.Pool
import System.IO.Error
import Web.ClientSession (getDefaultKey, encryptIO, decrypt, Key)
import Data.Maybe (isJust, fromMaybe)
import Control.Exception (bracket)

import Model.User

instance Error Text where
    noMsg = "No Message"
    strMsg = T.pack

newtype PonySession = PonySession { sessUid :: Maybe UserId }
    deriving (Ord, Read, Show, Eq, Typeable, Data)
$(deriveSafeCopy 0 'base ''PonySession)

instance Default PonySession where
    def = PonySession Nothing

newtype PonyServerPartT e m a = PonyServerPart { asRWS :: RWST () () PonySession (ServerPartT (ErrorT e m)) a }
    deriving (Monad, MonadIO, MonadReader (), MonadError e, MonadState PonySession, ServerMonad, MonadPlus, FilterMonad Response)

type PonyServerPart = PonyServerPartT Text IO

runPonyServer :: PonyServerPart Response -> IO ()
runPonyServer (PonyServerPart psp) = do
    key <- getDefaultKey
    simpleHTTP nullConf $ do
        _ <- compressedResponseFilter
        decodeBody $ defaultBodyPolicy "/tmp/" (10 * 1024 * 1024) 4096 4096
        mapServerPartT' (spUnwrapErrorT errorHandler) $ do
            sess <- fmap (fromMaybe def . decodeSession key) (lookCookieValue "ponysession") `mplus` return def
            (a, sess', _) <- runRWST psp () sess
            csess <- encodeSession key sess'
            addCookie sessionLife $ mkCookie "ponysession" csess
            return a
    where
        errorHandler = simpleErrorHandler . show
        sessionLife = MaxAge $ 60 * 60 * 24 * 7

encodeSession :: (MonadIO m) => Key -> PonySession -> m String
encodeSession k = liftIO . fmap unpack . encryptIO k . pack . show

decodeSession :: Key -> String -> Maybe PonySession
decodeSession k = fmap (read . unpack) . decrypt k . pack

authenticated :: PonyServerPart ()
authenticated = do
    (PonySession muid) <- get
    unless (isJust muid) mzero

lookUserId :: PonyServerPart UserId
lookUserId = do
    (PonySession muid) <- get
    case muid of
        Nothing -> mzero
        Just uid -> return uid

setUserId :: UserId -> PonyServerPart ()
setUserId = put . PonySession . Just

logoutUser :: PonyServerPart ()
logoutUser = put $ PonySession Nothing
