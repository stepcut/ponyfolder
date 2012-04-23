{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, EmptyDataDecls, TemplateHaskell #-}
module Server (
        PonyServerPartT, PonyServerPart, runPonyServer,
        lookUserId, authenticated, logoutUser, setUserId,
        runDB,
        throwError, catchError
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.RWS
import Control.Monad.State
import Data.Text (Text)
import Data.ByteString.Char8 (pack, unpack)
import Data.Data (Data, Typeable)
import Data.Default
import Data.SafeCopy (base, deriveSafeCopy)
import Database.MongoDB as M hiding (unpack)
import Happstack.Server
import Happstack.Server.Error
import Happstack.Server.Compression
import System.IO.Pool
import System.IO.Error
import Web.ClientSession (getDefaultKey, encryptIO, decrypt, Key)
import Data.Maybe (isJust, fromMaybe)
import Control.Exception (bracket)

type MongoPool e = Pool e Pipe

type UserId = Text

newtype PonySession = PonySession { sessUid :: Maybe UserId }
    deriving (Ord, Read, Show, Eq, Typeable, Data)
$(deriveSafeCopy 0 'base ''PonySession)

instance Default PonySession where
    def = PonySession Nothing

newtype PonyServerPartT e m a = PonyServerPart { asRWS :: RWST (MongoPool e) () PonySession (ServerPartT (ErrorT e m)) a }
    deriving (Monad, MonadIO, MonadReader (MongoPool e), MonadError e, MonadState PonySession, ServerMonad, MonadPlus, FilterMonad Response)

type PonyServerPart = PonyServerPartT IOError IO

runPonyServer :: PonyServerPart Response -> IO ()
runPonyServer (PonyServerPart psp) = bracket mongoPool killAll $ \pool -> do
    key <- getDefaultKey
    simpleHTTP nullConf $ do
        _ <- compressedResponseFilter
        decodeBody $ defaultBodyPolicy "/tmp/" (10 * 1024 * 1024) 4096 4096
        mapServerPartT' (spUnwrapErrorT errorHandler) $ do
            sess <- fmap (fromMaybe def . decodeSession key) (lookCookieValue "ponysession") `mplus` return def
            (a, sess', _) <- runRWST psp pool sess
            csess <- encodeSession key sess'
            addCookie sessionLife $ mkCookie "ponysession" csess
            return a
    where
        errorHandler = simpleErrorHandler . show
        sessionLife = MaxAge $ 60 * 60 * 24 * 7

mongoPool :: IO (MongoPool IOError)
mongoPool = newPool fac 10
    where fac = Factory {
            newResource = connect $ M.host "127.0.0.1",
            killResource = close,
            isExpired = isClosed
        }

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

runDB :: Action IO a -> PonyServerPart a
runDB a = do
    pipe <- (liftIO . runIOE . aResource) =<< ask
    f <- liftIO $ access pipe master "ponyfolder" a
    case f of
        Left failure -> do
            throwError . userError . show $ failure
            mzero
        Right x -> return x
