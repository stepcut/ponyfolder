{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, DeriveDataTypeable, EmptyDataDecls, TemplateHaskell, RecordWildCards #-}
module Server (
        PonyServerPartT, PonyServerPart, runPonyServer,
        lookUserId, authenticated, logoutUser, lookUser, updateUser,
        UserId, throwError, catchError, mplus, msum, mzero
    ) where

import Control.Monad
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.RWS
import Data.Text (Text)
import Data.Aeson (encode, decode, FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Data (Data, Typeable)
import Data.Default (Default(..))
import Data.Time (getCurrentTime)
import Happstack.Server
import Happstack.Server.Compression
import Web.ClientSession (getDefaultKey, encryptIO, decrypt, Key)
import Data.Maybe (isJust, fromMaybe)
import Control.Exception (bracket)
import Data.Acid (openLocalState, AcidState, query, update)
import Data.Acid.Local (createCheckpointAndClose)

import Model.User

instance Error Text where
    noMsg = "No Message"
    strMsg = T.pack

newtype PonySession = PonySession (Maybe UserId)
    deriving (Ord, Read, Show, Eq, Typeable, Data, FromJSON, ToJSON)

instance Default PonySession where
    def = PonySession Nothing

data PonyContents = PonyContents {
        ponyUsers :: AcidState UserSet
    }

newtype PonyServerPartT e m a = PonyServerPart (RWST PonyContents () PonySession (ServerPartT (ErrorT e m)) a)
    deriving (Monad, MonadIO, MonadReader PonyContents, MonadError e, MonadState PonySession, ServerMonad, MonadPlus, FilterMonad Response)

type PonyServerPart = PonyServerPartT Text IO

runPonyServer :: PonyServerPart Response -> IO ()
runPonyServer (PonyServerPart psp) = bracket (openLocalState initialUserSet) (createCheckpointAndClose) $ \userSet -> do
    key <- getDefaultKey
    simpleHTTP nullConf $ do
        _ <- compressedResponseFilter
        decodeBody $ defaultBodyPolicy "/tmp/" (10 * 1024 * 1024) 4096 4096
        mapServerPartT' (spUnwrapErrorT errorHandler) $ do
            sess <- fmap (fromMaybe def . decodeSession key) (lookCookieValue "ponysession") `mplus` return def
            (a, sess', _) <- runRWST psp (PonyContents userSet) sess
            csess <- encodeSession key sess'
            addCookie sessionLife $ mkCookie "ponysession" csess
            return a
    where
        errorHandler = simpleErrorHandler . show
        sessionLife = MaxAge $ 60 * 60 * 24 * 7

encodeSession :: (MonadIO m) => Key -> PonySession -> m String
encodeSession k = liftIO . fmap B.unpack . encryptIO k . B.concat . BL.toChunks . encode

decodeSession :: Key -> String -> Maybe PonySession
decodeSession k s = do
    v <- decrypt k $ B.pack s
    decode . BL.fromChunks $ [v]

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

logoutUser :: PonyServerPart ()
logoutUser = put $ PonySession Nothing

lookUser :: PonyServerPart User
lookUser = do
    uid <- lookUserId
    PonyContents{..} <- ask
    now <- liftIO getCurrentTime
    liftIO $ query ponyUsers $ UserByEmail uid now

updateUser :: User -> PonyServerPart ()
updateUser user = do
    PonyContents{..} <- ask
    liftIO $ update ponyUsers $ SaveUser user
