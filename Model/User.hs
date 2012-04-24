{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Model.User where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Data (Data, Typeable)
import Data.IxSet (Indexable(..), IxSet(..), (@=), Proxy(..), getOne, ixFun, ixSet, updateIx)
import qualified Data.IxSet as I
import Data.Map as M (Map, empty)
import Data.SafeCopy (base, deriveSafeCopy, SafeCopy(..))
import Data.Text (Text, unpack, pack)
import Data.Time (UTCTime, getCurrentTime)
import Control.Monad (mzero)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.Acid
import Data.String

newtype UserId = UserId { unUserId :: Text }
    deriving (Eq, Ord, Data, Read, Show, Typeable, SafeCopy)

instance ToJSON UserId where
    toJSON = String . unUserId

instance FromJSON UserId where
    parseJSON (String t) = return $ UserId t
    parseJSON _ = mzero

instance IsString UserId where
    fromString = UserId . pack

data AccessPermission = PermissionNone | PermissionAdmin
    deriving (Eq, Ord, Data, Read, Show, Typeable)
$(deriveSafeCopy 0 'base ''AccessPermission)
$(deriveJSON id ''AccessPermission)

newtype UserPublic = UserPublic { unUserPublic :: Bool } deriving (Show, Read, Eq, Ord, Data, Typeable, SafeCopy)

data User = User {
        userEmail :: !UserId,
        userName :: !Text,
        userPublic :: !Bool,
        userNsfw :: !Bool,
        userPermissions :: !AccessPermission,
        userCreated :: !UTCTime,
        userMeta :: Map Text Text
    } deriving (Show, Read, Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''User)
$(deriveJSON (drop 4) ''User)

instance Indexable User where
    empty = ixSet [ ixFun $ (:[]) . userEmail,
                    ixFun $ (:[]) . userName,
                    ixFun $ (:[]) . UserPublic . userPublic,
                    ixFun $ (:[]) . userPermissions,
                    ixFun $ (:[]) . userCreated
                  ]

newtype UserSet = UserSet { unUserSet :: IxSet User }
    deriving (Show, Read, Eq, Ord, Data, Typeable, SafeCopy)

initialUserSet = UserSet I.empty

userByEmail :: UserId -> UTCTime -> Update UserSet User
userByEmail email t = do
    (UserSet userSet) <- get
    case getOne $ userSet @= email of
        Just u -> return u
        Nothing -> do
            let user = mkUser email t
                userSet' = I.insert user userSet
            put $ UserSet userSet
            return user

publicUsers :: Query UserSet [User]
publicUsers = do
    (UserSet userSet) <- ask
    return $ I.toAscList (Proxy :: Proxy Text) $ userSet @= (UserPublic True)

adminUsers :: Query UserSet [User]
adminUsers = do
    (UserSet userSet) <- ask
    return $ I.toAscList (Proxy :: Proxy Text) $ userSet @= PermissionAdmin

mkUser email t = User email "" False False PermissionNone t M.empty

$(makeAcidic ''UserSet ['userByEmail, 'publicUsers, 'adminUsers])
