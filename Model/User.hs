{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, RecordWildCards, TemplateHaskell, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Model.User where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Data (Data, Typeable)
import Data.IxSet (Indexable(..), IxSet(..), (@=), (@<=), Proxy(..), getOne, ixFun, ixSet, updateIx)
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

data User = User {
        userEmail :: !UserId,
        userName :: !Text,
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
                    ixFun $ (:[]) . userPermissions,
                    ixFun $ (:[]) . userCreated
                  ]

newtype UserSet = UserSet { unUserSet :: IxSet User }
    deriving (Show, Read, Eq, Ord, Data, Typeable, SafeCopy)

initialUserSet :: UserSet
initialUserSet = UserSet I.empty

userByEmail :: UserId -> UTCTime -> Query UserSet User
userByEmail email t = do
    (UserSet userSet) <- ask
    return $ I.getOneOr (mkUser email t) $ userSet @= email @<= t

allUsers :: Query UserSet [User]
allUsers = do
    (UserSet userSet) <- ask
    return $ I.toAscList (Proxy :: Proxy Text) userSet

adminUsers :: Query UserSet [User]
adminUsers = do
    (UserSet userSet) <- ask
    return $ I.toAscList (Proxy :: Proxy Text) $ userSet @= PermissionAdmin

saveUser :: User -> Update UserSet ()
saveUser user@User{..} = do
    (UserSet userSet) <- get
    put $ UserSet $ updateIx userEmail user userSet

mkUser :: UserId -> UTCTime -> User
mkUser email t = User email "" False PermissionNone t M.empty

$(makeAcidic ''UserSet ['userByEmail, 'allUsers, 'adminUsers, 'saveUser])
