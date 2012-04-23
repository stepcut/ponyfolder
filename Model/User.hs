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

data User = User {
        userEmail :: !UserId,
        userName :: Text,
        userPublic :: !Bool,
        userNsfw :: !Bool,
        userMeta :: Map Text Text
    } deriving (Show, Read, Eq, Ord, Data, Typeable)
$(deriveSafeCopy 0 'base ''User)
$(deriveJSON (drop 4) ''User)

instance Indexable User where
    empty = ixSet [ ixFun $ \u -> [ userEmail u ] ]

newtype UserSet = UserSet { unUserSet :: IxSet User }
    deriving (Show, Read, Eq, Ord, Data, Typeable, SafeCopy)

userByEmail :: UserId -> Update UserSet User
userByEmail email = do
    (UserSet userSet) <- get
    case getOne $ userSet @= email of
        Just u -> return u
        Nothing -> do
            let user = mkUser email
                userSet' = I.insert user userSet
            put $ UserSet userSet
            return user

mkUser email = User email "" False False M.empty

$(makeAcidic ''UserSet ['userByEmail])
