{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module Data.Fleece where

import Data.Bson (val, u, cast', Val, Field(..))
import qualified Data.Bson as M
import Data.Attoparsec.Number (Number(..))
import Data.Text (unpack, pack, Text)
import Data.Aeson
import Data.Data (Data, Typeable)
import qualified Data.Vector as V (toList, fromList)
import qualified Data.HashMap.Strict as H (toList, fromList)

instance Val Text where
    val = val . unpack
    cast' = fmap pack . cast'

instance Val Value where
	val Null = M.Null
	val (Bool b) = M.Bool b
	val (Number (I n)) = M.Int64 $ fromIntegral n
	val (Number (D n)) = M.Float n
	val (String t) = M.String . u . unpack $ t
	val (Array v) = M.Array $ map val $ V.toList v
	val (Object o) = M.Doc $ map (\(l, v) -> u (unpack l) := val v) $ H.toList o

	cast' M.Null = Just Null
	cast' (M.Bool b) = Just $ Bool b
	cast' (M.Int32 n) = Just $ toJSON n
	cast' (M.Int64 n) = Just $ toJSON n
	cast' (M.Float n) = Just $ toJSON n
	cast' (M.UTC t) = Just $ toJSON t
	cast' (M.String s) = Just $ toJSON $ M.unpack s
	cast' (M.Doc d) = fmap (Object . H.fromList) $ mapM (uncurry (fmap . (,)) . (\(l := v) -> (pack $ M.unpack l, cast' v))) d
	cast' (M.Array vs) = fmap (Array . V.fromList) $ mapM cast' vs
	cast' x = Just . toJSON . show $ x

instance (Show a, Eq a, Typeable a, FromJSON a, ToJSON a) => Val a where
    val = val . toJSON
    cast' x = do
        v <- cast' x
        case fromJSON v of
            Error _ -> Nothing
            Success a -> Just a

asDocument :: (Monad m) => Value -> m M.Document
asDocument (Object o) = return $ map (\(l, v) -> u (unpack l) := val v) $ H.toList o
asDocument _ = fail "Not an object"
