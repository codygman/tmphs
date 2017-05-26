{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell, QuasiQuotes, DataKinds #-}
module Gen where

import Database.Cassandra.CQL
import Language.Haskell.TH
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.UUID
import System.Random
import Control.Monad
import Control.Lens
import Data.Function
import Data.List
import Data.Maybe
import Data.String
import Keymap

getColFamInfo :: Query Rows () (Text, Text, Text, Text, Text)
getColFamInfo = "select keyspace_name, columnfamily_name, column_name, type, validator from system.schema_columns"

getColFamInfoIO servers auth keyspace = do
  -- TODO Maybe set auth via environment variables?
  let poolCfg = (defaultConfig servers keyspace auth){ piKeyspaceConfig = Nothing}
  pool <- newPool' poolCfg -- servers, keyspace, maybe auth
  runCas pool $ do
    info <- executeRows QUORUM getColFamInfo ()
    let info' = filter (\(ks,_,_,_,_) -> not $ "system" `T.isInfixOf` ks) info
    let groupedData = groupBy ((==) `on` view _2) info'
    let f (_,_,a,b,c) = (a,b,c)
    let groupedData' = (\tbl -> (tbl ^? _head . _2, f <$> tbl)) <$> groupedData -- remove all but table names and column data
    let groupedData'' = (\(x,y) -> (fromJust x,y)) <$> filter (isJust . fst) groupedData'-- remove any Nothings
    return groupedData''

genAllTblDataTypes :: [Server] -> Maybe Authentication -> Keyspace -> Q [Dec]
genAllTblDataTypes servers auth keyspace = do
  info <- runIO (getColFamInfoIO servers auth keyspace)
  decs <- mapM mkDataType info
  pure decs

mkDataType :: (Text, [(Text,Text,Text)]) -> DecQ
mkDataType info = do
  -- TODO title case dataTypeName
  let dataTypeName = T.toTitle $ fst info
      cxt = []
      nm = mkName $ T.unpack dataTypeName
      tyVarBndr = []
      knd = Nothing
      cxt2 = []
  conLst <- g nm info
  return $ DataD cxt nm tyVarBndr knd [conLst] cxt2

-- TODO rename
g :: Name -> (Text, [(Text,Text,Text)]) -> ConQ
g nm cols = do
  recC nm (k (snd cols))
  where
    -- TODO rename
    h :: (Text, Text, Text) -> VarBangTypeQ
    h (nm', typ, keyTyp) =
      do n <- newName $ T.unpack nm'
         varBangType n (bangType (bang noSourceUnpackedness noSourceStrictness) (conT $ keyMap keyTyp))
    k :: Functor f => f (Text, Text, Text) -> f VarBangTypeQ
    -- TODO rename
    k lst = fmap h lst

