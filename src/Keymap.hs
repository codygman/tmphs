{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Keymap where

import Data.Text (Text)
import Data.UUID (UUID)
import Language.Haskell.TH
import Data.String

-- keyMap "org.apache.cassandra.db.marshal.UTF8Type" = ''Text
-- keyMap "org.apache.cassandra.db.marshal.BooleanType" = ''Bool
-- keyMap "org.apache.cassandra.db.marshal.UUIDType" = ''UUID
-- keyMap "org.apache.cassandra.db.marshal.Int32Type" == ''Int
-- keyMap "org.apache.cassandra.db.marshal.AsciiType" == ''Text

-- keyMapLookup "org.apache.cassandra.db.marshal.AsciiType" == ''String
-- keyMapLookup x = error $ show x ++ " is not yet supported"


keyMap :: Text -> Name
keyMap x
  | any (== x) ["org.apache.cassandra.db.marshal.Int32Type"] = ''Int
  | any (== x) ["org.apache.cassandra.db.marshal.UTF8Type", "org.apache.cassandra.db.marshal.AsciiType"] = ''Text
  | any (== x) ["org.apache.cassandra.db.marshal.BooleanType"] = ''Bool
  | any (== x) ["org.apache.cassandra.db.marshal.UUIDType"] = ''UUID
  | otherwise = error $ show x ++ " is not yet supported"
