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


ignoreDropFailure :: Cas () -> Cas ()
ignoreDropFailure code = code `catch` \exc -> case exc of
    ConfigError _ _ -> return ()  -- Ignore the error if the table doesn't exist
    Invalid _ _ -> return ()
    _               -> throwM exc

dropSongs :: Query Schema () ()
dropSongs = "drop table songs"

createSongs :: Query Schema () ()
createSongs = "create table songs (id uuid PRIMARY KEY, title ascii, artist varchar, femaleSinger boolean, timesPlayed int, comment text)"

insertSong :: Query Write (UUID, ByteString, Text, Bool, Int, Maybe Text) ()
insertSong = "insert into songs (id, title, artist, femaleSinger, timesPlayed, comment) values (?, ?, ?, ?, ?, ?)"

getSongs :: Query Rows () (UUID, ByteString, Text, Bool, Int, Maybe Text)
getSongs = "select id, title, artist, femaleSinger, timesPlayed, comment from songs"

getOneSong :: Query Rows UUID (Text, Int)
getOneSong = "select artist, timesPlayed from songs where id=?"

getColFamInfo :: Query Rows () (Text, Text, Text, Text, Text)
getColFamInfo = "select keyspace_name, columnfamily_name, column_name, type, validator from system.schema_columns"


getColFamInfoIO = do
  let auth = Just (PasswordAuthenticator "cassandra" "cassandra")
  let poolCfg = (defaultConfig [("localhost", "9042")] "test1" auth){ piKeyspaceConfig = Nothing}
  pool <- newPool' poolCfg -- servers, keyspace, maybe auth
  runCas pool $ do
    info <- executeRows QUORUM getColFamInfo ()
    let info' = filter (\(ks,_,_,_,_) -> not $ "system" `T.isInfixOf` ks) info
    let groupedData = groupBy ((==) `on` view _2) info'
    let f (_,_,a,b,c) = (a,b,c)
    let groupedData' = (\tbl -> (tbl ^? _head . _2, f <$> tbl)) <$> groupedData -- remove all but table names and column data
    let groupedData'' = (\(x,y) -> (fromJust x,y)) <$> filter (isJust . fst) groupedData'-- remove any Nothings
    return groupedData''

cassTest =  do
    -- let auth = Just (PasswordAuthenticator "cassandra" "cassandra")
    let auth = Nothing

    -- this config will automatically run keyspace creation cql script during each connection initializationj
    -- suitable for a development purposes

    let ksCfg  = "CREATE KEYSPACE IF NOT EXISTS test1 WITH replication = { 'class' : 'SimpleStrategy', 'replication_factor' : '1' };"
    -- let poolCfg = (defaultConfig [("localhost", "9042")] "test1" auth){ piKeyspaceConfig = Just ksCfg}
    let poolCfg = (defaultConfig [("localhost", "9042")] "test1" auth){ piKeyspaceConfig = Nothing}

    pool <- newPool' poolCfg -- servers, keyspace, maybe auth
    runCas pool $ do
        
        -- ignoreDropFailure $ liftIO . print =<< executeSchema QUORUM dropSongs ()
        -- liftIO . print =<< executeSchema QUORUM createSongs ()

        -- u1 <- liftIO randomIO
        -- u2 <- liftIO randomIO
        -- u3 <- liftIO randomIO
        -- executeWrite QUORUM insertSong (u1, "La Grange", "ZZ Top", False, 2, Nothing)
        -- executeWrite QUORUM insertSong (u2, "Your Star", "Evanescence", True, 799, Nothing)
        -- executeWrite QUORUM insertSong (u3, "Angel of Death", "Slayer", False, 50, Just "Singer Tom Araya")

        -- songs <- executeRows QUORUM getSongs ()
        -- liftIO $ forM_ songs $ \(uuid, title, artist, female, played, mComment) -> do
        --     putStrLn ""
        --     putStrLn $ "id            : "++show uuid
        --     putStrLn $ "title         : "++C.unpack title
        --     putStrLn $ "artist        : "++T.unpack artist
        --     putStrLn $ "female singer : "++show female
        --     putStrLn $ "times played  : "++show played
        --     putStrLn $ "comment       : "++show mComment

        -- liftIO $ putStrLn ""
        -- liftIO . print =<< executeRow QUORUM getOneSong u2
      info <- executeRows QUORUM getColFamInfo ()
      liftIO $ forM_ info $ \song -> print song


schema' = ("Tblnm", [ ("colnm","typ", "string")
                    , ("colnm2","typ", "int")
                    ])

mkDataType :: String -> (String, [(String,String,String)]) -> DecsQ
mkDataType dataTypeName cols = do
  -- TODO title case dataTypeName
  let cxt = []
      nm = mkName dataTypeName
      tyVarBndr = []
      knd = Nothing
      cxt2 = []
  conLst <- g nm cols
  return $ [DataD cxt nm tyVarBndr knd [conLst] cxt2]

-- TODO rename
g :: Name -> (String, [(String,String,String)]) -> ConQ
g nm cols = do
  recC nm (k (snd cols))
  where
    -- TODO rename
    h :: (String, String, String) -> VarBangTypeQ
    h (nm', typ, keyTyp) =
      do n <- newName nm'
         varBangType n (bangType (bang noSourceUnpackedness noSourceStrictness) (conT $ keyMap keyTyp))
    k :: Functor f => f (String, String, String) -> f VarBangTypeQ
    -- TODO rename
    k lst = fmap h lst

keyMap "int" = ''Int
keyMap "string" = ''String
keyMap x = error $ show x ++ " is not yet supported"
