{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Lib
    ( someFunc
    ) where

import Gen

  -- let auth = Just (PasswordAuthenticator "cassandra" "cassandra")
$(genAllTblDataTypes [("localhost", "9042")] "test1" (Just (PasswordAuthenticator "cassandra" "cassandra")))

someFunc :: IO ()
someFunc = undefined
-- someFunc = do let x = Tblnm 1
--               putStrLn "done"
