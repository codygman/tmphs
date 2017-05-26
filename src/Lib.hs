{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Lib
    ( someFunc
    ) where

import Gen


$(mkDataType (fst schema') schema')

someFunc :: IO ()
someFunc = undefined
-- someFunc = do let x = Tblnm 1
--               putStrLn "done"
