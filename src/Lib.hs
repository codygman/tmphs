{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Lib
    ( someFunc
    ) where

import Gen


$(f (fst schema') schema')

someFunc :: IO ()
someFunc = undefined
-- someFunc = do let x = Tblnm 1
--               putStrLn "done"
