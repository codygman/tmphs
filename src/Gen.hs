{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Gen where


import Language.Haskell.TH

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
