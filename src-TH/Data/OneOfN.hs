{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Data.OneOfN where

import Data.Char (toLower)
import Data.Data (Data, Typeable)
import Language.Haskell.TH

$( do
    let minN = 2
        maxN = 20
        maxTypeableN = 7
        lc []     = []
        lc (c:cs) = toLower c : cs
        
        -- same naming scheme as HaXml, but start using numerals at 10,
        -- as per (one) standard English writing convention.
        nOfM n m = concat [numWord n, "Of", show m]
            where
                numWords = ["Zero","One","Two","Three","Four","Five",
                            "Six","Seven","Eight","Nine"]
                numWord n
                    | n < length numWords   = numWords !! n
                    | otherwise             = "Choice" ++ show n
        typeVarName n = mkName ("t" ++ show n)
        
        mkOneOfN m = do
            -- the data declaration
            ty <- dataD (cxt []) (mkName (nOfM 1 m))
                (map (PlainTV . typeVarName) [1..m]) 
                [ normalC (mkName (nOfM n m)) [strictType isStrict (varT (typeVarName n))] 
                    | n <- [1..m]]
                ([''Eq, ''Ord, ''Read, ''Show]
                    ++ if m <= maxTypeableN then [''Data, ''Typeable] else [])
            
            -- a "folding" function, like 'Prelude.either'
            let f n = mkName ("f" ++ show n)
                it  = mkName "it"
                x   = mkName "x"
            fun <- funD (mkName (lc (nOfM 1 m)))
                [ clause
                    (map (varP . f) [1..m] ++ [varP it])
                    (normalB (caseE (varE it)
                        [ match (conP (mkName (nOfM n m)) [varP x])
                            (normalB (appE (varE (f n)) (varE x))) []
                        | n <- [1..m]
                        ])) []
                ]
            
            return [ty, fun]
    
    fmap concat (mapM mkOneOfN [minN..maxN])
 )