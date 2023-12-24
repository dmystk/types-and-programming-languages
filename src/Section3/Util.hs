{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Section3.Util
    ( pprint
    ) where

import Section3.Parser

pprint :: Expr -> IO ()
pprint expr = putStrLn $ read expr
    where
        read :: Expr -> String
        read ETrue = "true"
        read EFalse = "false"
        read EZero = "0"
        read (ESucc e) = "succ " ++ readSub e
        read (EPred e) = "pred " ++ readSub e
        read (EIsZero e) = "iszero " ++ readSub e
        read (EIf e1 e2 e3) =
            "if " ++ readSub e1 ++ " then " ++ readSub e2 ++ " else " ++ readSub e3

        readSub :: Expr -> String
        readSub ETrue = "true"
        readSub EFalse = "false"
        readSub EZero = "0"
        readSub e@(ESucc _) = paren $ read e
        readSub e@(EPred _) = paren $ read e
        readSub e@(EIsZero _) = paren $ read e
        readSub e@(EIf {}) = paren $ read e

        paren :: String -> String
        paren str = '(' : str ++ [')']
