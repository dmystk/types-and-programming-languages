module Section3.Wrong.Evaluator
    ( eval
    , step
    ) where

import Section3.Wrong.Parser

isNumerical :: Expr -> Bool
isNumerical EZero = True
isNumerical (ESucc e) = isNumerical e
isNumerical _ = False

isBadNat :: Expr -> Bool
isBadNat ETrue = True
isBadNat EFalse = True
isBadNat EWrong = True
isBadNat _ = False

isBadBool :: Expr -> Bool
isBadBool e | isNumerical e = True
isBadBool EWrong = True
isBadBool _ = False

eval :: Expr -> Expr
eval e = maybe e eval (step e)

step :: Expr -> Maybe Expr
step (EIf ETrue e _) = Just e
step (EIf EFalse _ e) = Just e
step (EIf e _ _) | isBadBool e = Just EWrong
step (EIf e1 e2 e3) = (\e1' -> EIf e1' e2 e3) <$> step e1
step (ESucc e) | isBadNat e = Just EWrong
step (ESucc e) = ESucc <$> step e
step (EPred EZero) = Just EZero
step (EPred (ESucc e)) | isNumerical e = Just e
step (EPred e) | isBadNat e = Just EWrong
step (EPred e) = EPred <$> step e
step (EIsZero EZero) = Just ETrue
step (EIsZero (ESucc e)) | isNumerical e = Just EFalse
step (EIsZero e) | isBadNat e = Just EWrong
step (EIsZero e) = EIsZero <$> step e
step _ = Nothing
