{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
data AE where
    Nat :: Int -> AE
    Plus :: AE -> AE -> AE
    Minus :: AE -> AE -> AE

eval::AE-> Maybe Int
eval (Nat x) = Just x
eval (Plus l r) = case (eval l) of
                    (Nothing) -> Nothing
                    (Just l') -> case (eval r) of
                                    Nothing -> Nothing
                                    (Just r') -> Just (l'+ r')
eval (Minus l r) = case (eval l) of
                    Nothing -> Nothing
                    (Just l') -> case (eval r) of
                                Nothing -> Nothing
                                (Just r') -> if l'>=r' then Just (l'-r') else Nothing

