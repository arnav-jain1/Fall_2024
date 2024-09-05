{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
data AE where
    Nat :: Int -> AE
    Plus :: AE -> AE -> AE
    Minus :: AE -> AE -> AE
    deriving (show)

eval::AE-> Maybe Int
eval (Nat x) = Just x
eval (Plus l r) = case (eval l)
                    (Nothing) -> Nothing
                    (Just l') -> case (eval r)
                                    (Just r') -> (Just l'+r')
                                    Nothing -> Nothing
eval (Minus l r) = case (eval l)
                    Nothing -> Nothing
                    (Just l') -> case (eval r)
                                Nothing -> Nothing
                                (Just r') -> if l'>=r' then (Just l'-r') else Nothing

