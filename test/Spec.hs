{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Test.QuickCheck

identityLaw :: (Applicative a, Eq (a b)) => a b -> Bool
identityLaw x = (pure id <*> x) == x

homomorphismLaw :: (Applicative c) => (a -> b) -> a -> Bool
homomorphismLaw  f x = (pure f <*> pure x) == (pure (f x) :: c b)

main = do
  quickCheck (identityLaw :: Maybe Integer -> Bool)
  --quickCheck (homomorphismLaw :: (Int -> Int) -> Int -> Bool)
