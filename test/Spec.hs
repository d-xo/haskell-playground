{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

import Test.QuickCheck

identityLaw :: (Applicative a, Eq (a b)) => a b -> Bool
identityLaw x = (pure id <*> x) == x

homomorphismLaw :: forall a b c. (Eq (c b), Applicative c) => (a -> b) -> a -> Bool
homomorphismLaw f x = (pure f <*> pure x) == (pure (f x) :: c b)

interchangeLaw :: forall a b c. (Applicative a, Eq (a c)) => a (b -> c) -> b -> Bool
interchangeLaw u x = (u <*> pure x) == (pure ($ x) <*> u)

compositionLaw :: forall a b c d. (Applicative a, Eq (a b)) => a (c -> b) -> a (d -> c) -> a d -> Bool
compositionLaw u v w = (u <*> (v <*> w)) == (pure (.) <*> u <*> v <*> w)

main = do
  quickCheck (identityLaw :: Maybe Integer -> Bool)
  quickCheck (homomorphismLaw @Integer @Integer @Maybe (* 1000))
  quickCheck (interchangeLaw @Maybe @Integer @Integer (Just (+ 55959)))
  quickCheck (compositionLaw @Maybe @Integer @Integer @Integer (Just ((-) 9900)) (Just (* 1000)))
