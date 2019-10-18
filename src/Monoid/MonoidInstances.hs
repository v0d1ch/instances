module Monoid.MonoidInstances where

import Test.QuickCheck

-- 1. Validate all of your instances with QuickCheck. Since Semigroup’s
-- only law is associativity, that’s the only property you need to
-- reuse. Keep in mind that you’ll potentially need to import the
-- modules for Monoid and Semigroup and to avoid naming conflicts
-- for the (<>) depending on your version of GHC.

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

instance (Semigroup a, Eq a) => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
