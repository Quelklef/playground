{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Prelude hiding (Monoid, mempty, mappend, (<>), Semigroup, Monad, return, (>>=))
import qualified Prelude as P
import qualified Test.QuickCheck as Q
import qualified Test.QuickCheck.Arbitrary as Q
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS


instance (Q.Arbitrary a, Ord a) => Q.Arbitrary (MultiSet a) where
  arbitrary = MS.fromList <$> Q.arbitrary

takeR :: Int -> [a] -> [a]
takeR n = reverse . take n . reverse

dropR :: Int -> [a] -> [a]
dropR n = reverse . drop n . reverse


data Monoid a = Monoid a (a -> a -> a)

data Semigroup a = Semigroup (a -> a -> a)

data Monad m = Monad (forall a. a -> m a) (forall a b. m a -> (a -> m b) -> m b)

op :: Monoid a -> Monoid a
op (Monoid mempty mappend) = Monoid mempty (flip mappend)

-- lists under concatenation
concatenating :: Monoid [a]
concatenating = Monoid [] (++)

-- concatenates lists, but combines n items off the head/tail via the supplied semigroup operation
-- at n=infinity this is the same as 'zipping'
-- at n=0 this is the same as 'concatenating'
concatenatingOverlapping :: Int -> Semigroup a -> Monoid [a]
concatenatingOverlapping n (Semigroup (<>)) = Monoid mempty mappend
  where
  mempty = []
  mappend xs ys =
    let Monoid _ zipWithLonger = zipping (Semigroup (<>))
    in dropR n xs ++ zipWithLonger (takeR n xs) (take n ys) ++ drop n ys

-- zipping, taking the length of the longer list
zipping :: Semigroup a -> Monoid [a]
zipping (Semigroup (<>)) = Monoid mempty mappend
  where
  mempty = []
  mappend xs ys =
    let n = min (length xs) (length ys)
    in (++) (zipWith (<>) (take n xs) (take n ys))
            (drop n xs ++ drop n ys)

-- xs <> ys extends xs using elements from ys
extending :: Monoid [a]
extending = Monoid mempty mappend
  where
  mempty = []
  mappend xs ys = xs ++ drop (length xs) ys

-- alternative definition for extending
extending' :: Monoid [a]
extending' = zipping (Semigroup const)

-- xs <> ys is creates the mutliset-cartprod of xs and ys and then combines pairs
-- with the supplied monoid instance for a
msCartProd :: Ord a => Monoid a -> Monoid (MultiSet a)
msCartProd (Monoid e (<>)) = Monoid mempty mappend
  where
  mempty = MS.singleton e
  mappend xs ys = xs `MS.bind` (\x -> MS.map (x <>) ys)

{-

Proof of associativity is as follows:

(ma <> mb) <> mc
= (ma >>= const mb) >>= const mc
= ma >>= (\a -> const mb a >>= const mc)  [Associativity of Monad]
= ma >>= (\a -> mb >>= const mc)
= ma >>= const (mb >>= const mc)
= ma <> (mb <> mc)

nb. this is (<>) = (>>)

-}
sequencing :: Monad m -> (forall a. Semigroup (m a))
sequencing (Monad _ (>>=)) = Semigroup (<>)
  where
  ma <> mb = ma >>= const mb

-- this is the 'Monad a => Monad (IO a)' instance in base!
funnelling :: Monad m -> Monoid a -> Monoid (m a)
funnelling (Monad return (>>=)) (Monoid e (<>)) = Monoid mempty mappend
  where
  mempty = return e
  mappend = \ma mb -> ma >>= \a -> mb >>= \b -> return $ a <> b

main = do

  isMonoid "concatenating" $ concatenating @Int
  isMonoid "concatenatingOverlapping" $ concatenatingOverlapping @Int 5 (Semigroup (+))
  isMonoid "zipping" $ zipping @Int (Semigroup @Int (+))
  isMonoid "extending" $ extending @Int
  isMonoid "extending'" $ extending' @Int
  isMonoid "msCartProd" $ msCartProd @Int (Monoid 0 (+))
  isSemigroup "sequencing" $ sequencing @[] (Monad P.return (P.>>=)) @Int

  where

  check = Q.quickCheckWith (Q.stdArgs { Q.maxSuccess = 500 })

  isMonoid name mon = do
    isMonoid' name mon
    isMonoid' ("op(" ++ name ++ ")") (op mon)

    where

    isMonoid' name mon = do

      putStrLn $ "Associativity of " ++ name
      check $ isAssoc mon
      putStrLn $ "Identity of " ++ name
      check $ isId mon

    isId (Monoid e (<>)) = Q.property (\a -> a <> e == a && e <> a == a)
    isAssoc (Monoid _ (<>)) = Q.property (\a b c -> a <> (b <> c) == (a <> b) <> c)

  isSemigroup name sem = do
    putStrLn $ "Associativity of " ++ name
    check $ isAssoc sem

    where
    isAssoc (Semigroup (<>)) = Q.property (\a b c -> a <> (b <> c) == (a <> b) <> c)
