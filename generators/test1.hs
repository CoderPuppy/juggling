{-# OPTIONS_GHC -Wno-tabs #-}

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Data.Traversable
import Data.Tuple
import Test.QuickCheck
import qualified Data.Set as S

newtype Perm = Perm { unPerm :: [Int] } deriving (Show)
newtype Cycle = Cycle { unCycle :: [Int] } deriving (Show, Eq, Ord)
newtype Siteswap = Siteswap { unSiteswap :: [Int] } deriving (Show)

perm :: Int -> [Perm]
perm 0 = pure $ Perm []
perm n = do
	x <- [0..(n - 1)]
	Perm ys <- perm (n - 1)
	let f y = if y < x then y else mod (y + 1) n
	pure $ Perm $ x:fmap f ys

siteswap :: Perm -> Siteswap
siteswap (Perm p) = Siteswap $ fmap (flip mod (length p)) $ zipWith (-) p [0..]

expandSiteswap :: Int -> Siteswap -> [Siteswap]
expandSiteswap max (Siteswap s) = fmap Siteswap $ for s $ \throw ->
	fmap
		((+ throw) . (* length s))
		[0..(max + if throw == 0 then 1 else 0)]

numProps :: Siteswap -> Int
numProps (Siteswap s) = div (sum s) (length s)

period :: Siteswap -> Int
period = length . unSiteswap

siteswapPerm :: Siteswap -> Perm
siteswapPerm (Siteswap s) = Perm $ fmap (flip mod (length s)) $ zipWith (+) s [0..]

minCycle :: Cycle -> Cycle
minCycle (Cycle c) = Cycle $ uncurry (<>) $ swap $ span (/= minimum c) c

disjointCycles :: Perm -> S.Set Cycle
disjointCycles (Perm p) = S.fromList $ fmap (minCycle . Cycle . go S.empty) [0..length p - 1]
	where
		go v i | S.member i v = []
		go v i = i:go (S.insert i v) (p !! i)

follow :: Siteswap -> Int -> (S.Set Int, Int)
follow (Siteswap s) i = go i S.empty
	where
		go i v | i >= length s = (v, i - length s)
		go i v | S.member i v = (v, i)
		go i v = go (s !! i + i) (S.insert i v)

check :: Siteswap -> Bool
check s = (== S.fromList [0..period s - 1]) $ S.unions $ fmap (fst . snd) $ filter (uncurry (==) . second snd) $ fmap (id &&& follow s) [0..period s - 1]

minRep :: Siteswap -> Int
minRep (Siteswap s) = fromJust $ find (check . Siteswap . join . flip replicate s) [1..]

instance Arbitrary Perm where
	arbitrary = sized $ \n -> do
		m <- chooseInt (1, n)
		p <- shuffle [0..(m - 1)]
		pure $ Perm p
instance Arbitrary Siteswap where
	arbitrary = do
		s <- fmap (unSiteswap . siteswap) arbitrary
		fmap Siteswap $ for s $ \throw ->
			fmap
				((+ throw) . (* length s))
				(chooseInt (0,3))

order :: Perm -> Int
order = foldl' lcm 1 . fmap (length . unCycle) . S.toList . disjointCycles

w1 = do
	n <- [1..]
	p <- perm n
	let s = siteswap p
	s <- expandSiteswap 3 s
	pure s
