{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}

module SymmetricSync where

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Foldable
import Data.Tuple
import Debug.Trace
import Data.Set qualified as S

-- generating symmetric synchronous patterns
--
-- (4x,2x)(4,2x)* ≡ (4x,2x)(4,2x)(2x,4x)(2x,4)
-- (6x,4x)* ≡ (6x,4x)(4x,6x) BAD

effectiveHeight place height crossing = if
	| not crossing -> 2 * height
	| testBit place 0 -> 2 * height - 1
	| otherwise -> 2 * height + 1
landing n place height crossing = mod (place + effectiveHeight place height crossing) (4 * n)
fill1 n place open = do
	height <- [0..2 * n - 1]
	crossing <- [False, True]
	let landing1 = landing n place height crossing
	let landing2 = landing n (complementBit (place + 2 * n) 0) height crossing
	guard $ testBit open landing1
	guard $ testBit open landing2
	pure ((height, crossing), clearBit (clearBit open landing1) landing2)
generate n = fmap fst $ foldrM
	(\place (throws, open) -> do
		(throw, open') <- fill1 n place open
		pure (throw:throws, open'))
	([], bit (4 * n) - 1 :: Integer) [0..2 * n - 1]
addHeight minHeight maxHeight p h | h > maxHeight = []
addHeight minHeight maxHeight p h | h < minHeight = addHeight minHeight maxHeight p (h + p)
addHeight minHeight maxHeight p h = h:addHeight minHeight maxHeight p (h + p)
pp [] = "*"
pp ((l, lx):(r, rx):ss) = mconcat
	[ "(", show (2 * l), if lx then "x" else ""
	, ",", show (2 * r), if rx then "x" else ""
	, ")", pp ss ]

rotateOne ((l, lx):(r, rx):ss) = ss <> [(r, rx), (l, lx)]
swapHands [] = []
swapHands ((l, lx):(r, rx):ss) = (r, rx):(l, lx):swapHands ss

removeDuplicates patterns = foldl' go (S.fromList patterns) patterns
	where
		go pats ss = S.insert ss $ S.difference pats rotations
			where rotations = S.fromList $ scanl (const . rotateOne) ss ss

test = join $ traverse (traverse (fmap swap . traverse (addHeight minHeight maxHeight period) . swap)) $ generate period
	where
		period = 3 -- (_,_)(_,_)
		minHeight = 1 -- 2
		maxHeight = 5 -- a
-- traverse_ putStrLn $ fmap pp $ filter (\ss -> all (\(h, _) -> h > 0 && h <= 4) ss && sum (fmap fst ss) == 2 * 5) $ fmap (fmap $ first \h -> if h == 0 then 4 else h) $ generate 2

test2 =
	removeDuplicates $
	filter (all \(h, _) -> h >= minHeight && h <= maxHeight) $
	filter ((== period * numBalls) . sum . fmap fst) $
	filter (not . any \(h, x) -> h == 1 && not x) $
	fmap (fmap $ first \h -> if h == 0 then maxHeight else h) $
	generate period
	where
		period = 3 -- (_,_)(_,_)
		minHeight = 1 -- 2
		maxHeight = 5 -- a
		numBalls = 4
