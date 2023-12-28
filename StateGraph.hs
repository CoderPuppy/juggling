{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module StateGraph where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer.Lazy
import Data.Bits
import Data.Digits
import Data.Foldable
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.IntSet as IS

ppState n = take n . (++ repeat '0') . reverse . show . unDigits 10 . digits 2

throws h state = let state' = shiftR state 1 in
	if not $ testBit state 0 then pure (0, state') else
	do
		throw <- [1..h]
		guard $ not $ testBit state' (throw - 1)
		pure (throw, setBit state' (throw - 1))

states 0 h = pure 0
states b h | b > 0, h > 0 = choose <|> leave
	where
		choose = fmap (flip setBit 0 . flip shiftL 1) $ states (b - 1) (h - 1)
		leave = fmap (flip shiftL 1) $ states b (h - 1)
states b h = empty

generateGraph b h = do
	tell $ pure "digraph {"
	for_ (filter (flip testBit 0) $ states b h :: [Int]) \state -> do
		tell $ pure $ "  \"" <> ppState h state <> "\";"
		for_ (throws h state :: [(Int, Int)]) \(throw, state') -> do
			let zeroes = countTrailingZeros state'
			let state'' = shiftR state' zeroes
			tell $ pure $
				"  \"" <> ppState h state <> "\" -> \"" <> ppState h state'' <> "\" " <>
				"[label=\"" <> show throw <> " " <> replicate zeroes '0' <> "\"];"
	tell $ pure "}"

generateAdjacency b h = foldl' go IM.empty $ (filter (flip testBit 0) $ states b h :: [Int])
	where go adj state = IM.insert state newStates adj
		where newStates = do
			(throw, state') <- throws h state :: [(Int, Int)]
			let zeroes = countTrailingZeros state'
			let state'' = shiftR state' zeroes
			pure state''

-- toMatrix adj = [[fromMaybe 0 $ M.lookup (x, y) sparse | x <- [0..n - 1]] | y <- [0..n - 1]]
-- 	where
-- 		n = IM.size adj
-- 		keyMap = IM.fromList $ flip zip [0..] $ IS.toList $ IM.keysSet adj
-- 		sparse = foldr (\(i, adj) -> flip (foldr (\state' -> M.insertWith (+) (i, keyMap IM.! state') 1)) adj) M.empty $ IM.toList adj

toMatrix adj = foldr (\(state, adj) -> flip (foldr (\state' -> M.insertWith (+) (state, state') 1)) adj) M.empty $ IM.toList adj

-- do
-- 	tell $ pure "digraph {"
-- 	for_ (filter (flip testBit 0) $ states b h :: [Int]) \state -> do
-- 		tell $ pure $ "  \"" <> ppState h state <> "\";"
-- 		for_ (throws h state :: [(Int, Int)]) \(throw, state') -> do
-- 			let zeroes = countTrailingZeros state'
-- 			let state'' = shiftR state' zeroes
-- 			tell $ pure $
-- 				"  \"" <> ppState h state <> "\" -> \"" <> ppState h state'' <> "\" " <>
-- 				"[label=\"" <> show throw <> " " <> replicate zeroes '0' <> "\"];"
-- 	tell $ pure "}"

main = traverse_ putStrLn $ (execWriter $ generateGraph 2 12 :: [String])
