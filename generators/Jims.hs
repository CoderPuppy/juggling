{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Jims where

import Control.Lens.Indexed (imap)
import Data.Bits (xor)
import Data.Foldable
import Data.IntMap.Merge.Strict qualified as IM
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS

generateFromTo :: Enum i => i -> i -> (i -> a) -> IM.IntMap a
generateFromTo from to f = IM.fromList do
	i <- [from..to]
	pure (fromEnum i, f i)
generateBounded :: (Enum i, Bounded i) => (i -> a) -> IM.IntMap a
generateBounded = generateFromTo minBound maxBound

-- Finitely Sparse Stream
data FSS a = Empty | Entry Int a (FSS a)
	deriving (Show, Eq, Ord)

data State j l = State {
	stLandings :: IM.IntMap {- j -} (FSS l)
}
type Beat j c = j -> (Int, j, c)
data Pattern j c = Pattern {
	patLen :: Int,
	patThrows :: Int -> Beat j c
}

class Crossing c l where
	crossTo :: c -> l -> l

simCatch :: forall j l. (Show j, Enum j, Show l, Enum l) => State j l -> State j l
simCatch State{..} = State
	{ stLandings = flip imap stLandings \(toEnum -> j :: j) landings ->
		case landings of
			Entry 0 _ rest -> rest
			_ -> error $ "Juggler " <> show j <> " has nothing landing"
	}

simThrow :: forall j c l. (Show j, Enum j, Crossing c l) => Beat j c -> State j l -> State j l
simThrow throws State{..} = State
	{ stLandings = foldl'
		(\landings (toEnum -> j, j_landings) ->
			case j_landings of
				Entry 0 l _ ->
					let (throw, j', crossing) = throws j in
					-- (fromEnum j', (throw, crossTo crossing l))
					IM.alter
						(\case
							Nothing -> error $ "Missing landing pattern for juggler " <> show j'
							Just j'_landings -> Just _
						)
						(fromEnum j') landings
				_ -> error $ "Juggler " <> show j <> " has nothing landing to trigger a throw"
		)
		stLandings
		(IM.toList stLandings)
	}

patLandings :: forall j c. (Enum j, Bounded j) => Pattern j c -> IM.IntMap {- i -} (IM.IntMap {- j -} Int)
patLandings Pattern{..} = IM.fromListWith (IM.unionWith (+)) do
	i <- [0..patLen - 1]
	j :: j <- [minBound..]
	let (throw, j', _) = patThrows i j
	pure (mod (i + throw) patLen, IM.singleton (fromEnum j') 1)

patExpectedLandings :: forall j c. (Enum j, Bounded j) => Pattern j c -> IM.IntMap {- i -} (IM.IntMap {- j -} Int)
patExpectedLandings Pattern{patLen} =
	generateFromTo 0 (patLen - 1) \i ->
	generateBounded \(j :: j) ->
	1

pat_2count :: Pattern Bool Bool
pat_2count = Pattern 2 $ \i j -> (2, (mod i 2 == 0) `xor` j, True)
pat_3count :: Pattern Bool Bool
pat_3count = Pattern 3 $ \i j -> (2, (mod i 3 == 0) `xor` j, True)
pat_jims_3count :: Pattern Bool Bool
pat_jims_3count = Pattern 3 $ \i j -> (2, (mod i 3 == 0) `xor` j, j)
