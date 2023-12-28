{-# OPTIONS -Wno-tabs #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Lang2 where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.IntMap as IM

data SynDuration = SynFinDur Int | SynInfDur
	deriving (Show)

data SynSchedRepeat a = SynSchedRepeat {
	synSchedRepeatStart :: Int,
	synSchedRepeatPositive :: Bool,
	synSchedRepeatPeriod :: Int,
	synSchedRepeatData :: IM.IntMap a
} deriving (Show)
data SynSchedule a = SynSchedule {
	synSchedOnce :: IM.IntMap a,
	synSchedRepeat :: Maybe (SynSchedRepeat a)
} deriving (Show)

data SynType
	= SynFun SynType SynDuration SynType
	| SynProduct (M.Map T.Text SynType)
	| SynProp (SynSchedule ())
	deriving (Show)

data SynTerm
	= SynVar Int
	| SynAscribe SynTerm SynType
	| SynBlock [(Maybe T.Text, SynTerm)]
	| SynLam T.Text (Maybe SynType) SynTerm
	| SynApp SynTerm SynTerm
	| SynTuple (M.Map T.Text SynTerm)
	| SynProj SynTerm T.Text
	| SynWait Int
	| SynManipulate SynTerm
	deriving (Show)
