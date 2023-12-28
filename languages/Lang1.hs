{-# OPTIONS_GHC -Wno-tabs #-}
{-# LANGUAGE EmptyDataDeriving #-}

module Lang1 where

data Unifiable a -- TODO
	deriving (Show)

data Schedule a -- TODO
	deriving (Show)

data Duration
	= DFinite Int -- positive
	| DInfinite
	deriving (Show)

data Type
	= TProp (Schedule Bool)
	| TFun Type Duration Type
	| TTuple [Type]
	deriving (Show)

data Term
	= Var Int
	| Lam Type Term
	| Apply Term Term
	| Tuple [Term]
	| Proj Term Int
	deriving (Show)

durAdd :: Duration -> Duration -> Duration
durAdd DInfinite _ = DInfinite
durAdd _ DInfinite = DInfinite
durAdd (DFinite a) (DFinite b) = DFinite (a + b)

termDuration :: [Type] -> Term -> Duration
termDuration ctx (Apply f a) = let TFun at d rt = termType ctx f in durAdd d (termDuration ctx a)
termDuration ctx (Tuple components) = foldr durAdd (DFinite 0) (fmap (termDuration ctx) components)
termDuration ctx (Proj t _) = termDuration ctx t
termDuration ctx _ = DFinite 0

termType :: [Type] -> Term -> Type
termType ctx (Var i) = ctx !! i
termType ctx (Lam at body) = TFun at (termDuration (at:ctx) body) (termType (at:ctx) body)
termType ctx (Apply f a) = let TFun at d rt = termType ctx f in rt
termType ctx (Tuple components) = TTuple (fmap (termType ctx) components)
termType ctx (Proj t i) = let TTuple ts = termType ctx t in ts !! i

sanityCheck :: [Type] -> Term -> Maybe String
sanityCheck ctx (Var i) = if i >= 0 && i < length ctx then Nothing else
	Just $ "variable reference " <> show i <> " out of range for context " <> show ctx
sanityCheck ctx (Lam at body) = sanityCheck (at:ctx) body
sanityCheck ctx (Apply f a) = case termType ctx f of
	TFun at dur rt -> _
	ty -> Just $ "Application of non-function type " <> show ty
