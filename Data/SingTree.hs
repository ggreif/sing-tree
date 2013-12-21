{-# LANGUAGE DataKinds, PolyKinds, KindSignatures, GADTs, TypeOperators,
             StandaloneDeriving, RankNTypes, ScopedTypeVariables,
             ConstraintKinds, TypeFamilies #-}

module SingTree where

import GHC.TypeLits
import GHC.Exts
import Data.Proxy
import Data.Thrist

-- we want something like a directory tree
-- Root, Directory, Node

-- Mid-term TODO: lexicographic ordering
-- Long term todo: diffing of (sub)trees

-- The tree must be parametrized in the list of all
-- the paths it contains

data Tree :: [Symbol] -> * where
  Root :: Tree '[]
  Leaf :: Tree p
  Fork :: KnownSymbol n => Tree p -> Tree (n ': p)

deriving instance Show (Tree p)

t0 = Root
t2 = Fork Leaf :: forall p . Tree ("dd" ': p)
t3 = Fork t2 :: forall p . Tree ("hh" ': "dd" ': p)

getNodeName :: Tree (n ': p) -> String
getNodeName t@(Fork _) = symbolVal (prox t)
  where prox :: Tree (n ': p) -> Proxy n
        prox _ = Proxy

type family AppendPath (a :: [Symbol]) (b :: [Symbol]) :: [Symbol] where
  AppendPath '[] b = b
  AppendPath (a ': as) b = a ': AppendPath as b

-- Stuff for singleton paths
--
class KnownSymbolicPath (p :: [Symbol]) where
  pathSing :: SSymbolPath p

instance KnownSymbolicPath '[] where
  pathSing = SSymbolPath []

instance (KnownSymbol n, KnownSymbolicPath p) => KnownSymbolicPath (n ': p) where
  pathSing = SSymbolPath (SomeSymbol (Proxy :: Proxy n) : rest)
    where SSymbolPath rest = pathSing :: SSymbolPath p

newtype SSymbolPath (s :: [Symbol]) = SSymbolPath [SomeSymbol]

-- UNRELATED?
-- Toying around (this was part of a dream; I know this is crazy)

data Foo (constr :: k -> Constraint) :: k -> * where
  Bar :: constr a => Proxy a -> Foo constr a

-- can we create an LE witness from an LE constraint?