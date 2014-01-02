{-# LANGUAGE DataKinds, PolyKinds, KindSignatures, GADTs, TypeOperators,
             StandaloneDeriving, RankNTypes, ScopedTypeVariables,
             ConstraintKinds, TypeFamilies, UndecidableInstances #-}

module Data.SingTree where

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

data List :: [[Symbol]] -> * where
  Ni :: List '[]
  Sn :: Apart ts t => List ts -> Tree' t -> List (ts `Append` t)

type family Roots (as :: [[Symbol]]) :: [Symbol] where
  Roots '[] = '[]
  Roots ('[r] ': ps) = r ': Roots ps
  Roots ((r ': rs) ': ps) = Roots (rs ': ps)

type Apart (l :: [[Symbol]]) (r :: [[Symbol]]) = '[] ~ Intersect (Roots l) (Roots r)



type family Intersect (l :: [k]) (r :: [k]) :: [k] where
  Intersect '[] r = '[]
  Intersect l '[] = '[]
  Intersect (l ': ls) r = InterAppend (Intersect ls r) r l

-- We need a helper to find one elem and return it appended if there

type family InterAppend (l :: [k]) (r :: [k]) (one :: a) :: [k] where
  InterAppend acc '[] one = acc
  InterAppend acc (one ': rs) one = one ': acc
  InterAppend acc (r ': rs) one = InterAppend acc rs one

--type family Intersect (l :: [k]) (r :: [k]) :: [k] where
--  Intersect '[] r = '[]
--  Intersect l '[] = '[]
--  Intersect (l ': ls) (l ': rs) = l ': Intersect ls rs
--  Intersect (l ': ls) (r ': rs) = Intersect (l ': ls) rs
--  -- TODO: buggy

deriving instance Show (List t)

data Tree' :: [[Symbol]] -> * where
  File :: KnownSymbol file => Tree' '[ '[file] ]
  Dir :: KnownSymbol dir => Proxy dir -> List many -> Tree' (many `AllAppend` dir)

deriving instance Show (Tree' t)

type family AllAppend (as :: [[Symbol]]) (b :: Symbol) :: [[Symbol]] where
  AllAppend '[] b = '[]
  AllAppend (p ': ps) b = Append p '[b] ': AllAppend ps b


r0 = File :: Tree' '[ '["my.txt"] ]
r0' = File :: Tree' '[ '["yours.txt"] ]
r1 = Ni `Sn` r0 `Sn` r0'

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

type family Append (a :: [k]) (b :: [k]) :: [k] where
  Append '[] b = b
  Append (a ': as) b = a ': Append as b

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

-- can we create an LE witness (data type) from an LE constraint?
