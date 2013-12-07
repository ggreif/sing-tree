{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators,
             StandaloneDeriving, RankNTypes, ScopedTypeVariables #-}

module SingTree where

import GHC.TypeLits
import Data.Proxy
import Data.Thrist


data Tree :: [Symbol] -> * where
  Leaf :: Tree p
  Fork :: KnownSymbol n => Proxy n -> Tree p -> Tree (n ': p)
  Furk :: KnownSymbol n => Tree p -> Tree (n ': p)

deriving instance Show (Tree p)

t1 = Fork (Proxy :: Proxy "dd") Leaf
t2 = Furk Leaf :: forall p . Tree ("dd" ': p)
t3 = Furk t2 :: forall p . Tree ("hh" ': "dd" ': p)

getNodeName :: Tree (n ': p) -> String
getNodeName t@(Furk _) = symbolVal (prox t)
  where prox :: Tree (n ': p) -> Proxy n
        prox _ = Proxy


class KnownSymbolicPath (p :: [Symbol]) where
  pathSing :: SSymbolPath p

instance KnownSymbolicPath '[] where
  pathSing = SSymbolPath []

instance (KnownSymbol n, KnownSymbolicPath p) => KnownSymbolicPath (n ': p) where
  pathSing = SSymbolPath (SomeSymbol (Proxy :: Proxy n) : rest)
    where SSymbolPath rest = pathSing :: SSymbolPath p

newtype SSymbolPath (s :: [Symbol]) = SSymbolPath [SomeSymbol]
