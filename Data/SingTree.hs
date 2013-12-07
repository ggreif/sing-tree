{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeOperators,
             StandaloneDeriving, RankNTypes #-}

module SingTree where

import GHC.TypeLits
import Data.Proxy
import Data.Thrist

hey = 1


data Tree :: [Symbol] -> * where
  Leaf :: Tree p
  Fork :: KnownSymbol n => Proxy n -> Tree p -> Tree (n ': p)
  Furk :: KnownSymbol n => Tree p -> Tree (n ': p)

deriving instance Show (Tree p)

t1 = Fork (Proxy :: Proxy "dd") Leaf
t2 = Furk Leaf :: forall p . Tree ("dd" ': p)

getNodeName :: Tree (n ': p) -> String
getNodeName t@(Furk _) = symbolVal (prox t)
  where prox :: Tree (n ': p) -> Proxy n
        prox _ = Proxy


