module Control.Monad.Fork.Canceler where

import Prelude

import Control.Monad.Eff.Exception (Error)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)

-- | Semigroup (and monoid) for combining cancel functions produced by `fork`.
newtype Canceler m = Canceler (Error → m Boolean)

derive instance newtypeCanceler ∷ Newtype (Canceler m) _

instance semigroupCanceler ∷ Apply m ⇒ Semigroup (Canceler m) where
  append (Canceler f1) (Canceler f2) = Canceler (\e → (||) <$> f1 e <*> f2 e)

instance monoidCanceler ∷ Applicative m ⇒ Monoid (Canceler m) where
  mempty = Canceler (const (pure false))

cancel ∷ ∀ m. Canceler m → Error → m Boolean
cancel (Canceler f) = f
