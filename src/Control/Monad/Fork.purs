module Control.Monad.Fork 
  ( module Class
  , cancelWith
  ) where

import Prelude

import Control.Monad.Fork.Class (class MonadBracket, class MonadFork, class MonadKill, BracketCondition(..), bracket, fork, join, kill, never, suspend, uninterruptible) as Class
import Control.Monad.Fork.Class (class MonadBracket, BracketCondition(..), bracket)

cancelWith ∷ ∀ err fiber m a. MonadBracket err fiber m ⇒ (err → m Unit) → m a → m a
cancelWith cancel a =
  bracket
    (pure unit)
    (\bc _ → case bc of
      Killed e → cancel e
      Completed _ → pure unit
      Failed e → pure unit)
    (const a)

