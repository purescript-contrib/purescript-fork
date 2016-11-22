{-
Copyright 2016 SlamData, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Control.Monad.Fork.Class where

import Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Monad.Trans.Class (lift)

class Monad m ⇐ MonadFork e m | m → e where
  fork ∷ ∀ a. m a → m (e → m Boolean)

instance monadForkAff ∷ MonadFork Error (Aff.Aff eff) where
  fork = map Aff.cancel <<< Aff.forkAff

instance monadForkReaderT ∷ MonadFork e m ⇒ MonadFork e (ReaderT r m) where
  fork (ReaderT ma) =
    ReaderT \r → map lift <$> fork (ma r)
