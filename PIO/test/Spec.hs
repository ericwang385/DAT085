{-# LANGUAGE RebindableSyntax, GADTs, TypeFamilies, UndecidableInstances, MultiParamTypeClasses, ScopedTypeVariables, DataKinds #-}

import Data.List
import Data.Maybe
import Effect
import Monad
import Purpose
import Labeled
import Prelude hiding (Monad(..))
import qualified Prelude as P

data Login
data Ads
data Register = Combine Login Ads
type DB = Labeled All [(Labeled Register String, Labeled Login String)]

main :: IO()
main = undefined

existUser :: DB -> Labeled Register String -> Labeled Register Bool
existUser db name = do
            db' <- db
            name' <- name
            return False
