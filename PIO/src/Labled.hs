module Labled where
import GHC.Base (Applicative)
import Control.Monad.State.Strict (Functor)

newtype Labled l a = MkLabled a

instance Monad (Labled l) where
    return a = MkLabled a
    MkLabled a >>= k = MkLabled (let MkLabled b = k a in b)

instance Applicative (Labled l) where
    pure  = return 
    (<*>) = undefined

instance Functor (Labled l) where
    fmap = undefined

--Unsafe can only use internally
unLabled :: Labled l a -> a
unLabled (MkLabled a) = a