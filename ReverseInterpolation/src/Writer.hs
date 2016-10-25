module Writer where 

import           Control.Applicative()
import           Control.Monad()
import           Data.Functor()
import           Data.Monoid()

data Writer w a = Writer (a, w)

runWriter :: Writer w a -> (a, w)
runWriter (Writer a) = a

execWriter :: Writer w a -> w
execWriter p = snd $ runWriter p 

instance Functor (Writer w) where
    fmap f (Writer a) = Writer (f $ fst a, snd a)

instance Monoid w => Applicative (Writer w) where
    pure a = Writer (a, mempty)
    (Writer f) <*> a = fmap (fst f) a

instance Monoid w => Monad (Writer w) where
    (Writer a)  >>= f = let (x, u) = a
                            (y, v) = runWriter $ f x
                            in Writer (y, mappend u v)