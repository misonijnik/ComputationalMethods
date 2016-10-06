module Reader where

import           Control.Applicative ()
import           Control.Monad       ()
import           Data.Functor        ()
import           Data.Monoid         ()

data Reader env a = Reader (env -> a)

runReader :: Reader env a -> env -> a
runReader (Reader f) = f

instance Functor (Reader env) where
    fmap f (Reader a) = Reader $ f . a

instance Applicative (Reader env) where
    pure a = Reader $ const a
    (Reader f) <*> (Reader a) = Reader $ \env -> f env (a env)

instance Monad (Reader env) where
    return a = Reader $ const a
    ma >>= mf = Reader $ \env ->
        let b = runReader ma env
            in runReader (mf b) env
