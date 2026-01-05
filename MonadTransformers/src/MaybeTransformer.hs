module MaybeTransformer (MaybeT (..)) where
import Control.Monad (liftM)
import Control.Monad.Trans (MonadTrans (..))
import GHC.Base (ap)

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Monad m) => Monad (MaybeT m) where
  return = MaybeT . return . Just

  x >>= f = MaybeT $ do
    maybe_value <- runMaybeT x
    case maybe_value of
      Nothing -> return Nothing
      Just value -> runMaybeT $ f value

instance Monad m => Applicative (MaybeT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (MaybeT m) where
    fmap = liftM

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just