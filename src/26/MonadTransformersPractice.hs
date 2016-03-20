{-# LANGUAGE InstanceSigs #-}
module MonadTransformersPractice where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap :: Functor m => (a -> b) -> EitherT e m a -> EitherT e m b
  fmap f (EitherT mEea) = EitherT $ (fmap . fmap) f mEea

instance Applicative m => Applicative (EitherT e m) where
  pure :: Applicative m => a -> EitherT e m a
  pure = EitherT . pure . pure

  (EitherT eef) <*> (EitherT eea) = EitherT $ (<*>) <$> eef <*> eea

instance Monad m => Monad (EitherT e m) where
  return = pure

  (>>=) :: (Monad m) => EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
  EitherT mema >>= f = EitherT $ do
    ema <- mema
    case ema of
        Left a -> return $ Left a
        Right a -> runEitherT $ f a

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT meea) = EitherT $ fmap swapEither meea

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT meab) = meab >>= either f g

--ReaderT
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure a = ReaderT (pure (pure a))
  (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT rma) >>= f = ReaderT $ \r -> do
    a <- rma r
    runReaderT (f a) r

--StateT
newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Functor m) => Functor (StateT s m) where
  fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT smas) = StateT $ \s -> fmap f' (smas s) where
    f' (a, s) = (f a, s)

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT smfs) <*> (StateT smas) = StateT $ \s -> do
    (f, _) <- smfs s
    (a, _) <- smas s
    return (f a, s)

instance (Monad m) => Monad (StateT s m) where
  return = pure
  (StateT smas) >>= f = StateT $ \s -> do
    (a, _) <- smas s
    runStateT (f a) s

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> liftM (\v -> (v, s)) ma

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor f) => Functor (MaybeT f) where
  fmap :: (a -> b) -> MaybeT f a -> MaybeT f b
  fmap f (MaybeT fa) = MaybeT $ (fmap .fmap) f fa

instance (Applicative m) => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  MaybeT mf <*> MaybeT ma = MaybeT $ (<*>) <$> mf <*> ma

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f = MaybeT $ do
    v <- ma
    case v of
      Nothing -> return Nothing
      Just a -> runMaybeT $ f a

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

instance MonadTrans MaybeT where
  lift :: (Monad m) => m a -> MaybeT m a
  lift = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance MonadTrans (ReaderT r) where
  lift :: (Monad m) => m a -> ReaderT r m a
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO

