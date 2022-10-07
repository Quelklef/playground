{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Functor (($>))
import Data.List (intercalate)
import Data.Function (fix)
import Control.Monad (join, void, when)
import Data.Functor.Identity (Identity (..))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.CC
import qualified Data.IORef as Ref
import Data.Traversable (for)
import Unsafe.Coerce (unsafeCoerce)

-- toplevel monad
-- we aschew the region parameter
newtype Run a = Run (forall rg. CCT rg IO a)

newtype Prompt' a = Prompt' { unPrompt' :: forall rg. Prompt rg a }
newtype SubCont' m a b = SubCont' { unSubCont' :: forall rg. SubCont rg m a b }

unsafeToRun :: CCT rg IO a -> Run a
unsafeToRun = unsafeCoerce

unsafeToPrompt' :: Prompt rg a -> Prompt' a
unsafeToPrompt' = unsafeCoerce

unsafeToSubCont' :: SubCont rg m a b -> SubCont' m a b
unsafeToSubCont' = unsafeCoerce

unRun :: Run a -> (forall rg. CCT rg IO a)
unRun (Run a) = a

runRun :: Run a -> IO a
runRun (Run r) = runCCT r

instance Functor Run where
  fmap f (Run x) = Run (f <$> x)

instance Applicative Run where
  Run f <*> Run x = Run (f <*> x)
  pure x = Run (pure x)

instance Monad Run where
  return x = Run (pure x)
  Run ccx >>= f =
    Run $ do
      x <- ccx
      let ccy = unRun $ f x
      y <- ccy
      pure y

instance MonadIO Run where
  liftIO io = Run (liftIO io)

instance MonadDelimitedCont Prompt' (SubCont' IO) Run where
  newPrompt = Run (unsafeToPrompt' <$> newPrompt)
  pushPrompt (Prompt' p) (Run f) = Run (pushPrompt p f)
  withSubCont p sk = Run $ withSubCont (unPrompt' p) (\s -> unRun $ sk $ unsafeToSubCont' s)
  pushSubCont sk m = Run $ pushSubCont (unSubCont' sk) (unRun m)


newtype Pivot r = Pivot { runPivot :: forall x. (((x -> Run r) -> Run r) -> Run x) }

-- single control operator: like reset, but instead
-- of returning a prompt p to plug into shift, returns
-- an already-applied 'shift p' (referred to as "pivot")
anchor :: forall b. (Pivot b -> Run b) -> Run b
anchor func = Run $ do
  reset $ \prompt -> do
    let pivot = Pivot $ shift' (unsafeToPrompt' prompt)
    unRun $ func pivot

  where

  --   shift' prompt (\k -> k       v )
  -- = shift  prompt (\k -> k (pure v))
  shift'
    :: forall prompt s m a b
     . MonadDelimitedCont prompt s m
    => prompt b -> ((a -> m b) -> m b) -> m a
  shift' prompt handler =
    shift prompt (\k -> handler (\v -> k (pure v)))



-- -- -- --



data Effect ir or af = Effect
  { into :: ir -> or
  , mkAf :: Pivot or -> af
  }


main = do

  when False $
    (>>= print) $ runRun $ do
      with (reading ' ') $ \read -> do
        with writing $ \write -> do
          with forking $ \fork -> do
            (x :: Int) <- fork 1 2
            (y :: Int) <- fork 5 6
            spacer <- read
            write [ show x <> [spacer] <> show y ]
            pure ()

  when False $
    print $ runCC $ do
      reset $ \p0 -> do
        let fork a1 a2 =
              shift p0 $ \k -> do
                r1 <- k (pure a1)
                r2 <- k (pure a2)
                pure $ r1 <> r2
        it <- fork 1 2
        pure $ "which: " <> show it <> "; "

  when False $
    (>>= print) $ runRun $ do
      reset $ \p0 -> do
        ss <-
          shift p0 $ \k -> do
            let inp = ["fee", "fi", "fo", "fum"]
            out <- k (pure inp)
            (pure . Right) $ length out
        (pure . Left) (intercalate " " ss <> "!")



with :: forall

  af  -- afordance supplied to 'body'
  ir  -- inner result; result of 'body'
  or  -- outer result; result of 'with'

   . Effect ir or af
  -> (af -> Run ir)
  -> Run or

with (Effect { into, mkAf }) =
  \body ->
    anchor $ \pivot -> do
      let af = mkAf pivot
      r <- body af
      pure $ into r



reading :: forall r env. env -> Effect r r (Run env)
reading env = Effect
  { into = id
  , mkAf = \(Pivot pivot) -> pivot $ \k -> k env
  }

writing :: forall w a. Monoid w => Effect a (a, w) (w -> Run ())
writing = Effect
  { into = (, mempty)
  , mkAf =
      \(Pivot pivot) w' -> pivot $ \k -> do
        (r, w) <- k ()
        pure (r, w' <> w)
  }

forking :: Effect r [r] (a -> a -> Run a)
forking = Effect
  { into = (:[])
  , mkAf =
      \(Pivot pivot) -> \a1 a2 -> pivot $ \k -> do
        r1 <- k a1
        r2 <- k a2
        pure $ r1 <> r2
  }


