{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Functor (($>))
import Data.List (intercalate)
import Data.Function (fix)
import Control.Monad (join, void, when)
import Data.Functor.Identity (Identity (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.CC
import qualified Data.IORef as Ref
import Data.Traversable (for)
import Unsafe.Coerce (unsafeCoerce)

-- toplevel monad
type Run rg a = CCT rg IO a

runRun :: forall a. (forall rg. Run rg a) -> IO a
runRun = runCCT


-- type of 'shift p' for some prompt p
type Shift rg r = forall x. (((x -> Run rg r) -> Run rg r) -> Run rg x)

data Effect rg ir or af = Effect
  { into :: ir -> or
  , mkAf :: Shift rg or -> af
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



  ----------------------------------------------------
  -- Here we discharge effects into the ambient monad,
  -- which allows us to achieve 'flatter' code.
  -- This seems to be closer to how Koka does it. Not
  -- really sure if I prefer it, though!

  when False $
    (>>= print) $ runRun $ do
      reset $ \p0 -> do
        write <- shift' p0 $ \k -> do
          (r, w) <- with writing k
          pure r
        write "a"
        write "b"
        pure ()

  let
    handling p0 withit f =
      shift' p0 $ \k -> f =<< withit k

  when False $
    (>>= print) $ runRun $ do
      reset $ \p0 -> do
        write <- handling p0 (with writing)
          $ \(r, w) -> do
            liftIO (print w)
            pure r
        write "a"
        write "b"
        pure ()

  when True $
    (>>= print) $ runRun $ do
      reset $ \p0 -> do
        read <- handling p0 (with $ reading ' ') pure
        write <- handling p0 (with writing)
                 $ \(r, w) -> liftIO (print w) $> r
        fork <- handling p0 (with forking)
                $ \(r:rs) -> liftIO (print (r:rs)) $> r

        (x :: Int) <- fork 1 2
        (y :: Int) <- fork 5 6
        spacer <- read
        write [ show x <> [spacer] <> show y ]
        pure ()


with :: forall

  rg  -- effect region
  af  -- afordance supplied to 'body'
  ir  -- inner result; result of 'body'
  or  -- outer result; result of 'with'

   . Effect rg ir or af
  -> (af -> Run rg ir)
  -> Run rg or

with (Effect { into, mkAf }) =
  \body ->
    reset $ \prompt -> do
      let af = mkAf (shift' prompt)
      r <- body af
      pure $ into r

--   shift' prompt (\k -> k       v )
-- = shift  prompt (\k -> k (pure v))
shift'
  :: forall prompt s m a b
   . MonadDelimitedCont prompt s m
  => prompt b -> ((a -> m b) -> m b) -> m a
shift' prompt handler =
  shift prompt (\k -> handler (\v -> k (pure v)))




reading :: forall rg r env. env -> Effect rg r r (Run rg env)
reading env = Effect
  { into = id
  , mkAf = \shift -> shift $ \k -> k env
  }

writing :: forall rg w a. Monoid w => Effect rg a (a, w) (w -> Run rg ())
writing = Effect
  { into = (, mempty)
  , mkAf =
      \shift w' -> shift $ \k -> do
        (r, w) <- k ()
        pure (r, w' <> w)
  }

forking :: Effect rg r [r] (a -> a -> Run rg a)
forking = Effect
  { into = (:[])
  , mkAf =
      \shift -> \a1 a2 -> shift $ \k -> do
        r1 <- k a1
        r2 <- k a2
        pure $ r1 <> r2
  }


