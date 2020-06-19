{-# LANGUAGE LambdaCase #-}
{- |
Module: UI.NCurses.Gross
Description: An event loop for ncurses
Copyright: (c) Samuel Schlesinger 2020
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows

Gross is a library written to expose a restricted way of running an ncurses
program in Haskell. It is meant to encourage the user to run their application
in this harness, and the harness should be developed to be the minimal one
such that this is possible.
-}
module UI.NCurses.Gross (
  -- ** Program Harness
  {- |
    These functions are used to run ncurses programs in an event loop.
  -}
  ncurses,
  ncursesIO,
  Response(NoUpdate, UpdatedPlzRender, UpdatedDontRender, Stop),
  module UI.NCurses
) where

import UI.NCurses
import Control.Monad.IO.Class

-- | Represents a response to user input
data Response model
  = NoUpdate
    -- ^ No update to the model
  | UpdatedPlzRender model
    -- ^ Model updated, render requested
  | UpdatedDontRender model
    -- ^ Model updated, render not requested
  | Stop
    -- ^ Stop the program

-- | A convenience function for running ncurses in an event loop over a
-- stateful model.
ncurses
  :: (Event -> model -> Response model)
     -- ^ How your program responds to events
  -> (model -> Update a)
     -- ^ How your program is renderered given a blank screen and the cursor at (0, 0)
  -> model
     -- ^ The initial model
  -> IO ()
ncurses remodel = ncursesIO (\e m -> pure (remodel e m))

-- | A convenience function for running ncurses in an event loop in the
-- IO monad.
ncursesIO
  :: (Event -> model -> IO (Response model))
     -- ^ How your program responds to events
  -> (model -> Update a)
     -- ^ How your program is renderered given a blank screen and the cursor at (0, 0)
  -> model
     -- ^ The initial model
  -> IO ()
ncursesIO remodel display initial = runCurses $ do
  setEcho False
  window <- defaultWindow
  go window initial
  where
    renderModel window model = do
      updateWindow window $ do
        moveCursor 0 0
        clear
        display model
      render
    go window model = getEvent window Nothing >>= \case
      Nothing -> error "Invalidates an invariant of getEvent"
      Just event -> liftIO (remodel event model) >>= \case
        NoUpdate -> go window model
        UpdatedPlzRender model' -> renderModel window model' *> go window model'
        UpdatedDontRender model' -> go window model'
        Stop -> pure ()
