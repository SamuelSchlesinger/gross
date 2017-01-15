module Graphics.Gross (display, roguelike) where

import Control.Monad
import Control.Monad.Trans
import UI.NCurses

display :: (Integer -> Integer -> Char) 
        -> Curses ()

display f = do
  screen <- defaultWindow
  updateWindow screen $ do
    (nrows, ncols) <- windowSize
    void $ sequence $ do
      x <- [0 .. nrows - 2]
      y <- [0 .. ncols - 1]
      pure $ do
        moveCursor x y
        drawString [f x y]

roguelike :: (world -> Integer -> Integer -> Char)
          -> (world -> Event -> world)
          -> world
          -> Integer
          -> Curses ()

roguelike f e w n = do
  s <- defaultWindow
  display (f w)
  render
  me <- getEvent s (Just n)
  case me of
    Nothing -> roguelike f e w n
    Just event -> roguelike f e (e w event) n

