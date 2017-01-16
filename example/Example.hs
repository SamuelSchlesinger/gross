{-# LANGUAGE TemplateHaskell, RecordWildCards, LambdaCase #-}

import Graphics.Gross
import UI.NCurses
import Control.Monad.Trans
import Control.Lens

data Config = Config { _width :: Integer, _height :: Integer }
makeLenses ''Config

data Player = Player { _playerX :: Integer, _playerY :: Integer }
makeLenses ''Player

data World = World { _config :: Config, _player :: Player, _stones :: Integer -> Integer -> Bool }
makeLenses ''World

consistent :: World -> Bool
consistent world = let 
  Config w h = world^.config
  Player x y = world^.player in (world^.stones) x y && x <= w && x >= 1 && y <= h && y >= 1

displayWorld :: World -> Integer -> Integer -> Char
displayWorld world y x 
  | world^.player.playerX == x && world^.player.playerY == y = '&'
  | (world^.stones) x y = 'O'
  | otherwise = ' '

eventWorld :: World -> Event -> Update World
eventWorld world = \case
  EventSpecialKey k -> case k of
    KeyUpArrow -> pure $ snd ((player.playerY <-~ 1) world) 
    KeyDownArrow -> pure $ snd ((player.playerY <+~ 1) world)
    KeyLeftArrow -> pure $ snd ((player.playerX <-~ 1) world)
    KeyRightArrow -> pure $ snd ((player.playerX <+~ 1) world) 

main = runCurses $ do
  s <- defaultWindow
  (w, h) <- updateWindow s windowSize
  let init = World (Config (w - 2) (h - 2)) (Player (w `div` 2) (h `div` 2)) (\x y -> if x `mod` 4 == 0 && y `mod` 3 == 2 then True else False)
  roguelike displayWorld eventWorld init 300
