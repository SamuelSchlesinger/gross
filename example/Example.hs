import Graphics.Gross
import UI.NCurses
import Control.Monad.Trans

main = runCurses $ do
  display $ \x y -> if (x `mod` y) `mod` 4 == 1 then '@' else ' '
  render
  liftIO getChar
