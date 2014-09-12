import Graphics.UI.SDL
import Control.Monad
import Control.Applicative
import System.Random
import Control.Monad.Identity
import Control.Arrow hiding (loop)

windowW, windowH :: Double
windowW = 800
windowH = 600

main :: IO ()
main = withInit [InitEverything] $ do
    screen <- setVideoMode (floor windowW) (floor windowH) 32 [SWSurface]
    loop screen (None,None) =<< simpleFrame

paddle, ball, initBall, initLeftPad, initRightPad :: Rect
paddle = Rect 0 0 (floor padW) (floor padH)
ball =  Rect 0 0 (floor ballW) (floor ballW)
initBall = ball `addR` (windowW / 2 - ballW/2, windowH/2 - ballW/2)
initLeftPad = paddle `addR` (5, windowH/2 - padH/2) 
initRightPad = paddle `addR` (windowW-35, windowH/2 - padH/2) 
padW, padH, ballW :: Double
ballW = 20
padH = 100
padW = 30

type Speed = (Double, Double)
zeroSpeed :: Speed
zeroSpeed = (0,0)

data Frame = Frame Rect Rect Rect Speed StdGen

data InputP = Up | Down | None
type Input = (InputP, InputP)

simpleFrame :: IO Frame
simpleFrame = Frame initLeftPad initRightPad initBall zeroSpeed <$> getStdGen

addR :: Rect -> Speed -> Rect
addR (Rect x y w h) (x', y') = Rect (x + floor x') (y + floor y') w h

loop :: Surface -> Input -> Frame -> IO ()
loop screen input frame = do
    delay 50
    Graphics.UI.SDL.flip screen
    (quitEvent, input') <- whileEvents input
    let frame' = physics frame input'
    draw screen frame
    unless quitEvent (loop screen input' frame')

whenF :: Monad m => Bool -> (a -> a) -> a -> m a
whenF b f1 a = return $ if b then f1 a else a

physics :: Frame -> Input -> Frame
physics (Frame l r b s rng) (lInput, rInput) = runIdentity $ do
    (s, rng) <- whenF (s == zeroSpeed) launchBall (s, rng)
    b@(Rect x y _ _) <- return $ b `addR` s 
    s <- whenF (y < 0) (second abs) s
    s <- whenF (fromIntegral y > (windowH - ballW)) (second (negate . abs)) s
    (s, rng) <- whenF (rectOverlap l b) (first (first abs) . launchBall) (s, rng)
    (s, rng) <- whenF (rectOverlap r b) (first (first (negate . abs)) . launchBall) (s, rng)
    l <- return $ l `addR` inputSpeed lInput
    r <- return $ r `addR` inputSpeed rInput
    whenF (fromIntegral x < 0 || fromIntegral x > windowW - ballW) 
        (const (Frame initLeftPad initRightPad initBall zeroSpeed rng)) 
        (Frame l r b s rng)
  where
    launchBall (_, rng') = runIdentity $ do
        (dir, rng') <- return $ random rng'
        (x, rng') <- whenF dir (first negate) (randomR (15,30) rng')
        (y, rng') <- return $ randomR (-15,15) rng'
        return ((x,y), rng')
    rectOverlap :: Rect -> Rect -> Bool
    rectOverlap (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) 
      | x1 >= x2 + w2 = False
      | x2 >= x1 + w1 = False
      | y1 >= y2 + h2 = False
      | y2 >= y1 + h1 = False
      | otherwise     = True
    inputSpeed None = (0,0)
    inputSpeed Up = (0,-15)
    inputSpeed Down = (0,15)

draw :: Surface -> Frame -> IO ()
draw screen (Frame l r b _ _) = do
    void $ fillRect screen Nothing (Pixel 0x00000000)
    f l >> f r >> f b
  where
    p = Pixel 0xFFFFFFFF
    f rect = void (fillRect screen (Just rect) p)

whileEvents :: Input -> IO (Bool, Input)
whileEvents prevInput@(l,r) = do
    event <- pollEvent
    case event of
        Quit -> return (True, prevInput)
        KeyDown (Keysym SDLK_ESCAPE _ _) -> return (True, prevInput)
        KeyDown (Keysym SDLK_UP _ _) -> whileEvents (l,Up)
        KeyUp (Keysym SDLK_UP _ _) -> whileEvents (l,None)
        KeyDown (Keysym SDLK_DOWN _ _) -> whileEvents (l,Down) 
        KeyUp (Keysym SDLK_DOWN _ _) -> whileEvents (l,None) 
        KeyDown (Keysym SDLK_LSHIFT _ _) -> whileEvents (Up,r)
        KeyUp (Keysym SDLK_LSHIFT _ _) -> whileEvents (None,r)
        KeyDown (Keysym SDLK_LCTRL _ _) -> whileEvents (Down,r)
        KeyUp (Keysym SDLK_LCTRL _ _) -> whileEvents (None,r)
        NoEvent -> return (False, prevInput)
        _ -> whileEvents prevInput
