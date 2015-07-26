import Haste
import Haste.LocalStorage
import Haste.Graphics.Canvas
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree as PT
import Data.Either
import Data.Maybe
import Internal
import Types
import Test
import Params
import Vector
import GHC.Float
import Control.Monad.State.Lazy
import Data.Time.Clock

data GlobalState = GlobalState {
      canvas :: Canvas
    , elemCanvas :: Elem
    , frameNum :: Int
    , startTime :: UTCTime
    , lastTime :: UTCTime
}

-- drawPointMass returns a picture from a Node and a World
drawPointMass :: PointMass -> World -> Picture ()
drawPointMass (PointMass (Vector2D x y) _ _) _ = stroke shape
    where
        shape = do
            circle (float2Double x, float2Double y) 3

-- drawSpring returns a picture from a LEdge and a World
drawSpring :: PointMass -> PointMass -> Spring -> World -> Picture ()
drawSpring pm1 pm2 s w = 
    color (RGBA 0 0 0 (float2Double $ springConst s)) $
    stroke $ line (float2Double x1, float2Double y1) (float2Double x2, float2Double y2)
    where
        g = graph w
        Vector2D x1 y1 = pos pm1
        Vector2D x2 y2 = pos pm2

-- drawWorld draws all masses and springs
drawWorld :: World -> Picture ()
drawWorld w@(World g _) = picture
    where
        nodePic = do
            forM_ (labNodes g) $ \(_, pm) ->
                drawPointMass pm w
        edgePic = do
            let memNodeToPM = fromJust . lab g
            forM_ (labEdges g) $ \(n1, n2, s) ->
                let pm1 = memNodeToPM n1
                    pm2 = memNodeToPM n2
                in drawSpring pm1 pm2 s w
        picture = do
            nodePic
            edgePic

mkCanvas :: Int -> Int -> IO Elem
mkCanvas width height = do
    canvas <- newElem "canvas"
    setStyle canvas "border" "1px solid black"
    setStyle canvas "backgroundColor" "white"
    setProp canvas "width" (show width)
    setProp canvas "height" (show height)
    return canvas

step :: Float -> State World ()
step timeStep = updateWorld timeStep

-- Main loop includes drawing and stuff
loop :: GlobalState -> IO (State World (IO ()))
loop globState@(GlobalState canvas elemCanvas frameNum startTime lastTime) = do
    curTime <- getCurrentTime
    eitherXY <- getItem "clickedLast" :: IO (Either String (Float, Float))
    let doClick timeStep = either (\_ -> return ()) (\(x, y) -> forceGravityPoint timeStep (Vector2D x y)) eitherXY
    return $ do
        let ts = 0.01
        step ts
        doClick ts
        world <- get
        return $ do
            render canvas $ do
                drawWorld world
                let timeElapsed = realToFrac $ diffUTCTime curTime startTime :: Float
                let timeLastFrame = realToFrac $ diffUTCTime curTime lastTime :: Float
                text (20, 20) $ "Frame #: " ++ show frameNum
                text (20, 30) $ "Time Elapsed: " ++ show timeElapsed
                text (20, 40) $ "FPS: " ++ (take 5 $ show (1 / timeLastFrame))
            s <- loop $ GlobalState canvas elemCanvas (frameNum+1) startTime curTime
            Right ws <- getItem "worldStr" :: IO (Either String String)
            if ws == worldStr world
            then setTimeout 10 $ evalState s world
            else return ()

constructMenu :: IO ()
constructMenu = do
    b <- newElem "button"
    setProp b "innerHTML" "test"
    
makeWorld :: GlobalState -> World -> IO ()
makeWorld gs world = do
    setItem "worldStr" $ worldStr world
    s <- loop gs 
    evalState s world

main :: IO ()
main = do
    curTime <- getCurrentTime
    Just elemCanvas <- elemById "canvas"
    Just canvas <- getCanvas elemCanvas
    setItem "worldStr" $ "testWorldRod"
    -- Hooray!  Ugly IO hacks!
    onEvent elemCanvas OnMouseDown $ \_ point -> do
        setItem "clickedLast" point
    onEvent elemCanvas OnMouseUp $ \_ point -> do
        removeItem "clickedLast"
    onEvent elemCanvas OnMouseMove $ \point -> do
        e <- getItem "clickedLast" :: IO (Either String (Float, Float))
        either (\_ -> return ())
               (\_ -> setItem "clickedLast" point) e
    -- Button events
    let gs = GlobalState canvas elemCanvas 0 curTime curTime
        clickEvent e world = onEvent e OnClick $ \_ _ -> do
            Right ws <- getItem "worldStr"
            if ws /= worldStr world
            then makeWorld gs world
            else return ()
    makeWorld gs testWorldRod
    Just elem <- elemById "btnRod"
    clickEvent elem testWorldRod
    Just elem <- elemById "btnCircle"
    clickEvent elem testWorldCircle
    Just elem <- elemById "btnRigidSkin"
    clickEvent elem testWorldRigidSkin
    Just elem <- elemById "btnBlob"
    clickEvent elem testWorldBlob
    Just elem <- elemById "btnGrid"
    clickEvent elem testWorldGrid
    return ()
