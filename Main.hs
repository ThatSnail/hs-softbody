import Haste
import Haste.Graphics.Canvas
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree as PT
import Data.MemoCombinators as Memo
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
    , frameNum :: Int
    , startTime :: UTCTime
}

-- drawPointMass returns a picture from a Node and a World
drawPointMass :: PointMass -> World -> Picture ()
drawPointMass (PointMass (Vector2D x y) _ _) (World g) = stroke shape
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
drawWorld w@(World g) = picture
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
step timeStep = do
    updateWorld timeStep

-- Main loop includes drawing and stuff
loop :: GlobalState -> IO (State World (IO ()))
loop globState@(GlobalState canvas frameNum startTime) = do
    curTime <- getCurrentTime
    return $ do
        step 0.1
        world <- get
        return $ do
            render canvas $ do
                drawWorld world
                let timeElapsed = realToFrac $ diffUTCTime curTime startTime :: Float
                text (20, 20) $ "Frame #: " ++ show frameNum
                text (20, 30) $ "Time Elapsed: " ++ show timeElapsed
                text (20, 40) $ "FPS: " ++ show ((fromIntegral frameNum) / timeElapsed)
            s <- loop $ GlobalState canvas (frameNum+1) startTime
            setTimeout 10 $ evalState s world

main :: IO ()
main = do
    curTime <- getCurrentTime
    canvas <- mkCanvas world_w world_h
    addChild canvas documentBody
    Just can <- getCanvas canvas
    curTime <- getCurrentTime
    s <- loop $ GlobalState can 0 curTime
    evalState s testWorldBlob
