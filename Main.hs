import Haste
import Haste.Graphics.Canvas
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree as PT
import Data.Maybe
import Internal
import Types
import Test
import Vector
import GHC.Float
import Control.Monad.State.Lazy

-- drawPointMass returns a picture from a Node and a World
drawPointMass :: Node -> World -> Picture ()
drawPointMass node (World g) = picture
    where
        PointMass (Vector2D x y) _ _ = fromJust $ lab g node
        picture = do
            fill $ circle (float2Double x, float2Double y) 10

-- drawSpring returns a picture from a LEdge and a World
drawSpring :: LEdge Spring -> World -> Picture ()
drawSpring (n1, n2, _) (World g) = picture
    where
        PointMass (Vector2D x1 y1) _ _ = fromJust $ lab g n1
        PointMass (Vector2D x2 y2) _ _ = fromJust $ lab g n2
        picture = do
            stroke $ line (float2Double x1, float2Double y1) (float2Double x2, float2Double y2)

-- drawWorld draws all masses and springs
drawWorld :: World -> Picture ()
drawWorld w@(World g) = picture
    where
        nodePics = map (\n -> drawPointMass n w) $ nodes g
        edgePics = map (\e -> drawSpring e w) $ labEdges g
        picture = sequence_ (nodePics ++ edgePics)

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
loop :: Canvas -> State World (IO ())
loop can = do
    step 0.05
    world <- get
    return $ do
        render can (drawWorld world)
        setTimeout 20 $ evalState (loop can) world

main :: IO ()
main = do
    canvas <- mkCanvas 500 500
    addChild canvas documentBody
    Just can <- getCanvas canvas
    evalState (loop can) testWorldCircle
