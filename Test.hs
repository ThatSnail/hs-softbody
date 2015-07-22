module Test (
      testWorld
    , testWorldCircle
    , testWorldCircleMoving
    ) where

import Data.Traversable
import Data.Tuple
import Control.Monad as M
import Control.Monad.State.Lazy
import Internal
import Vector
import Types

testWorld :: World
testWorld = execState f makeEmptyWorld
    where
        f = do
            node1 <- insPointMass (PointMass (Vector2D 100 100) (Vector2D 0 0) 1)
            node2 <- insPointMass (PointMass (Vector2D 200 200) (Vector2D 0 0) 1)
            insSpring (Spring 1 50 0) node1 node2

testWorldCircle :: World
testWorldCircle = execState (M.sequence $ map (\(a, b) -> insSpring spring a b) $ zip nodeList (drop 1 $ cycle nodeList)) newWorld
    where
        nodeCount = 8
        radius = 100
        (cx, cy) = (200, 200)
        spring = Spring 10 50 0.2
        getPM angle = PointMass (Vector2D ((cos angle) * radius + cx)
                                     ((sin angle) * radius + cy)) (Vector2D 0 0) 1
        angles = drop 1 [0, 2 * pi / nodeCount .. 2 * pi]
        nodeActions = map (insPointMass . getPM) angles
        (newWorld, nodeList) = mapAccumL g makeEmptyWorld nodeActions
            where
                g w action = swap $ runState action w

testWorldCircleMoving :: World
testWorldCircleMoving = execState (M.sequence $ map (\(a, b) -> insSpring spring a b) $ zip nodeList (drop 1 $ cycle nodeList)) newWorld
    where
        nodeCount = 8
        radius = 100
        (cx, cy) = (200, 200)
        vel = Vector2D 1 1
        spring = Spring 10 50 0.2
        getPM angle = PointMass (Vector2D ((cos angle) * radius + cx)
                                     ((sin angle) * radius + cy)) vel 1
        angles = [0, 2 * pi / nodeCount .. 2 * pi]
        nodeActions = map (insPointMass . getPM) angles
        (newWorld, nodeList) = mapAccumL g makeEmptyWorld nodeActions
            where
                g w action = swap $ runState action w
