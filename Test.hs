module Test (
      testWorld
    , testWorldCircle
    , testWorldBlob
    ) where

import Data.Traversable as T
import Data.Tuple
import Control.Monad as M
import Control.Monad.State.Lazy
import Control.Applicative
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
testWorldCircle = execState f makeEmptyWorld
    where
        f = let nodeCount = 8
                r = 100
                (cx, cy) = (200, 200)
                spring = Spring 1 50 0
                rotate n = take . length <*> drop n . cycle
            in do
            nodeList <- T.forM (drop 1 [0, 2 * pi / nodeCount .. 2 * pi]) $ \angle ->
                            insPointMass $ PointMass (Vector2D ((cos angle) * r + cx)
                                                               ((sin angle) * r + cy))
                                                     (Vector2D 0 0)
                                                     1
            M.sequence $ zipWith (insSpring spring) <*> rotate 1 $ nodeList

testWorldBlob :: World
testWorldBlob = execState f makeEmptyWorld
    where
        f = let nodeCount = 8
                r1 = 100
                r2 = 150
                (cx, cy) = (200, 200)
                spring = Spring 5 100 0
                rotate n = take . length <*> drop n . cycle
            in do
            nodeList1 <- T.forM (drop 1 [0, 2 * pi / nodeCount .. 2 * pi]) $ \angle ->
                            insPointMass $ PointMass (Vector2D ((cos angle) * r1 + cx)
                                                               ((sin angle) * r1 + cy))
                                                     (Vector2D 0 0)
                                                     1
            nodeList2 <- T.forM (drop 1 [0, 2 * pi / nodeCount .. 2 * pi]) $ \angle ->
                            insPointMass $ PointMass (Vector2D ((cos angle) * r2 + cx)
                                                               ((sin angle) * r2 + cy))
                                                     (Vector2D 0 0)
                                                     1
            M.sequence $ zipWith (insSpring spring) <*> rotate 1 $ nodeList1
            M.sequence $ zipWith (insSpring spring) <*> rotate 1 $ nodeList2
            M.sequence $ zipWith (insSpring spring) nodeList1 nodeList2
            M.sequence $ zipWith (insSpring spring) (rotate 1 nodeList1) nodeList2
            M.sequence $ zipWith (insSpring spring) nodeList1 (rotate 1 nodeList2)
