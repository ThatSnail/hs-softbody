module Test (
      testWorldRod
    , testWorldCircle
    , testWorldRigidSkin
    , testWorldBlob
    , testWorldGrid
    ) where

import Data.Tuple
import Control.Monad as M
import Control.Monad.State.Lazy
import Control.Applicative
import Internal
import Vector
import Types

testWorldRod :: World
testWorldRod = execState f $ makeEmptyWorld "testWorldRod"
    where
        f = do
            node1 <- insPointMass (PointMass (Vector2D 100 100) (Vector2D 0 0) 1)
            node2 <- insPointMass (PointMass (Vector2D 200 200) (Vector2D 0 0) 1)
            insSpring (Spring 1 50 0) node1 node2

testWorldCircle :: World
testWorldCircle = execState f $ makeEmptyWorld "testWorldCircle"
    where
        f = let nodeCount = 32
                r = 100
                (cx, cy) = (400, 200)
                spring = Spring 1 25 0
                rotate n = take . length <*> drop n . cycle
            in do
            nodeList <- M.forM (drop 1 [0, 2 * pi / nodeCount .. 2 * pi]) $ \angle ->
                            insPointMass $ PointMass (Vector2D ((cos angle) * r + cx)
                                                               ((sin angle) * r + cy))
                                                     (Vector2D 0 0)
                                                     1
            M.sequence $ zipWith (insSpring spring) <*> rotate 1 $ nodeList

testWorldBlob :: World
testWorldBlob = execState f $ makeEmptyWorld "testWorldBlob"
    where
        f = let nodeCount = 16
                r1 = 100
                r2 = 150
                (cx, cy) = (400, 200)
                innerSpring = Spring 0.3 70 0.5
                outerSpring = Spring 1 50 0.5
                rigidSpring = Spring 3 70 0.5
                rotate n = take . length <*> drop n . cycle
            in do
            centerNode <- insPointMass $ PointMass (Vector2D cx cy) (Vector2D 0 0) 1
            nodeList1 <- M.forM (drop 1 [0, 2 * pi / nodeCount .. 2 * pi]) $ \angle ->
                            insPointMass $ PointMass (Vector2D ((cos angle) * r1 + cx)
                                                               ((sin angle) * r1 + cy))
                                                     (Vector2D 0 0)
                                                     1
            nodeList2 <- M.forM (drop 1 [0, 2 * pi / nodeCount .. 2 * pi]) $ \angle ->
                            insPointMass $ PointMass (Vector2D ((cos angle) * r2 + cx)
                                                               ((sin angle) * r2 + cy))
                                                     (Vector2D 0 0)
                                                     1
            M.sequence_ $ zipWith (insSpring outerSpring) <*> rotate 1 $ nodeList1
            M.sequence_ $ zipWith (insSpring outerSpring) <*> rotate 1 $ nodeList2
            M.sequence_ $ zipWith (insSpring innerSpring) nodeList1 nodeList2
            M.sequence_ $ zipWith (insSpring innerSpring) (rotate 1 nodeList1) nodeList2
            M.sequence_ $ zipWith (insSpring innerSpring) nodeList1 (rotate 1 nodeList2)
            M.forM nodeList1 $ insSpring rigidSpring centerNode

testWorldRigidSkin :: World
testWorldRigidSkin = execState f $ makeEmptyWorld "testWorldRigidSkin"
    where
        f = let nodeCount = 16
                r = 100
                (cx, cy) = (400, 200)
                spring1 = Spring 5 50 0.1
                spring2 = Spring 5 60 0.1
                spring3 = Spring 5 70 0.1
                rotate n = take . length <*> drop n . cycle
            in do
            nodeList <- M.forM (drop 1 [0, 2 * pi / nodeCount .. 2 * pi]) $ \angle ->
                           insPointMass $ PointMass (Vector2D ((cos angle) * r + cx)
                                                              ((sin angle) * r + cy))
                                                    (Vector2D 0 0)
                                                    1
            M.sequence_ $ zipWith (insSpring spring1) <*> rotate 1 $ nodeList
            M.sequence_ $ zipWith (insSpring spring2) <*> rotate 2 $ nodeList
            M.sequence_ $ zipWith (insSpring spring3) <*> rotate 3 $ nodeList

testWorldGrid :: World
testWorldGrid = execState f $ makeEmptyWorld "testWorldGrid"
    where
        f = let spring = Spring 50 50 0.1
            in do
                nodeListXY <- M.forM [0, 1..5] $ \x -> do
                    nodeListY <- M.forM [0, 1..5] $ \y -> do
                        let sx = x * 50
                            sy = y * 50
                        insPointMass (PointMass (Vector2D (400 + sx) (50 + sy)) (Vector2D 0 0) 1)
                    M.sequence_ $ zipWith (insSpring spring) <*> drop 1 $ nodeListY
                    return nodeListY
                M.sequence_ $ concat $ zipWith (zipWith (insSpring spring)) (drop 1 nodeListXY) nodeListXY
