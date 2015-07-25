module Internal (
      PointMass ( PointMass, pos, vel )
    , Spring ( Spring, springConst, damping )
    , World ( World, graph )
    , makeEmptyWorld
    , insPointMass
    , insSpring
    , updateWorld
    ) where

import Types
import Params
import Vector
import Data.List
import Data.Maybe
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree as PT
import Control.Monad.State.Lazy

-- A PointMass is a point mass connected to a series of springs
data PointMass = PointMass {
        pos :: Vector2D
      , vel :: Vector2D
      , mass :: Mass
    } deriving (Show)

-- A Spring is connected to two PointMasses
data Spring = Spring {
      springConst :: Float
    , eqDist      :: Float
    , damping     :: Float -- Damping ratio
    } deriving (Eq, Show)

-- A World is a wrapper around a DynGraph, plus some world-y information
data World = World {
      graph :: PT.Gr PointMass Spring
    }

-- makeEmptyWorld creates an empty World, ready to add masses and springs to
makeEmptyWorld :: World
makeEmptyWorld = World empty

-- insPointMass makes a Node for us to attach to a piece of sellophane tape
-- and keep in our pockets for a good luck wish
insPointMass :: PointMass -> State World Node
insPointMass pm = do
    (World g) <- get
    let newNode = (noNodes g) + 1
    put $ World $ insNode (newNode, pm) g
    return newNode

-- insSpring makes an Edge for us to attach to a piece of sellophane tape
-- and keep in our pockets for a good luck wish
insSpring :: Spring -> Node -> Node -> State World (LEdge Spring)
insSpring s a b = do
    (World g) <- get
    put $ World $ insEdge newEdge g
    return newEdge
        where
            newEdge = (a, b, s)

updateWorld :: Float -> State World ()
updateWorld timeStep = do
    (World g) <- get
    put $ World (execState update g)
        where
            update = do applyTimeEvolution timeStep
--                        applySpringForce timeStep
                        applyGravity timeStep

-- Collisions here too!
applyTimeEvolution :: Float -> State (PT.Gr PointMass Spring) ()
applyTimeEvolution timeStep = do
    g <- get
    put $ nmap f g
        where 
            f :: PointMass -> PointMass
            f (PointMass (Vector2D opx opy) (Vector2D ovx ovy) m) =
                let (mpx, mpy) = (opx + ovx, opy + ovy)
                    (mvx, mvy) = (ovx, ovy)
                    (px, vx) = if mpx < 0
                               then (-mpx, -mvx)
                               else (if mpx > world_w
                                     then (world_w - (mpx - world_w), -mvx)
                                     else (mpx, mvx))
                    (py, vy) = if mpy < 0
                               then (-mpy, -mvy)
                               else (if mpy > world_h
                                     then (world_h - (mpy - world_h), -mvy)
                                     else (mpy, mvy))
                in PointMass (Vector2D px py) (Vector2D vx vy) m

applySpringForce :: Float -> State (PT.Gr PointMass Spring) ()
applySpringForce timeStep = do
    g <- get
    let f p = do forM p $ \(spring, otherNode) ->
                    enactSpringForce timeStep (fromJust $ lab g otherNode) spring
    put $ gmap (\(ps, n, l, ss) -> (ps, n, execState (f $ nub $ ps ++ ss) l, ss)) g

applyGravity :: Float -> State (PT.Gr PointMass Spring) ()
applyGravity timeStep = do
    g <- get
    let gvec = Vector2D 0 9.81 in
        put $ nmap (\(PointMass p v m) -> PointMass p (v .+ (gvec .* timeStep)) m) g

enactSpringForce :: Float -> PointMass -> Spring -> State PointMass ()
enactSpringForce timeStep (PointMass op _ _) (Spring k eq damping) = do
   (PointMass p v m) <- get
   let dv = op .- p
       Vector2D dx dy = dv
       angleOtherToMe = atan2 dy dx
       distOtherToMe = mag dv
       distFromEq = eq - distOtherToMe
       forceMag = -k * distFromEq
       acc = forceMag / m
       accVec = Vector2DP acc angleOtherToMe
       dampAccVec = v .* (2 * (sqrt $ k * m) * (-damping)) ./ m
       nv = v .+ (((pr2 accVec) .+ dampAccVec) .* timeStep)
    in put $ PointMass p nv m
