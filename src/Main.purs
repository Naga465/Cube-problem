module Main where


import Math ( pi, sin, cos)
import Control.Monad.Eff (Eff)
import Partial.Unsafe(unsafePartial)
import Data.Maybe (Maybe(..))
import FFI.Util (property, setProperty)
import Prelude
import Graphics.Canvas
import  Signal.Time(every,millisecond)
import Signal


foreign import addEventListener :: forall a. CanvasElement -> String
                                    -> (Event -> Eff (canvas :: CANVAS | a) Unit)
                                    -> Eff (canvas :: CANVAS | a) Unit

type Time = Number
type Point = {x:: Number, y :: Number}

newtype Point2D = Point2D { x:: Number,   y:: Number }
newtype Point3D = Point3D { x:: Number,   y:: Number,   z:: Number }
newtype Angle3D = Angle3D { ax:: Number,  ay:: Number,  az::Number }
newtype Cube    =  Cube   { size::Number, color::String }

--projecting the slope

origin :: Point
origin = { x:400.0, y:400.0} 

rotaX :: Point3D -> Number -> Point3D
rotaX (Point3D p) angle = do
    let sinA = sin(angle)
    let cosA = cos(angle)
    let rty = p.y * cosA - p.z * sinA
    let rtz = p.y * sinA + p.z * cosA
    Point3D  { x: p.x, y: rty, z:rtz
    }
rotaY :: Point3D -> Number -> Point3D
rotaY (Point3D p) angle = do
    let sinA = sin(angle)
    let cosA = cos(angle)
    let rtx = p.x * cosA + p.z * sinA
    let rtz = -p.x * sinA + p.z * cosA
    Point3D  { x: rtx, y: p.y, z:rtz
    }


project :: Point3D -> Point2D
project (Point3D p) =  

    let 
        x'=(p.x / z') * origin.x  + origin.x
        y'=(-p.y / z') * origin.y + origin.y 
        z'= p.z + 4.0
    in  
        Point2D { x:x', y:y'}

rad :: Number -> Number
rad ang = ang * pi /180.0

rotate :: Point3D -> Number -> Number  -> Point3D
-- rotate (Point3D vec) rx ry rz = rotaZ (rotaY (rotaX (Point3D vec) (rad rx)) (rad ry)) (rad rz)
rotate (Point3D vec) rx ry = rotaY (rotaX (Point3D vec) (rad rx)) (rad ry)


path :: forall e. Context2D -> String -> (Context2D -> Eff (canvas :: CANVAS | e) Context2D) ->  Eff (canvas :: CANVAS | e) Context2D
path ctx color draw = withContext ctx $ do
  ctx <- setStrokeStyle color ctx
  ctx <- beginPath ctx
  ctx <- draw ctx
  stroke ctx

-- --Drawing a
linedraw :: forall eff. Context2D -> Point2D -> Point2D -> Eff(canvas::CANVAS |eff) Context2D
linedraw ctx (Point2D f) (Point2D t) = do
    ctx <- moveTo ctx f.x f.y
    lineTo ctx t.x t.y


dcube :: forall eff. Context2D -> Cube -> Angle3D -> Eff(canvas :: CANVAS | eff) Context2D
dcube ctx (Cube { size, color }) (Angle3D a) = do
    let v1 = project $ rotate (Point3D { x:  -size, y:  -size, z:  -size }) a.ax a.ay 
    let v2 = project $ rotate (Point3D { x:  -size, y:  size, z:  -size }) a.ax a.ay 
    let v3 = project $ rotate (Point3D { x:  -size, y:  -size, z:  size }) a.ax a.ay 
    let v4 = project $ rotate (Point3D { x:  -size, y:  size, z:  size }) a.ax a.ay 
    let v5 = project $ rotate (Point3D { x:  size, y:  -size, z:  -size }) a.ax a.ay 
    let v6 = project $ rotate (Point3D { x:  size, y:  size, z:  -size }) a.ax a.ay 
    let v7 = project $ rotate (Point3D { x:  size, y:  -size, z:  size }) a.ax a.ay 
    let v8 = project $ rotate (Point3D { x:  size, y:  size, z:  size }) a.ax a.ay 
    
    path ctx color \ctx -> do 
     ctx <- linedraw ctx v1 v5
     ctx <- linedraw ctx v5 v6
     ctx <- linedraw ctx v6 v2
     ctx <- linedraw ctx v2 v1
        
     ctx <- linedraw ctx v3 v7
     ctx <- linedraw ctx v7 v8
     ctx <- linedraw ctx v8 v4
     ctx <- linedraw ctx v4 v3
        
     ctx <- linedraw ctx v1 v3
     ctx <- linedraw ctx v5 v7
     ctx <- linedraw ctx v6 v8
     linedraw ctx v2 v4

deltaMove :: Point2D
deltaMove = Point2D { x: 0.0, y: 0.0}

previous :: Point2D
previous = Point2D { x: 0.0, y: 0.0}

rotationscale :: Point3D
rotationscale = Point3D { x: 0.8, y: 1.1, z: 0.8}

drag :: Array Boolean
drag = [ false ]

speedsensitivity :: Array Number
speedsensitivity = [ 0.0 ]

foreign import data Event :: Type
onMouseDown :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseDown evt = void $ do
    _ <- pure $ setProperty drag "0" true
    pure unit
onMouseUp :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseUp evt = void $ do
    _<- pure $ setProperty drag "0" false
    pure unit

onMouseMove :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseMove evt = void $ do
    let (offSetX :: Number) = property evt "offsetX"
    let (offSetY :: Number) = property evt "offsetY"
    _ <- if(property drag "0" ) then do
        let (prevMouseX :: Number) = property previous "x"
        let (prevMouseY :: Number) = property previous "y"
        _ <- pure $ setProperty deltaMove "x" (offSetX - prevMouseX)
        _ <- pure $ setProperty deltaMove "y" (offSetY - prevMouseY)
        let accel = (property speedsensitivity "0") + 3.0
        pure $ setProperty speedsensitivity "0" accel
        pure unit
        else do
                pure unit
    _ <- pure $ setProperty previous "x" offSetX
    _ <- pure $ setProperty previous "y" offSetY
    pure unit

foreign import state :: forall e. Context2D
                -> (Context2D -> Time -> Eff (canvas :: CANVAS | e) Unit)
                -> Eff (canvas :: CANVAS | e) Unit


main = void $ unsafePartial do
    Just canvas <- getCanvasElementById "thecanvas"
    ctx <- getContext2D canvas
    ctx <- setFillStyle "#0000FF" ctx
    addEventListener canvas "mouseup" onMouseUp
    addEventListener canvas "mousedown" onMouseDown
    addEventListener canvas "mousemove" onMouseMove
    state ctx updateCube 
    pure unit

-- init  :: Cube
-- init = { cube : (Cube { size : 1.0 ,color : "#0000FF"})}

-- toDir :: Boolean -> Boolean -> Boolean -> Boolean -> Direction 
-- toDir true _ _ _ = Left 
-- toDir _ true _ _ = Right 
-- toDir _ _ true _ = Up 
-- toDir _ _ _ true = Down 
-- toDir _ _ _ _   = None 

-- input :: forall eff. Eff (dom :: DOM | eff) (Signal Direction) 
-- input = do 
--   leftInput  <- keyPressed 37
--   rightInput <- keyPressed 39 
--   upInput    <- keyPressed 38 
--   downInput  <- keyPressed 40 
  --   let inputSignal = map4 toDir leftInput rightInput upInput downInput 
  -- let timeSignal  = every millisecond 
  -- pure $ sampleOn timeSignal inputSignal 

updateCube :: forall a. Context2D -> Time -> Eff (canvas :: CANVAS | a) Unit
updateCube ctx time = void $ do
    _ <- clearRect ctx { x:0.0 ,y :0.0, h:800.0, w:800.0} 
    let time = every millisecond 
    let cube = Cube { size:  0.8, color: "#0000FF" }
    let (accel :: Number) = property speedsensitivity "0"
    _ <- if (accel > 0.0) then do
            _ <- pure $ setProperty speedsensitivity "0" (accel - 1.0)
            let (delX :: Number) = property deltaMove "x"
            let (delY :: Number) = property deltaMove "y"
            _ <- pure $ setProperty rotationscale "x" ((property rotationscale "x") - delY)
            _ <- pure $ setProperty rotationscale "y" ((property rotationscale "y") - delX)
            pure unit

            else do
                pure unit

    let an = Angle3D { ax: property rotationscale  "x", ay: property rotationscale  "y", az:0.0}
    _ <- dcube ctx cube an

    pure unit
