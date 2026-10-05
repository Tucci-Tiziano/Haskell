import Data.IORef

jose :: IO (IORef Player)
jose = newIORef ((160, 160), 0, 0)






type Coords = (Double, Double)
type Angle = Double
type Fov = Double
type Player = (Coords, Angle, Fov)

mapa = [ 
    [1,1,1,1,1],
    [1,0,0,0,1],
    [1,0,0,0,1],
    [1,0,1,0,1],
    [1,1,1,1,1]]

    
width :: Double
width = 800

height :: Double
height = 960 

cellWidth :: Double
cellWidth = 160

cellHeight :: Double
cellHeight = 160


avanzarX :: IORef Player -> IO ()
avanzarX player = modifyIORef player (\((x,y),ang,fov) -> ((x + 10,y),ang,fov))


avanzarY :: IORef Player -> IO ()
avanzarY player = modifyIORef player (\((x,y),ang,fov) -> ((x ,y + 10),ang,fov))

retrocederX :: IORef Player -> IO ()
retrocederX player = modifyIORef player (\((x,y),ang,fov) -> ((x - 10,y),ang,fov))

retrocederY :: IORef Player -> IO ()
retrocederY player = modifyIORef player (\((x,y),ang,fov) -> ((x,y-10),ang,fov))

girarRelog :: IORef Player -> IO ()
girarRelog player = modifyIORef player (\((x,y),ang,fov) -> ((x,y),(ang-(pi/10)),fov))


girarContraRelog :: IORef Player -> IO ()
girarContraRelog player = modifyIORef player (\((x,y),ang,fov) -> ((x,y),(ang+(pi/10)),fov))

donde :: IORef Player -> IO Player
donde player = do
    playerA <- readIORef player
    return playerA



degToRad :: Double -> Double
degToRad deg = deg * pi / 180


distance :: IORef Player -> Coords -> IO Double
distance playerRef (x2, y2) = do
    ((x1, y1), _, _) <- readIORef playerRef
    return $ sqrt (((x1 - x2) * (x1 - x2)) + ((y1 - y2) * (y1 - y2)))

altura :: Double -> Double
altura a = height - ((height/10)*a)

-- Vertical
vys :: Double
vys = cellHeight

vxs :: IORef Player -> IO Double
vxs playerRef = do
    ((_, _), ang, _) <- readIORef playerRef
    return $ cellWidth * tan ang

vyn :: IORef Player -> IO Double
vyn playerRef = do
    ((_, y), ang, _) <- readIORef playerRef
    return $
        if sin ang > 0
            then fromIntegral (floor (y / cellHeight) + 1) * cellHeight
            else fromIntegral (ceiling (y / cellHeight) - 1) * cellHeight


vxn :: IORef Player -> Double -> IO Double
vxn playerRef yn = do
    ((x, y), ang, _) <- readIORef playerRef
    return $ x + (yn - y) / tan ang

-- Horizontal
hxs :: Double 
hxs = cellWidth

hys :: IORef Player -> IO Double
hys playerRef = do
    ((_, _), ang, _) <- readIORef playerRef
    return $ cellHeight / tan ang


hxn :: IORef Player -> IO Double
hxn playerRef = do
    ((x, _), ang, _) <- readIORef playerRef
    return $
        if cos ang > 0
            then fromIntegral (floor (x / cellWidth) + 1) * cellWidth
            else fromIntegral (ceiling (x / cellWidth) - 1) * cellWidth


hyn :: IORef Player -> Double -> IO Double
hyn playerRef xn = do
    ((x, y), ang, _) <- readIORef playerRef
    return $ y + (xn - x) * tan ang


avanzar :: Coords -> Double -> Coords
avanzar (x,y) ang = ((x+(cos ang)), (y+(sin ang)))

isWall :: Coords -> Bool
isWall (x,y) = (( mapa !! (floor(y / cellHeight) ) ) !! (floor(x / cellWidth) )) == 1

ray :: Coords -> Double -> Coords
ray (x,y) ang 
        | (isWall (x,y)) == False = ray (avanzar (x,y) ang) ang
        | otherwise = (x,y)

rayCast :: IORef Player -> IO Double
rayCast playerRef = do
    player@((x, y), ang, _) <- readIORef playerRef
    distanceResult <- distance playerRef (ray (x, y) ang)
    return distanceResult




--joseRef <- jose