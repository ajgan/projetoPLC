module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data GameAttribute = Score Int

width = 500
height = 500
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble
type SokobanAction a = IOGame GameAttribute () () TileAttribute a
data TileAttribute = NoTileAttribute
type SokobanTile = Tile TileAttribute
type SokobanMap = TileMatrix TileAttribute

magenta :: InvList
magenta = Just [(255,0,255)]

bmpList :: FilePictureList
bmpList = [("tile.bmp", Nothing),
           ("box.bmp", magenta)]

tileSize :: Double
tileSize = 50.0

t::SokobanTile
t = (0, False, 0.0, NoTileAttribute)

map1 :: SokobanMap
map1 = [[t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t]]

main :: IO ()
main = do
        let winConfig = ((0,0),(width,height),"Sokoban")
            gameMap = tileMap map1 tileSize tileSize
            guy    = objectGroup "guyGroup"  [createGuy]
            box   = objectGroup "boxGroup" [createBox]
            endPoint = objectGroup "endGroup" [createEndPoint]
            initScore = Score 0
            input = [(Char 'w', Press, walkUp),
                     (Char 'a', Press, walkLeft),
                     (Char 's', Press, walkDown),
                     (Char 'd', Press, walkRight),
                     (Char 'p', Press, \_ _ -> funExit)]
        funInit winConfig gameMap [guy,box,endPoint] () initScore input gameCycle (Timer 40) bmpList

createGuy :: GameObject ()
createGuy = let guyPic = Basic (Circle 10.0 1.0 0.0 1.0 Filled)
             in object "guy" guyPic False (((w/2)-25),25) (0,0) ()

createBox :: GameObject ()
createBox = let boxPic = Tex (tileSize , tileSize) 1
            in object "box" boxPic False (((w/2)-25),((h/2)-25)) (0,0) ()

createEndPoint :: GameObject ()
createEndPoint = let endPointBound = [(-25,-25),(25,-25),(25,25),(-25,25)]
                     endPointPic = Basic (Polyg endPointBound 1.0 0.0 0.0 Unfilled)
                 in object "endPoint" endPointPic False (((w/2)-25),(((3*h)/4))) (0,0) ()

walkRight :: Modifiers -> Position -> SokobanAction ()
walkRight _ _ = do
 obj <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 obj2 <- findObject "box" "boxGroup"
 (p2X,p2Y) <- getObjectPosition obj2
 if ((pX + 50) == p2X && pY==p2Y)
   then when (p2X + 75 <= w) moveBoxRight
   else do if (pX + 75 <= w)
             then do (setObjectPosition ((pX + 50),pY) obj)
             else do (setObjectPosition ((w - 25),pY) obj)


walkLeft :: Modifiers -> Position -> SokobanAction ()
walkLeft _ _ = do
 obj <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 obj2 <- findObject "box" "boxGroup"
 (p2X,p2Y) <- getObjectPosition obj2
 if ((pX - 50) == p2X && pY==p2Y)
   then when (p2X - 75 >= 0) moveBoxLeft
   else do if (pX - 75 >= 0)
             then do (setObjectPosition ((pX - 50),pY) obj)
             else do (setObjectPosition (25,pY) obj)

walkUp :: Modifiers -> Position -> SokobanAction ()
walkUp _ _ = do
 obj <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 obj2 <- findObject "box" "boxGroup"
 (p2X,p2Y) <- getObjectPosition obj2
 if (pX == p2X && (pY + 50)==p2Y)
   then when (p2Y + 75 <= h) moveBoxUp
   else do if (pY + 75 <= h)
             then do (setObjectPosition (pX,(pY + 50)) obj)
             else do (setObjectPosition (pX,(w - 25)) obj)

walkDown :: Modifiers -> Position -> SokobanAction ()
walkDown _ _ = do
 obj <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 obj2 <- findObject "box" "boxGroup"
 (p2X,p2Y) <- getObjectPosition obj2
 if (pX == p2X && (pY - 50)==p2Y)
   then when (p2Y - 75 >= 0) moveBoxDown
   else do if (pY - 75 >= 0)
             then do (setObjectPosition (pX,(pY - 50)) obj)
             else do (setObjectPosition (pX,25) obj)

moveBoxRight :: SokobanAction ()
moveBoxRight = do
 obj <- findObject "box" "boxGroup"
 obj2 <-findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 (pX2,pY2) <- getObjectPosition obj2
 if (pX + 75 <= w)
   then do (setObjectPosition ((pX + 50),pY) obj)
           (setObjectPosition ((pX2 + 50),pY2) obj2)
   else do (setObjectPosition ((w - 25),pY) obj)
           (setObjectPosition ((w - 25),pY2) obj2)

moveBoxLeft :: SokobanAction ()
moveBoxLeft = do
 obj <- findObject "box" "boxGroup"
 obj2 <-findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 (pX2,pY2) <- getObjectPosition obj2
 if (pX - 75 >= 0)
   then do (setObjectPosition ((pX - 50),pY) obj)
           (setObjectPosition ((pX2 - 50),pY2) obj2)
   else do (setObjectPosition (25,pY) obj)
           (setObjectPosition (25,pY2) obj2)

moveBoxUp :: SokobanAction ()
moveBoxUp = do
 obj <- findObject "box" "boxGroup"
 obj2 <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 (pX2,pY2) <- getObjectPosition obj2
 if (pY + 75 <= h)
   then do (setObjectPosition (pX,(pY + 50)) obj)
           (setObjectPosition (pX2,(pY2 + 50)) obj2)
   else do (setObjectPosition (pX,(w - 25)) obj)
           (setObjectPosition (pX2,(w - 25)) obj2)

moveBoxDown :: SokobanAction ()
moveBoxDown = do
 obj <- findObject "box" "boxGroup"
 obj2 <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 (pX2,pY2) <- getObjectPosition obj2
 if (pY - 75 >= 0)
   then do (setObjectPosition (pX,(pY - 50)) obj)
           (setObjectPosition (pX2,(pY2 - 50)) obj2)
   else do (setObjectPosition (pX,25) obj)
           (setObjectPosition (pX2,25) obj2)

gameCycle :: SokobanAction ()
gameCycle = do
 (Score n) <- getGameAttribute
 printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0
 guy <- findObject "guy" "guyGroup"
 box <- findObject "box" "boxGroup"
 endPoint <- findObject "endPoint" "endGroup"
 (pX,pY) <- getObjectPosition guy
 printOnScreen (show pX) TimesRoman24 (0,100) 1.0 1.0 1.0
 printOnScreen (show pY) TimesRoman24 (0,150) 1.0 1.0 1.0
 (pX2,pY2) <- getObjectPosition box
 printOnScreen (show pX2) TimesRoman24 (0,200) 1.0 1.0 1.0
 printOnScreen (show pY2) TimesRoman24 (0,250) 1.0 1.0 1.0
 col <- objectsCollision guy box
 col2 <- objectsCollision box endPoint
 when col2 (setGameAttribute (Score (n + 10)))

 --Rafael mandou usar setObjectCurrentPicture pra mudar a textura de um obj
