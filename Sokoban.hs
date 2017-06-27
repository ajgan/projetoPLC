module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data GameAttribute = Score Int

width = 500
height = 500
w = fromIntegral width :: GLdouble
h = fromIntegral height :: GLdouble

type PongAction a = IOGame GameAttribute () () () a

main :: IO ()
main = do
        let winConfig = ((0,0),(width,height),"Sokoban")
            bmpList = [("floor.bmp", Nothing)]
            gameMap = textureMap 0 50 50 w h
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
createBox = let boxBound = [(-25,-25),(25,-25),(25,25),(-25,25)]
                boxPic = Basic (Polyg boxBound 1.0 1.0 0.0 Filled)
            in object "box" boxPic False (((w/2)-25),((h/2)-25)) (0,0) ()

createEndPoint :: GameObject ()
createEndPoint = let endPointBound = [(-26,-26),(26,-26),(26,26),(-26,26)]
                     endPointPic = Basic (Polyg endPointBound 1.0 0.0 0.0 Unfilled)
                 in object "endPoint" endPointPic False (((w/2)-25),(((3*h)/4))) (0,0) ()

walkRight :: Modifiers -> Position -> IOGame GameAttribute () () () ()
walkRight _ _ = do
 obj <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 obj2 <- findObject "box" "boxGroup"
 (p2X,p2Y) <- getObjectPosition obj2
 if ((pX + 75) == p2X && pY==p2Y)
   then when (p2X + 75 <= w) moveBoxRight
   else do if (pX + 75 <= w)
             then do (setObjectPosition ((pX + 50),pY) obj)
             else do (setObjectPosition ((w - 25),pY) obj)


walkLeft :: Modifiers -> Position -> IOGame GameAttribute () () () ()
walkLeft _ _ = do
 obj <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 obj2 <- findObject "box" "boxGroup"
 (p2X,p2Y) <- getObjectPosition obj2
 if ((pX - 75) == p2X && pY==p2Y)
   then when (p2X - 75 >= w) moveBoxLeft
   else do if (pX - 75 >= 0)
             then do (setObjectPosition ((pX - 50),pY) obj)
             else do (setObjectPosition (25,pY) obj)

walkUp :: Modifiers -> Position -> IOGame GameAttribute () () () ()
walkUp _ _ = do
 obj <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 obj2 <- findObject "box" "boxGroup"
 (p2X,p2Y) <- getObjectPosition obj2
 if (pX == p2X && (pY + 75)==p2Y)
   then when (p2Y + 75 <= h) moveBoxUp
   else do if (pY + 75 <= h)
             then do (setObjectPosition (pX,(pY + 50)) obj)
             else do (setObjectPosition (pX,(w - 25)) obj)

walkDown :: Modifiers -> Position -> IOGame GameAttribute () () () ()
walkDown _ _ = do
 obj <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 obj2 <- findObject "box" "boxGroup"
 (p2X,p2Y) <- getObjectPosition obj2
 if (pX == p2X && (pY - 75)==p2Y)
   then when (p2Y - 75 >= 0) moveBoxDown
   else do if (pY - 75 >= 0)
             then do (setObjectPosition (pX,(pY - 50)) obj)
             else do (setObjectPosition (pX,25) obj)

moveBoxRight :: IOGame GameAttribute () () () ()
moveBoxRight = do
 obj <- findObject "box" "boxGroup"
 (pX,pY) <- getObjectPosition obj
 if (pX + 75 <= w)
   then do (setObjectPosition ((pX + 50),pY) obj)
   else do (setObjectPosition ((w - 25),pY) obj)

moveBoxLeft :: IOGame GameAttribute () () () ()
moveBoxLeft = do
 obj <- findObject "box" "boxGroup"
 (pX,pY) <- getObjectPosition obj
 if (pX - 75 >= 0)
   then do (setObjectPosition ((pX - 50),pY) obj)
   else do (setObjectPosition (25,pY) obj)

moveBoxUp :: IOGame GameAttribute () () () ()
moveBoxUp = do
 obj <- findObject "box" "boxGroup"
 (pX,pY) <- getObjectPosition obj
 if (pY + 75 <= h)
   then do (setObjectPosition (pX,(pY + 50)) obj)
   else do (setObjectPosition (pX,(w - 25)) obj)

moveBoxDown :: IOGame GameAttribute () () () ()
moveBoxDown = do
 obj <- findObject "box" "boxGroup"
 (pX,pY) <- getObjectPosition obj
 if (pY - 75 >= 0)
   then do (setObjectPosition (pX,(pY - 50)) obj)
   else do (setObjectPosition (pX,25) obj)

gameCycle :: IOGame GameAttribute () () () ()
gameCycle = do
 (Score n) <- getGameAttribute
 printOnScreen (show n) TimesRoman24 (0,0) 1.0 1.0 1.0
 guy <- findObject "guy" "guyGroup"
 box <- findObject "box" "boxGroup"
 endPoint <- findObject "endPoint" "endGroup"
 col <- objectsCollision box endPoint
 when col (setGameAttribute (Score (n + 10)))
 --Rafael mandou usar setObjectCurrentPicture pra mudar a textura de um obj
