module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data GameAttribute = Score Int
data GameState = Level Int | GameInit Int
type SokobanObject = GameObject ()


width = 500
height = 500
w = 500.0
h = 500.0
type SokobanAction a = IOGame GameAttribute () GameState TileAttribute a
data TileAttribute = NoTileAttribute
type SokobanTile = Tile TileAttribute
type SokobanMap = TileMatrix TileAttribute

magenta :: InvList
magenta = Just [(255,0,255)]

bmpList :: FilePictureList
bmpList = [("tile.bmp", Nothing),
           ("box.bmp", magenta),
           ("wall.bmp", Nothing),
           ("greenSquare.bmp", Nothing),
           ("redSquare.bmp", Nothing)]

tileSize :: GLdouble
tileSize = width/10

t::SokobanTile
t = (0, False, 0.0, NoTileAttribute)

m::SokobanTile
m = (2, False, 0.0, NoTileAttribute)

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

map2 :: SokobanMap
map2 = [[m, t, t, t, t, t, t, t, t, m],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [m, t, t, t, t, t, t, t, t, m]]

map3 :: SokobanMap
map3 = [[m, t, t, t, t, t, t, t, t, m],
        [t, m, t, t, t, t, t, t, m, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, t, t, t, t, t, t, t, t, t],
        [t, m, t, t, t, t, t, t, m, t],
        [m, t, t, t, t, t, t, t, t, m]]

main :: IO ()
main = do
        let winConfig = ((0,0),(500,500),"Sokoban")
            gameMap = multiMap [(tileMap map1 tileSize tileSize),
                                (tileMap map2 tileSize tileSize),
                                (tileMap map3 tileSize tileSize)] 0
            guy = objectGroup "guyGroup"  [createGuy]
            box = objectGroup "boxGroup" [createBox, createBox2, createBox3]
            endPoint = objectGroup "endGroup" [createEndPoint, createEndPoint2, createEndPoint3]
            initScore = Score 0
            input = [(Char 'w', Press, walkUp),
                     (Char 'a', Press, walkLeft),
                     (Char 's', Press, walkDown),
                     (Char 'd', Press, walkRight),
                     (Char 'p', Press, \_ _ -> funExit)]
        funInit winConfig gameMap [guy,box,endPoint] (GameInit 1) initScore input gameCycle (Timer 40) bmpList

createGuy :: SokobanObject
createGuy = let guyPic = Basic (Circle 10.0 1.0 0.0 1.0 Filled)
             in object "guy" guyPic False (((w/2)-25),25) (0,0) ()

createBox :: SokobanObject
createBox = let boxPic = Tex (tileSize , tileSize) 1
            in object "box" boxPic False (((w/2)-25),((h/2)-25)) (0,0) ()

createBox2 :: SokobanObject
createBox2 = let boxPic = Tex (tileSize , tileSize) 1
             in object "box2" boxPic False (((w/2)-75),((h/2)-25)) (0,0) ()

createBox3 :: SokobanObject
createBox3 = let boxPic = Tex (tileSize , tileSize) 1
             in object "box3" boxPic False (((w/2)+25),((h/2)-25)) (0,0) ()

createEndPoint :: SokobanObject
createEndPoint = let endPointBound = [(-25,-25),(25,-25),(25,25),(-25,25)]
                     endPointPic = Basic (Polyg endPointBound 1.0 0.0 0.0 Unfilled)
                 in object "endPoint" endPointPic False (((w/2)-25),(((3*h)/4))) (0,0) ()

createEndPoint2 :: SokobanObject
createEndPoint2 = let endPointBound = [(-25,-25),(25,-25),(25,25),(-25,25)]
                      endPointPic = Basic (Polyg endPointBound 1.0 0.0 0.0 Unfilled)
                  in object "endPoint2" endPointPic False (((w/2)-75),(((3*h)/4))) (0,0) ()

createEndPoint3 :: SokobanObject
createEndPoint3 = let endPointBound = [(-25,-25),(25,-25),(25,25),(-25,25)]
                      endPointPic = Basic (Polyg endPointBound 1.0 0.0 0.0 Unfilled)
                  in object "endPoint3" endPointPic False (((w/2)+25),(((3*h)/4))) (0,0) ()

walkRight :: Modifiers -> Position -> SokobanAction ()
walkRight _ _ = do
 guy <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition guy
 box <- findObject "box" "boxGroup"
 (pXbox,pYbox) <- getObjectPosition box
 box2 <- findObject "box2" "boxGroup"
 (pXbox2,pYbox2) <- getObjectPosition box2
 box3 <- findObject "box3" "boxGroup"
 (pXbox3,pYbox3) <- getObjectPosition box3
 if ((pX + 50) == pXbox && pY==pYbox)
   then when ((pXbox + 75 <= w) && ((pXbox2 /= pXbox + 50) || pYbox2/=pYbox) && ((pXbox3 /= pXbox + 50) || pYbox3 /= pYbox)) (moveBoxRight box)
   else do if ((pX + 50) == pXbox2 && pY==pYbox2)
             then when ((pXbox2 + 75 <= w) && ((pXbox /= pXbox2 + 50) || pYbox/=pYbox2) && ((pXbox3 /= pXbox2 + 50) || pYbox3/=pYbox2)) (moveBoxRight box2)
             else do if ((pX + 50) == pXbox3 && pY==pYbox3)
                       then when ((pXbox3 + 75 <= w) && ((pXbox2 /= pXbox3 + 50) || pYbox2/=pYbox3) && ((pXbox /= pXbox3 + 50) || pYbox/=pYbox3)) (moveBoxRight box3)
                       else do if (pX + 75 <= w)
                                 then do (setObjectPosition ((pX + 50),pY) guy)
                                 else do (setObjectPosition ((w - 25),pY) guy)

walkLeft :: Modifiers -> Position -> SokobanAction ()
walkLeft _ _ = do
  guy <- findObject "guy" "guyGroup"
  (pX,pY) <- getObjectPosition guy
  box <- findObject "box" "boxGroup"
  (pXbox,pYbox) <- getObjectPosition box
  box2 <- findObject "box2" "boxGroup"
  (pXbox2,pYbox2) <- getObjectPosition box2
  box3 <- findObject "box3" "boxGroup"
  (pXbox3,pYbox3) <- getObjectPosition box3
  if ((pX - 50) == pXbox && pY==pYbox)
    then when ((pXbox - 75 >= 0) && ((pXbox2 /= pXbox - 50) || pYbox2/=pYbox) && ((pXbox3 /= pXbox - 50) || pYbox3 /= pYbox)) (moveBoxLeft box)
    else do if ((pX - 50) == pXbox2 && pY==pYbox2)
              then when ((pXbox2 - 75 >= 0) && ((pXbox /= pXbox2 - 50) || pYbox/=pYbox2) && ((pXbox3 /= pXbox2 - 50) || pYbox3/=pYbox2)) (moveBoxLeft box2)
              else do if ((pX - 50) == pXbox3 && pY==pYbox3)
                        then when ((pXbox3 - 75 >= 0) && ((pXbox2 /= pXbox3 - 50) || pYbox2/=pYbox3) && ((pXbox /= pXbox3 - 50) || pYbox/=pYbox3)) (moveBoxLeft box3)
                        else do if (pX - 75 >= 0)
                                  then do (setObjectPosition ((pX - 50),pY) guy)
                                  else do (setObjectPosition (25,pY) guy)

walkUp :: Modifiers -> Position -> SokobanAction ()
walkUp _ _ = do
 obj <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 box <- findObject "box" "boxGroup"
 (pXbox,pYbox) <- getObjectPosition box
 if (pX == pXbox && (pY + 50)==pYbox)
   then when (pYbox + 75 <= h) (moveBoxUp box)
   else do if (pY + 75 <= h)
             then do (setObjectPosition (pX,(pY + 50)) obj)
             else do (setObjectPosition (pX,(w - 25)) obj)

walkDown :: Modifiers -> Position -> SokobanAction ()
walkDown _ _ = do
 obj <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition obj
 box <- findObject "box" "boxGroup"
 (pXbox,pYbox) <- getObjectPosition box
 if (pX == pXbox && (pY - 50)==pYbox)
   then when (pYbox - 75 >= 0) (moveBoxDown box)
   else do if (pY - 75 >= 0)
             then do (setObjectPosition (pX,(pY - 50)) obj)
             else do (setObjectPosition (pX,25) obj)

moveBoxRight :: SokobanObject -> SokobanAction ()
moveBoxRight box = do
 --obj <- findObject "box" "boxGroup"
 obj2 <-findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition box
 (pX2,pY2) <- getObjectPosition obj2
 if (pX + 75 <= w)
   then do (setObjectPosition ((pX + 50),pY) box)
           (setObjectPosition ((pX2 + 50),pY2) obj2)
   else do (setObjectPosition ((w - 25),pY) box)
           (setObjectPosition ((w - 25),pY2) obj2)

moveBoxLeft :: SokobanObject -> SokobanAction ()
moveBoxLeft box = do
 --obj <- findObject "box" "boxGroup"
 obj2 <-findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition box
 (pX2,pY2) <- getObjectPosition obj2
 if (pX - 75 >= 0)
   then do (setObjectPosition ((pX - 50),pY) box)
           (setObjectPosition ((pX2 - 50),pY2) obj2)
   else do (setObjectPosition (25,pY) box)
           (setObjectPosition (25,pY2) obj2)

moveBoxUp :: SokobanObject -> SokobanAction ()
moveBoxUp box = do
 --obj <- findObject "box" "boxGroup"
 obj2 <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition box
 (pX2,pY2) <- getObjectPosition obj2
 if (pY + 75 <= h)
   then do (setObjectPosition (pX,(pY + 50)) box)
           (setObjectPosition (pX2,(pY2 + 50)) obj2)
   else do (setObjectPosition (pX,(w - 25)) box)
           (setObjectPosition (pX2,(w - 25)) obj2)

moveBoxDown :: SokobanObject -> SokobanAction ()
moveBoxDown box = do
 --obj <- findObject "box" "boxGroup"
 obj2 <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition box
 (pX2,pY2) <- getObjectPosition obj2
 if (pY - 75 >= 0)
   then do (setObjectPosition (pX,(pY - 50)) box)
           (setObjectPosition (pX2,(pY2 - 50)) obj2)
   else do (setObjectPosition (pX,25) box)
           (setObjectPosition (pX2,25) obj2)

gameCycle :: SokobanAction ()
gameCycle = do
 (Score n) <- getGameAttribute
 state <- getGameState
 case state of
   (GameInit levelInit) -> do
    setGameState (Level levelInit)
    printOnScreen (show ("Nivel ") ++ show levelInit) TimesRoman24 (0,0) 1.0 1.0 1.0
   (Level level) -> do
    case level of
      1 -> levelOne (Score n)
      2 -> levelTwo (Score n)
      3 -> levelThree (Score n)

 --Rafael mandou usar setObjectCurrentPicture pra mudar a textura de um obj

levelOne :: GameAttribute -> SokobanAction()
levelOne (Score n) = do
 setCurrentMapIndex 0
 guy <- findObject "guy" "guyGroup"
 box <- findObject "box" "boxGroup"
 box2 <- findObject "box2" "boxGroup"
 box3 <- findObject "box3" "boxGroup"
 endPoint <- findObject "endPoint" "endGroup"
 endPoint2 <- findObject "endPoint2" "endGroup"
 endPoint3 <- findObject "endPoint3" "endGroup"
 col11 <- objectsCollision box endPoint
 col12 <- objectsCollision box endPoint2
 col13 <- objectsCollision box endPoint3
 col21 <- objectsCollision box2 endPoint
 col22 <- objectsCollision box2 endPoint2
 col23 <- objectsCollision box2 endPoint3
 col31 <- objectsCollision box3 endPoint
 col32 <- objectsCollision box3 endPoint2
 col33 <- objectsCollision box3 endPoint3
 when (col11 && col22 && col33) (setGameState (GameInit 2))
 when (col11 && col23 && col32) (setGameState (GameInit 2))
 when (col12 && col21 && col33) (setGameState (GameInit 2))
 when (col12 && col23 && col31) (setGameState (GameInit 2))
 when (col13 && col21 && col32) (setGameState (GameInit 2))
 when (col13 && col22 && col31) (setGameState (GameInit 2))

levelTwo :: GameAttribute -> SokobanAction()
levelTwo (Score n) = do
 setCurrentMapIndex 1
 guy <- findObject "guy" "guyGroup"
 box <- findObject "box" "boxGroup"
 box2 <- findObject "box2" "boxGroup"
 box3 <- findObject "box3" "boxGroup"
 endPoint <- findObject "endPoint" "endGroup"
 endPoint2 <- findObject "endPoint2" "endGroup"
 endPoint3 <- findObject "endPoint3" "endGroup"
 col11 <- objectsCollision box endPoint
 col12 <- objectsCollision box endPoint2
 col13 <- objectsCollision box endPoint3
 col21 <- objectsCollision box2 endPoint
 col22 <- objectsCollision box2 endPoint2
 col23 <- objectsCollision box2 endPoint3
 col31 <- objectsCollision box3 endPoint
 col32 <- objectsCollision box3 endPoint2
 col33 <- objectsCollision box3 endPoint3
 when (col11 && col22 && col33) (setGameState (GameInit 3))
 when (col11 && col23 && col32) (setGameState (GameInit 3))
 when (col12 && col21 && col33) (setGameState (GameInit 3))
 when (col12 && col23 && col31) (setGameState (GameInit 3))
 when (col13 && col21 && col32) (setGameState (GameInit 3))
 when (col13 && col22 && col31) (setGameState (GameInit 3))

levelThree :: GameAttribute -> SokobanAction()
levelThree (Score n) = do
 setCurrentMapIndex 2
 guy <- findObject "guy" "guyGroup"
 box <- findObject "box" "boxGroup"
 box2 <- findObject "box2" "boxGroup"
 box3 <- findObject "box3" "boxGroup"
 endPoint <- findObject "endPoint" "endGroup"
 endPoint2 <- findObject "endPoint2" "endGroup"
 endPoint3 <- findObject "endPoint3" "endGroup"
 col11 <- objectsCollision box endPoint
 col12 <- objectsCollision box endPoint2
 col13 <- objectsCollision box endPoint3
 col21 <- objectsCollision box2 endPoint
 col22 <- objectsCollision box2 endPoint2
 col23 <- objectsCollision box2 endPoint3
 col31 <- objectsCollision box3 endPoint
 col32 <- objectsCollision box3 endPoint2
 col33 <- objectsCollision box3 endPoint3
 when (col11 && col22 && col33) (printOnScreen (show ("Parabens")) TimesRoman24 (width/2,0) 1.0 1.0 1.0)
 when (col11 && col23 && col32) (printOnScreen (show ("Parabens")) TimesRoman24 (width/2,0) 1.0 1.0 1.0)
 when (col12 && col21 && col33) (printOnScreen (show ("Parabens")) TimesRoman24 (width/2,0) 1.0 1.0 1.0)
 when (col12 && col23 && col31) (printOnScreen (show ("Parabens")) TimesRoman24 (width/2,0) 1.0 1.0 1.0)
 when (col13 && col21 && col32) (printOnScreen (show ("Parabens")) TimesRoman24 (width/2,0) 1.0 1.0 1.0)
 when (col13 && col22 && col31) (printOnScreen (show ("Parabens")) TimesRoman24 (width/2,0) 1.0 1.0 1.0)
