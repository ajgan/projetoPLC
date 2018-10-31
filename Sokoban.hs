module Main where

import Graphics.UI.Fungen
import Graphics.Rendering.OpenGL (GLdouble)

data GameAttribute = Score Int
data GameState = Level Int | GameInit Int
type SokobanObject = GameObject ()

pontuation = 0
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
           ("box.bmp", Nothing),
           ("wall.bmp", Nothing),
           ("greenBox.bmp", Nothing),
           ("blackX.bmp", magenta),
           ("GuyR.bmp", magenta),
           ("GuyL.bmp", magenta),
           ("GuyU.bmp", magenta),
           ("GuyD.bmp", magenta)]

tileSize :: GLdouble
tileSize = width/10

t::SokobanTile
t = (0, False, 0.0, NoTileAttribute)

m::SokobanTile
m = (2, True, 0.0, NoTileAttribute)

map1 :: SokobanMap
map1 = [[m, m, m, m, m, m, m, m, m, m],
        [m, t, t, t, t, t, t, t, m, m],
        [m, t, t, t, t, t, t, t, t, m],
        [m, t, t, m, t, t, t, t, t, m],
        [m, t, t, m, m, t, t, t, t, m],
        [m, t, t, t, m, m, t, t, t, m],
        [m, t, t, t, t, m, m, t, t, m],
        [m, t, t, t, t, t, t, t, t, m],
        [m, m, t, t, t, t, t, t, t, m],
        [m, m, m, m, m, m, m, m, m, m]]

map2 :: SokobanMap
map2 = [[m, m, m, m, m, m, m, m, m, m],
        [m, m, m, m, m, m, m, m, m, m],
        [m, m, m, m, m, m, m, m, m, m],
        [m, m, m, t, m, t, t, m, m, m],
        [m, m, m, t, t, t, t, m, m, m],
        [m, m, m, t, t, t, t, m, m, m],
        [m, m, m, m, t, t, t, m, m, m],
        [m, m, m, m, m, m, m, m, m, m],
        [m, m, m, m, m, m, m, m, m, m],
        [m, m, m, m, m, m, m, m, m, m]]

map3 :: SokobanMap
map3 = [[m, m, m, m, m, m, m, m, m, m],
        [m, m, m, m, m, m, m, m, m, m],
        [m, t, t, t, t, m, m, m, t, m],
        [m, t, t, m, t, m, t, t, t, m],
        [m, t, t, m, t, m, t, t, t, m],
        [m, t, t, t, t, t, t, t, m, m],
        [m, t, t, t, m, m, t, t, m, m],
        [m, m, m, m, m, m, t, t, m, m],
        [m, m, m, m, m, m, m, m, m, m],
        [m, m, m, m, m, m, m, m, m, m]]

main :: IO ()
main = do
        let winConfig = ((0,0),(500,500),"Sokoban")
            gameMap = multiMap [(tileMap map1 tileSize tileSize),
                                (tileMap map2 tileSize tileSize),
                                (tileMap map3 tileSize tileSize)] 0
            endPoint = objectGroup "endGroup" [createEndPoint, createEndPoint2, createEndPoint3]
            box = objectGroup "boxGroup" [createBox, createBox2, createBox3]
            guy = objectGroup "guyGroup"  [createGuy]
            initScore = Score 0
            input = [(Char 'w', Press, walkUp),
                     (Char 'a', Press, walkLeft),
                     (Char 's', Press, walkDown),
                     (Char 'd', Press, walkRight),
                     (Char 'p', Press, \_ _ -> funExit),
                     (Char 'r', Press, restartGame)]
        funInit winConfig gameMap [endPoint,box,guy] (GameInit 0) initScore input gameCycle (Timer 40) bmpList

restartGame ::  Modifiers -> Position -> SokobanAction()
restartGame _ _ = do
  lvl <- getGameState
  setGameState (GameInit 0)

createEndPoint :: SokobanObject
createEndPoint = let endPointPic = Tex (tileSize , tileSize) 4
                 in object "endPoint" endPointPic False (((w/2)-25),(((3*h)/4))) (0,0) ()

createEndPoint2 :: SokobanObject
createEndPoint2 = let endPointPic = Tex (tileSize , tileSize) 4
                  in object "endPoint2" endPointPic False (((w/2)-75),(((3*h)/4))) (0,0) ()

createEndPoint3 :: SokobanObject
createEndPoint3 = let endPointPic = Tex (tileSize , tileSize) 4
                  in object "endPoint3" endPointPic False (((w/2)+25),(((3*h)/4))) (0,0) ()

createBox :: SokobanObject
createBox = let boxPic = Tex (tileSize , tileSize) 1
            in object "box" boxPic False (((w/2)-25),((h/2)-25)) (0,0) ()

createBox2 :: SokobanObject
createBox2 = let boxPic = Tex (tileSize , tileSize) 1
             in object "box2" boxPic False (((w/2)-75),((h/2)-25)) (0,0) ()

createBox3 :: SokobanObject
createBox3 = let boxPic = Tex (tileSize , tileSize) 1
             in object "box3" boxPic False (((w/2)+25),((h/2)-25)) (0,0) ()

createGuy :: SokobanObject
createGuy = let guyPic = Tex (tileSize , tileSize) 7
            in object "guy" guyPic False (((w/2)-25),25) (0,0) ()

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
 setObjectCurrentPicture 5 guy
 if (pX+50>width)
   then (return())
   else do
           tile <- getTileFromWindowPosition (pX+50,pY)
           if (getTileBlocked tile)
             then return()
             else do if ((pX + 50) == pXbox && pY==pYbox)
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
  setObjectCurrentPicture 6 guy
  if (pX-50<0)
    then (return())
    else do
            tile <- getTileFromWindowPosition (pX-50,pY)
            if (getTileBlocked tile)
              then return()
              else do if ((pX - 50) == pXbox && pY==pYbox)
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
  guy <- findObject "guy" "guyGroup"
  (pX,pY) <- getObjectPosition guy
  box <- findObject "box" "boxGroup"
  (pXbox,pYbox) <- getObjectPosition box
  box2 <- findObject "box2" "boxGroup"
  (pXbox2,pYbox2) <- getObjectPosition box2
  box3 <- findObject "box3" "boxGroup"
  (pXbox3,pYbox3) <- getObjectPosition box3
  setObjectCurrentPicture 7 guy
  if (pY+50>height)
    then (return())
    else do
            tile <- getTileFromWindowPosition (pX,pY+50)
            if (getTileBlocked tile)
              then return()
              else do if (pX == pXbox && (pY+50)==pYbox)
                        then when ((pYbox + 75 <= h) && ((pXbox2 /= pXbox) || pYbox2/=pYbox +50) && ((pXbox3 /= pXbox) || pYbox3 /= pYbox +50)) (moveBoxUp box)
                        else do if (pX == pXbox2 && pY+50==pYbox2)
                                  then when ((pYbox2 + 50 <= h) && ((pXbox /= pXbox2) || pYbox/=pYbox2 + 50) && ((pXbox3 /= pXbox2) || pYbox3/=pYbox2 + 50)) (moveBoxUp box2)
                                  else do if (pX == pXbox3 && pY+50==pYbox3)
                                            then when ((pYbox3 + 50 <= h) && ((pXbox2 /= pXbox3) || pYbox2/=pYbox3+50) && ((pXbox /= pXbox3) || pYbox/=pYbox3+50)) (moveBoxUp box3)
                                            else do if (pY + 75 <= h)
                                                      then do (setObjectPosition (pX,(pY+50)) guy)
                                                      else do (setObjectPosition (pX,(h-25)) guy)

walkDown :: Modifiers -> Position -> SokobanAction ()
walkDown _ _ = do
  guy <- findObject "guy" "guyGroup"
  (pX,pY) <- getObjectPosition guy
  box <- findObject "box" "boxGroup"
  (pXbox,pYbox) <- getObjectPosition box
  box2 <- findObject "box2" "boxGroup"
  (pXbox2,pYbox2) <- getObjectPosition box2
  box3 <- findObject "box3" "boxGroup"
  (pXbox3,pYbox3) <- getObjectPosition box3
  setObjectCurrentPicture 8 guy
  if (pY-50<0)
    then (return())
    else do
            tile <- getTileFromWindowPosition (pX,pY-50)
            if (getTileBlocked tile)
              then return()
              else do if (pX == pXbox && (pY-50)==pYbox)
                        then when ((pYbox - 75 >= 0) && ((pXbox2 /= pXbox) || pYbox2/=pYbox -50) && ((pXbox3 /= pXbox) || pYbox3 /= pYbox -50)) (moveBoxDown box)
                        else do if (pX == pXbox2 && pY-50==pYbox2)
                                  then when ((pYbox2 + 50 <= h) && ((pXbox /= pXbox2) || pYbox/=pYbox2 - 50) && ((pXbox3 /= pXbox2) || pYbox3/=pYbox2 - 50)) (moveBoxDown box2)
                                  else do if (pX == pXbox3 && pY-50==pYbox3)
                                            then when ((pYbox3 - 50 <= h) && ((pXbox2 /= pXbox3) || pYbox2/=pYbox3-50) && ((pXbox /= pXbox3) || pYbox/=pYbox3-50)) (moveBoxDown box3)
                                            else do if (pY - 75 >= 0)
                                                      then do (setObjectPosition (pX,(pY-50)) guy)
                                                      else do (setObjectPosition (pX,25) guy)

moveBoxRight :: SokobanObject -> SokobanAction ()
moveBoxRight box = do
 guy <-findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition box
 (pX2,pY2) <- getObjectPosition guy
 if (pX+75>width)
   then (return())
   else do
           tile <- getTileFromWindowPosition (pX+50,pY)
           if (getTileBlocked tile)
             then return()
             else if (pX + 75 <= w)
                    then do (setObjectPosition ((pX + 50),pY) box)
                            (setObjectPosition ((pX2 + 50),pY2) guy)
                    else do (setObjectPosition ((w - 25),pY) box)
                            (setObjectPosition ((w - 25),pY2) guy)

moveBoxLeft :: SokobanObject -> SokobanAction ()
moveBoxLeft box = do
 guy <-findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition box
 (pX2,pY2) <- getObjectPosition guy
 if (pX - 75<0)
   then (return())
   else do
           tile <- getTileFromWindowPosition (pX-50,pY)
           if (getTileBlocked tile)
             then return()
             else if (pX - 75 >= 0)
                    then do (setObjectPosition ((pX - 50),pY) box)
                            (setObjectPosition ((pX2 - 50),pY2) guy)
                    else do (setObjectPosition (25,pY) box)
                            (setObjectPosition (25,pY2) guy)
-- gadelha eu te amo
--amo voce
--daria tudo, daria o mundo
moveBoxUp :: SokobanObject -> SokobanAction ()
moveBoxUp box = do
 guy <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition box
 (pX2,pY2) <- getObjectPosition guy
 if (pY+75>h)
   then (return())
   else do
           tile <- getTileFromWindowPosition (pX,pY+50)
           if (getTileBlocked tile)
             then return()
             else if (pY + 75 <= h)
                    then do (setObjectPosition (pX,(pY + 50)) box)
                            (setObjectPosition (pX2,(pY2 + 50)) guy)
                    else do (setObjectPosition (pX,(w - 25)) box)
                            (setObjectPosition (pX2,(w - 25)) guy)

moveBoxDown :: SokobanObject -> SokobanAction ()
moveBoxDown box = do
 guy <- findObject "guy" "guyGroup"
 (pX,pY) <- getObjectPosition box
 (pX2,pY2) <- getObjectPosition guy
 if (pY - 75 < 0)
   then (return())
   else do
           tile <- getTileFromWindowPosition (pX,pY-50)
           if (getTileBlocked tile)
             then return()
             else if (pY - 75 >= 0)
                    then do (setObjectPosition (pX,(pY - 50)) box)
                            (setObjectPosition (pX2,(pY2 - 50)) guy)
                    else do (setObjectPosition (pX,25) box)
                            (setObjectPosition (pX2,25) guy)

gameCycle :: SokobanAction ()
gameCycle = do
 (Score n) <- getGameAttribute
 state <- getGameState
 printOnScreen (show ("Pressione 'p' para parar o jogo ou 'r' para recomecar")) Helvetica18 (0,10) 1.0 1.0 1.0
 printOnScreen (show ("Nivel ") ++ show (n)) TimesRoman24 (w-100,h-24) 1.0 1.0 1.0
 case state of
   (GameInit levelInit) -> do
    setGameState (Level levelInit)
   (Level level) -> do
    case level of
      0 -> beforeOne (Score n)
      1 -> levelOne (Score n)
      2 -> beforeTwo (Score n)
      3 -> levelTwo (Score n)
      4 -> beforeThree (Score n)
      5 -> levelThree (Score n)
      6 -> endGame (Score n)

beforeOne :: GameAttribute -> SokobanAction()
beforeOne (Score n) = do
 setCurrentMapIndex 0
 guy <- findObject "guy" "guyGroup"
 box <- findObject "box" "boxGroup"
 box2 <- findObject "box2" "boxGroup"
 box3 <- findObject "box3" "boxGroup"
 endPoint <- findObject "endPoint" "endGroup"
 endPoint2 <- findObject "endPoint2" "endGroup"
 endPoint3 <- findObject "endPoint3" "endGroup"
 setObjectAsleep False guy
 setObjectPosition (75,425) guy
 setObjectPosition (375,175) box
 setObjectPosition (325,125) box2
 setObjectPosition (175,125) box3
 setObjectPosition (175,225) endPoint
 setObjectPosition (275,275) endPoint2
 setObjectPosition (225,325) endPoint3
 setObjectCurrentPicture 8 guy
 setObjectCurrentPicture 1 box
 setObjectCurrentPicture 1 box2
 setObjectCurrentPicture 1 box3
 setGameAttribute (Score 1)
 setGameState (GameInit 1)

levelOne :: GameAttribute -> SokobanAction()
levelOne (Score n) = do
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
 if (col11 || col12 || col13)
   then setObjectCurrentPicture 3 box
   else setObjectCurrentPicture 1 box
 if (col21 || col22 || col23)
   then setObjectCurrentPicture 3 box2
   else setObjectCurrentPicture 1 box2
 if (col31 || col32 || col33)
   then setObjectCurrentPicture 3 box3
   else setObjectCurrentPicture 1 box3
 when (col11 && col22 && col33) (setGameState (GameInit 2))
 when (col11 && col23 && col32) (setGameState (GameInit 2))
 when (col12 && col21 && col33) (setGameState (GameInit 2))
 when (col12 && col23 && col31) (setGameState (GameInit 2))
 when (col13 && col21 && col32) (setGameState (GameInit 2))
 when (col13 && col22 && col31) (setGameState (GameInit 2))

beforeTwo :: GameAttribute -> SokobanAction()
beforeTwo (Score n) = do
 setCurrentMapIndex 1
 guy <- findObject "guy" "guyGroup"
 box <- findObject "box" "boxGroup"
 box2 <- findObject "box2" "boxGroup"
 box3 <- findObject "box3" "boxGroup"
 endPoint <- findObject "endPoint" "endGroup"
 endPoint2 <- findObject "endPoint2" "endGroup"
 endPoint3 <- findObject "endPoint3" "endGroup"
 setObjectPosition (175,275) guy
 setObjectPosition (225,275) box
 setObjectPosition (275,275) box2
 setObjectPosition (275,225) box3
 setObjectPosition (175,325) endPoint
 setObjectPosition (175,225) endPoint2
 setObjectPosition (225,225) endPoint3
 setObjectCurrentPicture 8 guy
 setObjectCurrentPicture 1 box
 setObjectCurrentPicture 1 box2
 setObjectCurrentPicture 1 box3
 setGameAttribute (Score 2)
 setGameState (GameInit 3)

levelTwo :: GameAttribute -> SokobanAction()
levelTwo (Score n) = do
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
 if (col11 || col12 || col13)
   then setObjectCurrentPicture 3 box
   else setObjectCurrentPicture 1 box
 if (col21 || col22 || col23)
   then setObjectCurrentPicture 3 box2
   else setObjectCurrentPicture 1 box2
 if (col31 || col32 || col33)
   then setObjectCurrentPicture 3 box3
   else setObjectCurrentPicture 1 box3
 when (col11 && col22 && col33) (setGameState (GameInit 4))
 when (col11 && col23 && col32) (setGameState (GameInit 4))
 when (col12 && col21 && col33) (setGameState (GameInit 4))
 when (col12 && col23 && col31) (setGameState (GameInit 4))
 when (col13 && col21 && col32) (setGameState (GameInit 4))
 when (col13 && col22 && col31) (setGameState (GameInit 4))

beforeThree :: GameAttribute -> SokobanAction()
beforeThree (Score n) = do
 setCurrentMapIndex 2
 guy <- findObject "guy" "guyGroup"
 box <- findObject "box" "boxGroup"
 box2 <- findObject "box2" "boxGroup"
 box3 <- findObject "box3" "boxGroup"
 endPoint <- findObject "endPoint" "endGroup"
 endPoint2 <- findObject "endPoint2" "endGroup"
 endPoint3 <- findObject "endPoint3" "endGroup"
 setObjectPosition (125,275) guy
 setObjectPosition (225,275) box
 setObjectPosition (375,275) box2
 setObjectPosition (325,225) box3
 setObjectPosition (225,325) endPoint
 setObjectPosition (225,375) endPoint2
 setObjectPosition (175,375) endPoint3
 setObjectCurrentPicture 8 guy
 setObjectCurrentPicture 1 box
 setObjectCurrentPicture 1 box2
 setObjectCurrentPicture 1 box3
 setGameAttribute (Score 3)
 setGameState (GameInit 5)

levelThree :: GameAttribute -> SokobanAction()
levelThree (Score n) = do
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
 if (col11 || col12 || col13)
   then setObjectCurrentPicture 3 box
   else setObjectCurrentPicture 1 box
 if (col21 || col22 || col23)
   then setObjectCurrentPicture 3 box2
   else setObjectCurrentPicture 1 box2
 if (col31 || col32 || col33)
   then setObjectCurrentPicture 3 box3
   else setObjectCurrentPicture 1 box3
 when (col11 && col22 && col33) (setGameState (GameInit 6))
 when (col11 && col23 && col32) (setGameState (GameInit 6))
 when (col12 && col21 && col33) (setGameState (GameInit 6))
 when (col12 && col23 && col31) (setGameState (GameInit 6))
 when (col13 && col21 && col32) (setGameState (GameInit 6))
 when (col13 && col22 && col31) (setGameState (GameInit 6))

endGame :: GameAttribute -> SokobanAction()
endGame (Score n) = do
  printOnScreen (show ("Parabens!")) TimesRoman24 (0, height - 24) 1.0 1.0 1.0
  guy <- findObject "guy" "guyGroup"
  setObjectAsleep True guy
