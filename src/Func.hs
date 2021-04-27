module Func where

import qualified Data.Set as Set
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pnt
import qualified Graphics.Gloss.Data.Vector as Vec
import qualified Graphics.Gloss.Geometry.Angle as Ang
import Struct

-- Функция, которая создает объекты игры: астероиды и игрок
initGame :: Game
initGame = Game createAst (Ship (Object (0, 0) 30 20) (0, 2) 3) [] False 0 Set.empty

-- Функция, которая отрисовывает текущие положение всех объектов игры в окне
render :: Game -> Picture
render (Game asteroids (Ship (Object (x, y) height width) vel _ ) bulls _ _ _ ) = 
    Pictures ([(Color white (Translate x y (Rotate (Ang.radToDeg (-(Vec.argV vel) + pi/2)) (lineLoop
      [(-width / 2, -height / 2), (0, -height / 3), (width / 2, -height / 2), (0, height / 2)]))))] 
      ++ map viewAst asteroids ++ map viewBull bulls)

-- Функция перемещения корабля
movePlayer :: Ship -> Ship
movePlayer ship = ship { shipBody = chgCoords (shipBody ship) (shipVel ship) } 
    where
        chgCoords (Object (x, y) h w) vel = Object ((x, y) Pnt.+ vel) h w

-- Функция поворота корабля
rotPlayer :: Char -> Ship -> Ship
rotPlayer sign ship
    | sign == 'd' = ship { shipVel = Vec.rotateV (Ang.degToRad (-3)) (shipVel ship) }
    | otherwise = ship { shipVel = Vec.rotateV (Ang.degToRad 3) (shipVel ship) }

-- Функция обработки ввода с клавиатуры
handleEvent :: Event -> Game -> Game
handleEvent (EventKey (Char 'w') Down _ _) game = 
    game { keys = Set.insert (Char 'w') (keys game)
         , player = movePlayer (player game) }
handleEvent (EventKey (Char 'w') Up _ _) game = game { keys = Set.delete (Char 'w') (keys game) }
handleEvent (EventKey (Char 'd') Down _ _) game =
    game { keys = Set.insert (Char 'd') (keys game)
         , player = rotPlayer 'd' (player game) }
handleEvent (EventKey (Char 'd') Up _ _) game = game { keys = Set.delete (Char 'd') (keys game) }
handleEvent (EventKey (Char 'a') Down _ _) game =
    game { keys = Set.insert (Char 'a') (keys game)
         , player = rotPlayer 'a' (player game) }
handleEvent (EventKey (Char 'a') Up _ _) game = game { keys = Set.delete (Char 'a') (keys game) }
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) game = game { bullets = (createBull (player game)) : (bullets game)}
handleEvent _ state = state

updateGame :: Float -> Game -> Game
updateGame _ game = checkGame (handleColl (moveAsteroid (moveBull (keyPush game))))

-- Функция движения снаряда
moveBull :: Game -> Game 
moveBull game = game { bullets = map move (filter (\x -> durability x /= 0 ) (bullets game)) } where
    move bull = bull { bullBody = Object ((position (bullBody bull)) Pnt.+ (bullVel bull)) (height (bullBody bull)) (width (bullBody bull))
                     , durability = (durability bull) - 1 }

-- Функция перемещения астероида
moveAsteroid :: Game -> Game
moveAsteroid game = game { asteroids = map move (asteroids game) } where
    move ast = ast { astBody = Object ((position (astBody ast)) Pnt.+ (astVel ast)) (height (astBody ast)) (width (astBody ast))}

-- Функция, проверяющая зажатия клавиш
keyPush :: Game -> Game
keyPush game
    | Set.member (Char 'w') (keys game) && Set.member (Char 'd') (keys game) = 
        game { player = movePlayer (rotPlayer 'd' (player game)) }
    | Set.member (Char 'w') (keys game) && Set.member (Char 'a') (keys game) = 
        game { player = movePlayer (rotPlayer 'a' (player game)) }
    | Set.member (Char 'w') (keys game) = game { player = movePlayer (player game) }
    | Set.member (Char 'd') (keys game) = game { player = rotPlayer 'd' (player game) }
    | Set.member (Char 'a') (keys game) = game { player = rotPlayer 'a' (player game) }
    | otherwise = game

-- Функция создания снаряда корабля
createBull :: Ship -> Bullet 
createBull (Ship (Object (x, y) height width) vel _) = Bullet (Object (x, y) 4 4) (10 Pnt.* (Vec.normalizeV vel)) 30

-- Функция, которая возвращает картинку снарядя
viewBull :: Bullet -> Picture
viewBull (Bullet (Object (x, y) height width) vel _ ) = (Color white (Translate x y (Rotate (Ang.radToDeg (-(Vec.argV vel) + pi/2)) (Translate 0 30 (rectangleSolid height width)))))

-- Функция, создающая атероиды
createAst :: [Asteroid]
createAst = Asteroid (Object (0, 200) 100 100 ) 3 (1, 0) : []

-- Функция, которая возвращает картинку астероида
viewAst :: Asteroid -> Picture
viewAst (Asteroid (Object (x, y) h w) _ vel ) = (Color white (Translate x y (lineLoop lines))) where
    lines = [(-w/3, -h/2), (w/3, -h/2), (w/2, 0), (w/3, h/2), (-w/3, h/2), (-w/2, 0)]

-- Функция обработки столкновений объектов в игре
handleColl :: Game -> Game
handleColl game = game { asteroids = concatMap (checkAst (bullets game)) (asteroids game) }

-- Функция обработки столкновения астероида
checkAst :: [Bullet] -> Asteroid -> [Asteroid]
checkAst [] ast = [ast]
checkAst bul ast = isColl (checkAst1 bul ast) ast

isColl :: [Bool] -> Asteroid -> [Asteroid]
isColl coll ast
    | any (True ==) coll = destroyAst ast
    | otherwise = [ast]

checkAst1 :: [Bullet] -> Asteroid -> [Bool]
checkAst1 [] ast = [False]
checkAst1 (bull : bullS) ast = checkAst2 bull ast : checkAst1 bullS ast

checkAst2 :: Bullet -> Asteroid -> Bool
checkAst2 (Bullet (Object (x_b, y_b) h_b w_b) _ _) (Asteroid (Object (x_a, y_a) h_a w_a) stage vel)
    | x_b >= x_a - w_a/2 && x_b <= x_a + w_a/2 && y_b + 10 >= y_a - h_a/2 && y_b - 10 <= y_a + h_a/2 = True
    | otherwise = False

-- Функция обработки разрущения астероида
destroyAst :: Asteroid -> [Asteroid]
destroyAst (Asteroid (Object (x, y) h w) stage vel) 
    | stage == 0 = []
    | otherwise = [Asteroid (Object (x - w/2, y) (2*h/3) (2*w/3)) (stage - 1) vel, Asteroid (Object (x + w/2, y) (2*h/3) (2*w/3)) (stage - 1) vel]

-- Функция проверки состояния игры
checkGame :: Game -> Game
checkGame game = game { asteroids = map transAst (asteroids game)
                      , player = transShip (player game)
                      , bullets = map transBul (bullets game)} where
                          transAst (Asteroid (Object (x, y) w h) dest vel) = Asteroid (Object (transObj (x, y)) w h) dest vel
                          transShip (Ship (Object (x, y) w h) vel liv) = Ship (Object (transObj (x, y)) w h) vel liv
                          transBul (Bullet (Object (x, y) w h) vel dur) = Bullet (Object (transObj (x, y)) w h) vel dur

transObj :: Point -> Point
transObj (x, y)
    | y >= 350 = (x, -350)
    | y <= -350 = (x, 350)
    | x >= 450 = (-450, y)
    | x <= -450 = (450 , y)
    | otherwise = (x, y)

run :: IO ()
run = do
    play display black 60 initGame render handleEvent updateGame
    where
        display = InWindow "Asteroids" (800, 600) (0, 0)
