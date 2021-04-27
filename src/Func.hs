module Func where

import qualified Data.Set as Set
import System.Random
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pnt
import qualified Graphics.Gloss.Data.Vector as Vec
import qualified Graphics.Gloss.Geometry.Angle as Ang
import Struct

-- Функция, которая отрисовывает текущие положение всех объектов игры в окне
render :: Game -> Picture
render (Game asteroids (Ship (Object (x, y) height width) vel) bulls _ scores _ _ _) = 
    Pictures (viewScore scores : [(Color white (Translate x y (Rotate (Ang.radToDeg (-(Vec.argV vel) + pi/2)) (lineLoop
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

-- Функция обработки состояния игры
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
createBull (Ship (Object (x, y) height width) vel) = Bullet (Object (x, y) 4 4) (10 Pnt.* (Vec.normalizeV vel)) 30

-- Функция, которая возвращает картинку снарядя
viewBull :: Bullet -> Picture
viewBull (Bullet (Object (x, y) height width) vel _ ) = (Color white (Translate x y (Rotate (Ang.radToDeg (-(Vec.argV vel) + pi/2)) (Translate 0 30 (rectangleSolid height width)))))

-- Функция, создающая атероиды
createAst :: Int -> StdGen -> StdGen -> [Asteroid]
createAst 0 _ _ = []
createAst n genx geny = Asteroid (Object (fst xGen, fst yGen) 100 100) 3 (fst xVel, fst yVel) : createAst (n-1) (snd xGen) (snd yGen) where
    xGen = randomR (-400, 400) genx
    yGen = randomR (-300, 300) geny
    xVel = randomR (-3, 3) genx
    yVel = randomR (-3, 3) geny

-- Функция, убирающая астероиды в пределах близости корабля во время инициализации карты
avoidShip :: Asteroid -> Asteroid
avoidShip (Asteroid (Object (x, y) h w) stg vel)
    | x > -100 && x < 0 = Asteroid (Object (x - 100, y) h w) stg (notZero vel)
    | x > 0 && x < 100 = Asteroid (Object (x + 100, y) h w) stg (notZero vel)
    | y > 0 && y < 100 = Asteroid (Object (x, y + 100) h w) stg (notZero vel)
    | y > -100 && y < 0 = Asteroid (Object (x, y - 100) h w) stg (notZero vel)
    | otherwise = Asteroid (Object (x, y) h w) stg (notZero vel)

-- Функция, убирающая нулевую скорость у астероида по одному направлению
notZero :: (Float, Float) -> (Float, Float)
notZero (x, y)
    | x == 0 = (1, y)
    | y == 0 = (x, 1)
    | otherwise = (x, y)

-- Функция, которая возвращает картинку астероида
viewAst :: Asteroid -> Picture
viewAst (Asteroid (Object (x, y) h w) _ vel ) = (Color white (Translate x y (lineLoop lines))) where
    lines = [(-w/3, -h/2), (w/3, -h/2), (w/2, 0), (w/3, h/2), (-w/3, h/2), (-w/2, 0)]

-- Функция обработки столкновений объектов в игре
handleColl :: Game -> Game
handleColl game = game { asteroids = concatMap (checkAst (fst genX, fst genY) (bullets game)) (asteroids game) 
                       , scores = addScore (concatMap (checkAst1 (bullets game)) (asteroids game)) (scores game)
                       --, isEnd = checkShip (player game) (asteroids game)
                       , randomGenX = snd genX
                       , randomGenY = snd genY } where
                           genX = randomR (-3, 3) (randomGenX game)
                           genY = randomR (-3, 3) (randomGenY game)

{-- Функция обработки столкновения корабля
checkShip :: Ship -> [Asteroid] -> Bool
checkShip _ [] = [False]
checkShip ship (ast : astS) = finCheck (checkShip1 ship ast : checkShip ship astS) where
    finCheck col
        | any (True ==) col = True
        | otherwise = False

checkShip1 :: Ship -> Asteroid -> Bool
checkShip1 (Ship (Object (x_s, y_s) h_s w_s) _ _) (Asteroid (Object (x_a, y_a) h_a w_a) _ _)
    | x_s + w_s/2 >= x_a - w_a/3 && x_s  
    | otherwise = False-}
-- Функция добавления очков
addScore :: [Bool] -> Integer -> Integer
addScore col scor
    | any (True ==) col = scor + 10
    | otherwise = scor

-- Функция обработки столкновения астероида
checkAst :: (Float, Float) -> [Bullet] -> Asteroid -> [Asteroid]
checkAst _ [] ast = [ast]
checkAst genV bul ast = isColl genV (checkAst1 bul ast) ast

isColl :: (Float, Float) -> [Bool] -> Asteroid -> [Asteroid]
isColl genV coll ast
    | any (True ==) coll = destroyAst genV ast
    | otherwise = [ast]

checkAst1 :: [Bullet] -> Asteroid -> [Bool]
checkAst1 [] ast = [False]
checkAst1 (bull : bullS) ast = checkAst2 bull ast : checkAst1 bullS ast

checkAst2 :: Bullet -> Asteroid -> Bool
checkAst2 (Bullet (Object (x_b, y_b) h_b w_b) _ _) (Asteroid (Object (x_a, y_a) h_a w_a) stage vel)
    | x_b >= x_a - w_a/2 && x_b <= x_a + w_a/2 && y_b + 10 >= y_a - h_a/2 && y_b - 10 <= y_a + h_a/2 = True
    | otherwise = False

-- Функция обработки разрущения астероида
destroyAst :: (Float, Float) -> Asteroid -> [Asteroid]
destroyAst (velX, velY) (Asteroid (Object (x, y) h w) stage _) 
    | stage == 0 = []
    | otherwise = [Asteroid (Object (x - w/3, y) (2*h/3) (2*w/3)) (stage - 1) (velX, velY), Asteroid (Object (x + w/3, y) (2*h/3) (2*w/3)) (stage - 1) (velX - 0.5, velY + 0.5)]

-- Функция проверки состояния игры
checkGame :: Game -> Game
checkGame game = game { asteroids = map transAst (asteroids game)
                      , player = transShip (player game)
                      , bullets = map transBul (bullets game)} where
                          transAst (Asteroid (Object (x, y) w h) dest vel) = Asteroid (Object (transObj (x, y)) w h) dest vel
                          transShip (Ship (Object (x, y) w h) vel) = Ship (Object (transObj (x, y)) w h) vel
                          transBul (Bullet (Object (x, y) w h) vel dur) = Bullet (Object (transObj (x, y)) w h) vel dur

transObj :: Point -> Point
transObj (x, y)
    | y >= 350 = (x, -350)
    | y <= -350 = (x, 350)
    | x >= 450 = (-450, y)
    | x <= -450 = (450 , y)
    | otherwise = (x, y)

viewScore :: Integer -> Picture
viewScore scor = Color white (Translate (-400) 270 (Scale 0.2 0.2 (Text (concat ["Scores:", show scor]))))

run :: IO ()
run = do
    genx <- newStdGen
    geny <- newStdGen
    let initGame =  Game (map avoidShip (createAst 7 genx geny)) (Ship (Object (0, 0) 30 20) (0, 3)) [] False 0 Set.empty genx geny
    play display black 60 initGame render handleEvent updateGame
    where
        display = InWindow "Asteroids" (800, 600) (0, 0)
