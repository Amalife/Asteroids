module Kernel where

import qualified Data.Set as Set
import System.Random
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pnt
import qualified Graphics.Gloss.Data.Vector as Vec
import qualified Graphics.Gloss.Geometry.Angle as Ang
import Struct
import Render
import Asteroid
import Bullet
import Player

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
updateGame _ game
    | (isEnd game) == True = checkPos (moveAsteroid game)
    | otherwise = addAst (checkPos (handleColl (moveAsteroid (moveBull (keyPush game)))))

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

-- Функция обработки столкновений объектов в игре
handleColl :: Game -> Game
handleColl game = game { asteroids = concatMap (checkAst (fst genX, fst genY) (bullets game)) (asteroids game) 
                       , scores = addScore (concatMap (checkAst1 (bullets game)) (asteroids game)) (scores game)
                       , isEnd = checkShip (player game) (asteroids game)
                       , randomGenX = snd genX
                       , randomGenY = snd genY } where
                           genX = randomR (-3, 3) (randomGenX game)
                           genY = randomR (-3, 3) (randomGenY game)

-- Функция добавления очков
addScore :: [Bool] -> Integer -> Integer
addScore col scor
    | any (True ==) col = scor + 10
    | otherwise = scor

-- Функция проверки позиции объектов игры
checkPos :: Game -> Game
checkPos game = game { asteroids = map transAst (asteroids game)
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

-- MAIN --
run :: IO ()
run = do
    genx <- newStdGen
    geny <- newStdGen
    let initGame =  Game (map avoidShip (createAst 5 genx geny)) (Ship (Object (0, 0) 30 20) (0, 3)) [] False 0 Set.empty genx geny
    play display black 60 initGame render handleEvent updateGame
    where
        display = InWindow "Asteroids" (800, 600) (0, 0)
