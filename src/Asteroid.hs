module Asteroid where

import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as Pnt
import System.Random
import Struct

-- Функция, создающая атероиды в начале игры
createAst :: Int -> StdGen -> StdGen -> [Asteroid]
createAst 0 _ _ = []
createAst n genx geny = Asteroid (Object (fst xGen, fst yGen) 100 100) 3 (fst xVel, fst yVel) : createAst (n-1) (snd xGen) (snd yGen) where
    xGen = randomR (-400, 400) genx
    yGen = randomR (-300, 300) geny
    xVel = randomR (-2, 2) genx
    yVel = randomR (-2, 2) geny

-- Функция перемещения астероида
moveAsteroid :: Game -> Game
moveAsteroid game = game { asteroids = map move (asteroids game) } where
    move ast = ast { astBody = Object ((position (astBody ast)) Pnt.+ (astVel ast)) (height (astBody ast)) (width (astBody ast))}

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

-- Функция обработки разрущения астероида
destroyAst :: (Float, Float) -> Asteroid -> [Asteroid]
destroyAst (velX, velY) (Asteroid (Object (x, y) h w) stage _) 
    | stage == 0 = []
    | otherwise = [Asteroid (Object (x - w/3, y) (2*h/3) (2*w/3)) (stage - 1) (velX, velY), Asteroid (Object (x + w/3, y) (2*h/3) (2*w/3)) (stage - 1) (velX - 0.5, velY + 0.5)]


-- Функция, добавляющая астероиды в процессе игры
addAst :: Game -> Game
addAst game
    | countAst (isNotStage3 (asteroids game)) <= 3 = game { asteroids = makeAst (position (shipBody (player game))) (randomGenX game) (randomGenY game) : (asteroids game)}
    | otherwise = game where
        countAst asts = length (filter (\x -> x == True) asts)

isNotStage3 :: [Asteroid] -> [Bool]
isNotStage3 [] = []
isNotStage3 (ast : astS) = is3 (destroyStage ast) : isNotStage3 astS where
    is3 stage
        | stage == 3 = True
        | otherwise = False

makeAst :: Point -> StdGen -> StdGen -> Asteroid
makeAst (x, y) genx geny
    | abs (x - 100) >= 300 = Asteroid (Object (-459, fst yGen) 100 100) 3 (notZero (fst xVel, fst yVel))
    | abs (x - 100) >= 200 = Asteroid (Object (459, fst yGen) 100 100) 3 (notZero (fst xVel, fst yVel))
    | abs (x - 100) >= 100 = Asteroid (Object (fst xGen, -349) 100 100) 3 (notZero (fst xVel, fst yVel))
    | otherwise = Asteroid (Object (fst xGen, 349) 100 100) 3 (notZero (fst xVel, fst yVel)) where
        xGen = randomR (-400, 400) genx
        yGen = randomR (-300, 300) geny
        xVel = randomR (-3, 3) genx
        yVel = randomR (-3, 3) geny
