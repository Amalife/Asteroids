module Struct where

import qualified Data.Set as Set
import Graphics.Gloss.Interface.Pure.Game

-- Тип точки, предполагается как пиксель на экране и левый нижний угол объекта игры
--type Point = (Float, Float)

--type Vector = (Point, Point)
-- Структура объекта игры, имеет свою позицию, высоту и ширину спрайта
data Object = Object 
    { position :: Point
    , height :: Float
    , width :: Float
    } 

-- Структура астероида, имеет структуру объекта стадию разрушения и постоянную скорость
data Asteroid = Asteroid
    { astBody :: Object
    , destroyStage :: Int 
    , astVel :: Vector
    }

-- Структура коробля, имеет структуру объекта, скорость корабля и кол-во жизней
data Ship = Ship
    { shipBody :: Object
    , shipVel :: Vector
    , lives :: Int 
    }

-- Структура снаряда корабля, имеет структуру объекта, скорость снаряда и прочность снаряда
data Bullet = Bullet
    { bullBody :: Object
    , bullVel :: Vector
    , durability :: Int
    }

-- Структура самой игры, имеет список астероидов, структуру корабля, список снарядов корабля на карте, проверку на конец игры и очки игры
data Game = Game
    { asteroids :: [Asteroid]
    , player :: Ship
    , bullets :: [Bullet]
    , isEnd :: Bool
    , scores :: Integer
    , keys :: Set.Set Key
    }