module Bullet where

import qualified Graphics.Gloss.Data.Point.Arithmetic as Pnt
import qualified Graphics.Gloss.Data.Vector as Vec
import Struct
import Asteroid

-- Функция создания снаряда корабля
createBull :: Ship -> Bullet 
createBull (Ship (Object (x, y) height width) vel) = Bullet (Object (x, y) 4 4) (10 Pnt.* (Vec.normalizeV vel)) 30

-- Функция движения снаряда
moveBull :: Game -> Game 
moveBull game = game { bullets = map move (filter (\x -> durability x /= 0 ) (bullets game)) } where
    move bull = bull { bullBody = Object ((position (bullBody bull)) Pnt.+ (bullVel bull)) (height (bullBody bull)) (width (bullBody bull))
                     , durability = (durability bull) - 1 }

-- Функция обработки столкновения астероида со снарядом
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
