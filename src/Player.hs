module Player where

import qualified Graphics.Gloss.Data.Point.Arithmetic as Pnt
import qualified Graphics.Gloss.Geometry.Angle as Ang
import qualified Graphics.Gloss.Data.Vector as Vec
import Struct

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

-- Функция обработки столкновения корабля
checkShip :: Ship -> [Asteroid] -> Bool
checkShip _ [] = False
checkShip ship (ast : astS) = finCheck (checkShip1 ship ast : checkShip ship astS : []) where
    finCheck col
        | any (True ==) col = True
        | otherwise = False

checkShip1 :: Ship -> Asteroid -> Bool
checkShip1 (Ship (Object (x_s, y_s) h_s w_s) _) (Asteroid (Object (x_a, y_a) h_a w_a) _ _)
    | distX > w_a/4 + h_s || distY > h_a/4 + h_s = False
    | distX <= w_a/4 || distY <= h_a/4 = True
    | otherwise = distCSQ <= (h_a/4)^2 where
        distX = abs (x_s - x_a)
        distY = abs (y_s - y_a)
        distCSQ = (distX - w_a/4)^2 + (distY - h_a/4)^2
