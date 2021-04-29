module Render where

import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Geometry.Angle as Ang
import qualified Graphics.Gloss.Data.Vector as Vec
import Struct

-- Функция, которая отрисовывает текущие положение всех объектов игры в окне
render :: Game -> Picture
render (Game asteroids (Ship (Object (x, y) height width) vel) bulls end scores _ _ _)
    | end == True = Pictures (viewOver scores ++ map viewAst asteroids)
    | otherwise = Pictures (viewScore scores : [(Color white (Translate x y (Rotate (Ang.radToDeg (-(Vec.argV vel) + pi/2)) (lineLoop
      [(-width / 2, -height / 2), (0, -height / 3), (width / 2, -height / 2), (0, height / 2)]))))] 
      ++ map viewAst asteroids ++ map viewBull bulls)

-- Функция, которая возвращает картинку снарядя
viewBull :: Bullet -> Picture
viewBull (Bullet (Object (x, y) height width) vel _ ) = (Color white (Translate x y (Rotate (Ang.radToDeg (-(Vec.argV vel) + pi/2)) (Translate 0 30 (rectangleSolid height width)))))

-- Функция, которая возвращает картинку астероида
viewAst :: Asteroid -> Picture
viewAst (Asteroid (Object (x, y) h w) _ vel ) = (Color white (Translate x y (lineLoop lines))) where
    lines = [(-w/3, -h/2), (w/3, -h/2), (w/2, 0), (w/3, h/2), (-w/3, h/2), (-w/2, 0)]

-- Функция, выводящая очки на экран
viewScore :: Integer -> Picture
viewScore scor = Color white (Translate (-400) 270 (Scale 0.2 0.2 (Text (concat ["Scores:", show scor]))))

-- Функция, выводящая экран окончания игры
viewOver :: Integer -> [Picture]
viewOver scor = [Color white (Translate (-200) 50 (Scale 0.5 0.5 (Text ("Game Over ")))),
            Color white (Translate (-200) (-50) (Scale 0.5 0.5 (Text (concat ["Scores: ", show scor]))))]
