module Main where

import Graphics.Gloss



-- Função principal que cria a janela e define o estado inicial
main :: IO ()
main = display (InWindow "Hello, Gloss!" (800, 600) (10, 10)) white picture

-- Função que desenha o círculo na janela
picture :: Picture
picture = circle 50



