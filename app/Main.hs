-- Importe o Gloss e os módulos necessários
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Constantes para largura, altura e título da tela
screenWidth, screenHeight :: Int
screenWidth = 1000
screenHeight = 650

screenTitle :: String
screenTitle = "Platformer"

-- Constantes para escalamento dos sprites
characterScaling, tileScaling :: Float
characterScaling = 1
tileScaling = 0.5

-- Definição de tipos de dados para o Player
data Player = Player
    { playerSprite :: Picture
    , playerPosition :: (Float, Float)
    }

-- Definição de tipos de dados para Wall
data Wall = Wall
    { wallSprite :: Picture
    , wallPosition :: (Float, Float)
    }
-- Definição de tipos de dados para o estado do jogo
data GameState = GameState
    { playerList :: [Player]
    , wallList :: [Wall]
    }

-- Definição do estado inicial do jogo (usando o GameState)
initialState :: GameState
initialState = GameState
    { playerList = [initialPlayer]
    , wallList = initialWallList
    }

-- Função principal para iniciar o jogo
main :: IO ()
main = playIO
    (InWindow screenTitle (screenWidth, screenHeight) (100, 100)) -- Configurações da janela
    (makeColorI 100 149 237 255) -- Cor de fundo (Cornflower Blue)
    60 -- FPS (frames por segundo)
    initialState -- Estado inicial do jogo
    render -- Função para renderizar o estado do jogo
    handleEvent -- Função para lidar com eventos de entrada
    update -- Função para atualizar o estado do jogo

-- Função para renderizar o estado do jogo
render :: GameState -> IO Picture
render gameState = return $ pictures $ map renderPlayer (playerList gameState) ++ map renderWall (wallList gameState)
    where
        renderPlayer player = uncurry translate (playerPosition player) $ playerSprite player
        renderWall wall = uncurry translate (wallPosition wall) $ wallSprite wall

-- Função para lidar com eventos de entrada
handleEvent :: Event -> GameState -> IO GameState
handleEvent _ state = return state -- Por enquanto, não tratamos eventos

-- Função para atualizar o estado do jogo
update :: Float -> GameState -> IO GameState
update _ state = return state -- Por enquanto, não atualizamos o estado

-- Definição do jogador inicial
initialPlayer :: Player
initialPlayer = Player
    { playerSprite = Color black $ circleSolid 20 -- Exemplo de jogador como um círculo preto
    , playerPosition = (64, 128) -- Posição inicial do jogador
    }

-- Lista inicial de paredes (um exemplo simples com uma única parede no chão)
initialWallList :: [Wall]
initialWallList =
    [ Wall
        { wallSprite = Color green $ rectangleSolid 128 32 -- Exemplo de parede como um retângulo verde
        , wallPosition = (512, 32) -- Posição da parede no chão
        }
    ]
