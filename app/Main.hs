-- Importe dos Módulos
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Constantes para largura, altura 
screenWidth, screenHeight :: Int
screenWidth = 1000
screenHeight = 650

-- Constante para título da tela
screenTitle :: String
screenTitle = "Platformer"

-- Constantes para escalamento dos sprites (ainda n foi USADO)
characterScaling, tileScaling :: Float
characterScaling = 10
tileScaling = 0.5

-- Definição de tipos de dados para o Player
data Player = Player
    { playerSprite :: Picture
    , playerPosition :: (Float, Float)
    }

-- Function to load and scale the player's image from BMP file
loadPlayerSprite :: Float -> IO Picture
loadPlayerSprite scalingFactor = do
    sprite <- loadBMP "./app/c_front.bmp"
    let scaledSprite = scale scalingFactor scalingFactor sprite
    return scaledSprite

-- Definição do jogador inicial
initialPlayer :: IO Player
initialPlayer = do
    sprite <- loadPlayerSprite characterScaling  -- Load player image with character scaling
    return Player
        { playerSprite = sprite
        , playerPosition = (0, 0)  -- Initial player position
        }

-- Definição de tipos de dados para Wall
data Wall = Wall
    { wallSprite :: Picture
    , wallPosition :: (Float, Float)
    }

-- Lista inicial de paredes (um exemplo simples com uma única parede no chão)
initialWallList :: [Wall]
initialWallList =
    [ Wall
        { wallSprite = scale tileScaling tileScaling $ Color green $ rectangleSolid 128 32 -- Exemplo de parede como um retângulo verde escalonada
        , wallPosition = (512, 32) -- Posição da parede no chão
        }
        , Wall
        { wallSprite = Color green $ rectangleSolid 128 32
        , wallPosition = (256, 32)
        }
    ]

-- Definição de tipos de dados para o estado do jogo
data GameState = GameState
    { playerList :: [Player]
    , wallList :: [Wall]
    }

-- Definição do estado inicial do jogo (usando o GameState)
initialState :: IO GameState
initialState = do
    player <- initialPlayer
    return GameState
        { playerList = [player]
        , wallList = initialWallList
        }

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


-- Função principal para iniciar o jogo
main :: IO ()
main = do
    initialState' <- initialState
    playIO
        (InWindow screenTitle (screenWidth, screenHeight) (100, 100)) -- Configurações da janela
        (makeColorI 100 149 237 255) -- Cor de fundo (Cornflower Blue)
        60 -- FPS (frames por segundo)
        initialState' -- Estado inicial do jogo
        render -- Função para renderizar o estado do jogo
        handleEvent -- Função para lidar com eventos de entrada
        update -- Função para atualizar o estado do jogo


