-- Importe dos Módulos
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Constantes para largura, altura
screenWidth, screenHeight :: Int
screenWidth = 1000
screenHeight = 650

-- Constante para título da tela
screenTitle :: String
screenTitle = "Cueio Lalao"

-- Constantes para escalamento dos sprites
characterScaling, wallScaling, tileScaling, soilScaling, skyscaling :: Float
characterScaling = 10
wallScaling = 10
tileScaling = 10
soilScaling = 10
skyscaling = 2

-- Definição de tipos de dados para o Player
data Player = Player
  { playerSprite :: Picture,
    playerPosition :: (Float, Float)
  }

-- Function to load and scale the player's image from BMP file
loadPlayerSprite :: Float -> IO Picture
loadPlayerSprite scalingFactor = do
  sprite <- loadBMP "./app/c_right.bmp"
  let scaledSprite = scale scalingFactor scalingFactor sprite
  return scaledSprite

-- Definição do jogador inicial
initialPlayer :: IO Player
initialPlayer = do
  sprite <- loadPlayerSprite characterScaling -- Load player image with character scaling
  return
    Player
      { playerSprite = sprite,
        playerPosition = (-350, -100) -- Initial player position
      }

-- Definição de tipos de dados para Wall
data Wall = Wall
  { wallSprite :: Picture,
    wallPosition :: (Float, Float)
  }

-- Function to load and scale the player's image from BMP file
loadWallSprite :: Float -> IO Picture
loadWallSprite scalingFactor = do
  sprite <- loadBMP "./app/grass_wall.bmp"
  let scaledSprite = scale scalingFactor scalingFactor sprite
  return scaledSprite

-- Definição de tipos de dados para Sky
data Sky = Sky
    { skySprite :: Picture,
      skyPosition :: (Float, Float)
    }

-- Function to load and scale the player's image from BMP file
loadSkySprite :: Float -> IO Picture
loadSkySprite scalingFactor = do
    sprite <- loadBMP "./app/cloud_blue.bmp"
    let scaledSprite = scale scalingFactor scalingFactor sprite
    return scaledSprite

initialSkyList :: IO [Sky]
initialSkyList = do
    sprite <- loadSkySprite 1
    return
        [ Sky
            { skySprite = sprite,
              skyPosition = (0, 0)
            }
        ]


-- Lista inicial de paredes (um exemplo simples com uma única parede no chão)
initialWallList :: IO [Wall]
initialWallList = do
  sprite <- loadWallSprite wallScaling
  return
    [ Wall
        { wallSprite = sprite, -- Exemplo de parede como um retângulo verde escalonada
          wallPosition = (-500, 0) -- Posição da parede no chão
        }
        -- , Wall
        -- { wallSprite = sprite
        -- , wallPosition = (300, 0)
        -- }
    ]

-- Definição de tipos de dados para Wall
data Tile = Tile
  { tileSprite :: Picture,
    tilePosition :: (Float, Float)
  }

-- Function to load and scale the player's image from BMP file
loadTileSprite :: Float -> IO Picture
loadTileSprite scalingFactor = do
  sprite <- loadBMP "./app/grass_floor.bmp"
  let scaledSprite = scale scalingFactor scalingFactor sprite
  return scaledSprite

loadTileSoilSprite :: Float -> IO Picture
loadTileSoilSprite scalingFactor = do
  sprite <- loadBMP "./app/soil_floor.bmp"
  let scaledSprite = scale scalingFactor scalingFactor sprite
  return scaledSprite

loadWaterSprite :: Float -> IO Picture
loadWaterSprite scalingFactor = do
  sprite <- loadBMP "./app/water.bmp"
  let scaledSprite = scale scalingFactor scalingFactor sprite
  return scaledSprite

-- Lista inicial de chãos
initialTileList :: IO [Tile]
initialTileList = do
  sprite <- loadTileSprite tileScaling
  soilSprite <- loadTileSoilSprite soilScaling
  waterSprite <- loadWaterSprite soilScaling
  return
    [ Tile
        { tileSprite = waterSprite, -- Exemplo de solo
          tilePosition = (-300, -300) -- Posição do chao
        },
        Tile
        { tileSprite = waterSprite, -- Exemplo de solo
          tilePosition = (300, -300) -- Posição do chao
        },
        Tile
        { tileSprite = soilSprite, -- Exemplo de solo
          tilePosition = (-300, -200) -- Posição do chao
        },
        Tile
        { tileSprite = soilSprite, -- Exemplo de solo
          tilePosition = (0, -200) -- Posição do chao
        },
        Tile
        { tileSprite = soilSprite, -- Exemplo de solo
          tilePosition = (300, -200) -- Posição do chao
        },
        Tile
        { tileSprite = sprite,
          tilePosition = (-100, -200)
        },
        Tile
        { tileSprite = sprite,
          tilePosition = (400, -200)
        }
    ]

-- Definição de tipos de dados para o estado do jogo
data GameState = GameState
  { skyList :: [Sky],
    tileList :: [Tile],
    wallList :: [Wall],
    playerList :: [Player]
  }

-- Initial game state (using GameState)
initialState :: IO GameState
initialState = do
  sky <- initialSkyList
  tiles <- initialTileList
  walls <- initialWallList
  player <- initialPlayer

  return
    GameState
      { skyList = sky,
        tileList = tiles,
        wallList = walls,
        playerList = [player]
      }

-- Função para renderizar o estado do jogo
render :: GameState -> IO Picture
render gameState =
  return $
    pictures $
    map renderSky (skyList gameState)
        ++ map renderWall (wallList gameState)
        ++ map renderTile (tileList gameState)
        ++ map renderPlayer (playerList gameState)
     
  where
    renderSky sky = uncurry translate (skyPosition sky) $ skySprite sky
    renderTile tile = uncurry translate (tilePosition tile) $ tileSprite tile
    renderWall wall = uncurry translate (wallPosition wall) $ wallSprite wall
    renderPlayer player = uncurry translate (playerPosition player) $ playerSprite player

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
