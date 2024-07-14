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
characterScaling, chickenScaling, cloudScalling, eggScalling, mountainScalling, platformScaling, elementsScalling, skyScaliung, tileScaling, treeScaling, wallScaling, waterScaling, soilScaling :: Float
characterScaling = 5 
chickenScaling = 5 
cloudScalling = 5 
eggScalling = 5 
mountainScalling = 5 
platformScaling = 5 
elementsScalling = 5 
skyScaliung = 5 
tileScaling = 5 
treeScaling = 5 
wallScaling = 5 
waterScaling = 5 
soilScaling = 5

-- Definição de tipos de dados para o Player
data Player = Player
  { playerSprite :: Picture,
    playerPosition :: (Float, Float)
  }

-- Function to load and scale the player's image from BMP file
loadPlayerSprite :: Float -> IO Picture
loadPlayerSprite scalingFactor = do
  sprite <- loadBMP "./app/character/c_right.bmp"
  let scaledSprite = scale scalingFactor scalingFactor sprite
  return scaledSprite

-- Definição do jogador inicial
initialPlayer :: IO Player
initialPlayer = do
  sprite <- loadPlayerSprite characterScaling -- Load player image with character scaling
  return
    Player
      { playerSprite = sprite,
        playerPosition = (-400, -150) -- Initial player position
      }

-- Definição de tipos de dados para Sky
data Sky = Sky
    { skySprite :: Picture,
      skyPosition :: (Float, Float)
    }

-- Function to load and scale the player's image from BMP file
loadSkySprite :: Float -> IO Picture
loadSkySprite scalingFactor = do
    sprite <- loadBMP "./app/sky/cloud_violet.bmp"
    let scaledSprite = scale scalingFactor scalingFactor sprite
    return scaledSprite

-- Function to load and scale the player's image from BMP file
loadEggSprite :: Float -> IO Picture
loadEggSprite scalingFactor = do
    sprite <- loadBMP "./app/eggs/easter_egg_rabbit.bmp"
    let scaledSprite = scale scalingFactor scalingFactor sprite
    return scaledSprite

-- Function to load and scale the player's image from BMP file
loadCupCakeSprite :: Float -> IO Picture
loadCupCakeSprite scalingFactor = do
    sprite <- loadBMP "./app/clouds/cupcake.bmp"
    let scaledSprite = scale scalingFactor scalingFactor sprite
    return scaledSprite

initialSkyList :: IO [Sky]
initialSkyList = do
    sprite <- loadSkySprite 1
    spriteEgg <- loadEggSprite 0.2
    sprite <- loadSkySprite 1
    spriteCupCake <- loadCupCakeSprite 2
    return
        [ Sky
            { skySprite = sprite,
              skyPosition = (0, 0)
            },
            Sky
            { skySprite = spriteEgg,
              skyPosition = (0, 50)
            }
            ,
            Sky
            { skySprite = spriteCupCake,
              skyPosition = (-300, 100)
            }
        ]

-- Definição de tipos de dados para Wall
data Wall = Wall
  { wallSprite :: Picture,
    wallPosition :: (Float, Float)
  }

-- Function to load and scale the player's image from BMP file
loadWallSprite :: Float -> IO Picture
loadWallSprite scalingFactor = do
  sprite <- loadBMP "./app/wall/choco_wall_big.bmp"
  let scaledSprite = scale scalingFactor scalingFactor sprite
  return scaledSprite

-- Lista inicial de paredes (um exemplo simples com uma única parede no chão)
initialWallList :: IO [Wall]
initialWallList = do
  sprite <- loadWallSprite wallScaling
  return
    [ Wall
        { wallSprite = sprite, -- Exemplo de parede como um retângulo verde escalonada
          wallPosition = (300, 0) -- Posição da parede no chão
        }
        , Wall
        { wallSprite = sprite
        , wallPosition = (-300, -300)
        }
        , Wall
        { wallSprite = sprite
        , wallPosition = (300, -300)
        }
    ]

-- Definição de tipos de dados para Wall
data Tile = Tile
  { tileSprite :: Picture,
    tilePosition :: (Float, Float)
  }

-- Function to load and scale the player's image from BMP file
loadTileSprite :: Float -> IO Picture
loadTileSprite scalingFactor = do
  sprite <- loadBMP "./app/platforms/choco_plat_big.bmp"
  let scaledSprite = scale scalingFactor scalingFactor sprite
  return scaledSprite

loadTileSoilSprite :: Float -> IO Picture
loadTileSoilSprite scalingFactor = do
  sprite <- loadBMP "./app/platforms/choco_soil.bmp"
  let scaledSprite = scale scalingFactor scalingFactor sprite
  return scaledSprite

loadWaterSprite :: Float -> IO Picture
loadWaterSprite scalingFactor = do
  sprite <- loadBMP "./app/water/choco_water.bmp"
  let scaledSprite = scale scalingFactor scalingFactor sprite
  return scaledSprite

-- Lista inicial de chãos
initialTileList :: IO [Tile]
initialTileList = do
  sprite <- loadTileSprite tileScaling
  soilSprite <- loadTileSoilSprite soilScaling
  waterSprite <- loadWaterSprite soilScaling
  return
    [  Tile
        { tileSprite = waterSprite, -- Exemplo de solo
          tilePosition = (0, -300) -- Posição do chao
        },
        Tile
        { tileSprite = waterSprite, -- Exemplo de solo
          tilePosition = (-100, -300) -- Posição do chao
        },
        Tile
        { tileSprite = waterSprite, -- Exemplo de solo
          tilePosition = (100, -300) -- Posição do chao
        },
      Tile
        { tileSprite = soilSprite, -- Exemplo de solo
          tilePosition = (-300, -300) -- Posição do chao
        },
        Tile
        { tileSprite = soilSprite, -- Exemplo de solo
          tilePosition = (300, -300) -- Posição do chao
        },
        Tile
        { tileSprite = sprite,
          tilePosition = (0, -100)
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
    ++ map renderTile (tileList gameState)
        ++  map renderWall (wallList gameState)
        ++ map renderPlayer (playerList gameState)
     
  where
    renderSky sky = uncurry translate (skyPosition sky) $ skySprite sky
    renderWall wall = uncurry translate (wallPosition wall) $ wallSprite wall
    renderTile tile = uncurry translate (tilePosition tile) $ tileSprite tile
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
    (makeColorI 155 212 195 255) -- Cor de fundo (Cornflower Blue)
    60 -- FPS (frames por segundo)
    initialState' -- Estado inicial do jogo
    render -- Função para renderizar o estado do jogo
    handleEvent -- Função para lidar com eventos de entrada
    update -- Função para atualizar o estado do jogo
