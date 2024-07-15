-- Importação dos Módulos
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Exception (handle)

-- Constantes para largura e altura da tela
screenWidth, screenHeight :: Int
screenWidth = 1000
screenHeight = 650

-- Constante para título da tela
screenTitle :: String
screenTitle = "Platformer"

-- Constantes para escalamento dos sprites
characterScaling, platformScaling, tileScaling :: Float
characterScaling = 3
platformScaling = 2.5
tileScaling = 2.5

-- Velocidade de movimento do jogador, em pixels por frame
playerMovementSpeed :: Float
playerMovementSpeed = 5

-- Função para carregar e escalar sprites a partir de arquivos BMP
loadSprite :: Float -> FilePath -> IO Picture
loadSprite scalingFactor filePath = do
  sprite <- loadBMP filePath
  return $ scale scalingFactor scalingFactor sprite

-- Função para carregar e escalar o background
loadBackground :: IO Picture
loadBackground = do
  bg <- loadBMP "./app/background/background.bmp"
  let bmpWidth = 797 -- largura da imagem de background
      bmpHeight = 510 -- altura da imagem de background
      scaleX = fromIntegral screenWidth / bmpWidth
      scaleY = fromIntegral screenHeight / bmpHeight
  return $ scale scaleX scaleY bg


-- Definição de tipos de dados para os elementos do jogo
data GameEntity = GameEntity
  { entitySprite :: Picture,
    entityPosition :: (Float, Float)
  }

-- Definição do estado do jogo
data GameState = GameState
  { background :: Picture,
    platformList :: [GameEntity],
    player :: GameEntity,
    playerVelocity :: (Float, Float)  -- Velocidade do jogador (vx, vy)
  }

-- Estado inicial do jogo
initialState :: IO GameState
initialState = do
  backgroundPicture <- loadBackground
  playerSprite <- loadSprite characterScaling "./app/character/c_right.bmp"
  platformSprite <- loadSprite platformScaling "./app/platforms/platform.bmp"
  tileSprite <- loadSprite tileScaling "./app/tiles/choco_tile.bmp"

  let playerEntity = GameEntity playerSprite (64, 130)
      platformEntities = [ GameEntity platformSprite (x * platformScaling, 50) | x <- [0, 192 .. 1248] ]
      tile_x = (platformScaling * 120) - 64
      tilePositions = [(tile_x, 150), (tile_x * 2, 150), (tile_x * 3, 150)]
      tileEntities = map (GameEntity tileSprite) tilePositions

  return $
    GameState
      { background = backgroundPicture,
        platformList = platformEntities ++ tileEntities,
        player = playerEntity,
        playerVelocity = (0, 0)  -- Inicialmente o jogador está parado
      }

-- Função para transformar coordenadas para o sistema com origem no canto inferior esquerdo
toBottomLeftCoords :: (Float, Float) -> (Float, Float)
toBottomLeftCoords (x, y) = (x - fromIntegral screenWidth / 2, y - fromIntegral screenHeight / 2)

-- Função para desenhar uma entidade de jogo na tela
drawEntity :: GameEntity -> Picture
drawEntity entity = uncurry translate (toBottomLeftCoords (entityPosition entity)) (entitySprite entity)

-- Função para renderizar o estado do jogo
render :: GameState -> Picture
render state =
  pictures $
    [background state] ++
    map drawEntity (platformList state) ++
    [drawEntity (player state)]

-- Função para lidar com eventos de teclado
handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey key keyState _ _) state =
  case keyState of
    Down ->
      case key of
        SpecialKey KeyUp -> return $ state { playerVelocity = (vx, playerMovementSpeed) }
        SpecialKey KeyDown -> return $ state { playerVelocity = (vx, -playerMovementSpeed) }
        SpecialKey KeyLeft -> return $ state { playerVelocity = (-playerMovementSpeed, vy) }
        SpecialKey KeyRight -> return $ state { playerVelocity = (playerMovementSpeed, vy) }
        _ -> return state
    Up ->
      case key of
        SpecialKey KeyUp -> return $ state { playerVelocity = (vx, 0) }
        SpecialKey KeyDown -> return $ state { playerVelocity = (vx, 0) }
        SpecialKey KeyLeft -> return $ state { playerVelocity = (0, vy) }
        SpecialKey KeyRight -> return $ state { playerVelocity = (0, vy) }
        _ -> return state
  where
    (vx, vy) = playerVelocity state
handleEvent _ state = return state

-- Função para atualizar o estado do jogo
update :: Float -> GameState -> IO GameState
update _deltaTime state = do
  let (vx, vy) = playerVelocity state
      (px, py) = entityPosition (player state)
      newPlayerPos = (px + vx, py + vy)
      updatedPlayer = (player state) { entityPosition = newPlayerPos }
  return $ state { player = updatedPlayer }

-- Função principal do jogo
main :: IO ()
main = do
  initialState' <- initialState
  playIO
    (InWindow screenTitle (screenWidth, screenHeight) (10, 10))
    (makeColorI 155 212 195 255)
    60
    initialState'
    (return . render)
    handleEvent
    update
