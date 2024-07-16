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

-- Constantes de física
gravity :: Float
gravity = 100

-- Velocidade de movimento do jogador, em pixels por frame
playerMovementSpeed :: Float
playerMovementSpeed = 10

-- Velocidade de salto do jogador, em pixels por frame
playerJumpSpeed :: Float
playerJumpSpeed = 40

-- Altura máxima que o jogador pode alcançar durante o pulo
maxJumpHeight :: Float
maxJumpHeight = 200  -- Ajuste conforme necessário

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
    entityPosition :: (Float, Float),
    entitySize :: (Float, Float)
  }

-- Definição do estado do jogo
data GameState = GameState
  { background :: Picture,
    platformList :: [GameEntity],
    tileList :: [GameEntity],
    player :: GameEntity,
    playerVelocity :: (Float, Float),  -- Velocidade do jogador (vx, vy)
    playerJumping :: Bool              -- Indica se o jogador está pulando
  }

-- Estado inicial do jogo
initialState :: IO GameState
initialState = do
  backgroundPicture <- loadBackground
  playerSprite <- loadSprite characterScaling "./app/character/c_right.bmp"
  platformSprite <- loadSprite platformScaling "./app/platforms/platform.bmp"
  tileSprite <- loadSprite tileScaling "./app/tiles/choco_tile.bmp"

  let playerEntity = GameEntity playerSprite (64, 130) (14 * characterScaling, 16 * characterScaling)
      platformEntities = [ GameEntity platformSprite (x * platformScaling, 50) (143 * platformScaling, 47 * platformScaling) | x <- [0, 192 .. 1248]]
      tileEntities = [ GameEntity tileSprite (x * tileScaling, 200) (16 * tileScaling, 13 * tileScaling) | x <- [70, 140 .. 1248]]
      -- tile_x = (platformScaling * 120) - 64
      -- tilePositions = [(tile_x, 150), (tile_x * 2, 150), (tile_x * 3, 150)]
      -- tileEntities = map (\(x, y) -> GameEntity tileSprite (x, y) (16, 13)) tilePositions

  return $
    GameState
      { background = backgroundPicture,
        platformList = platformEntities, 
        tileList = tileEntities,
        player = playerEntity,
        playerVelocity = (0, 0), -- Inicialmente o jogador está parado
        playerJumping = False  -- Inicialmente o jogador não está pulando
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
    map drawEntity (tileList state) ++
    [drawEntity (player state)]

-- Função para lidar com eventos de teclado
handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey key keyState _ _) state =
  case keyState of
    Down ->
      case key of
        SpecialKey KeyUp -> do
          let (vx, vy) = playerVelocity state
          if vy == 0  -- Verifica se não está pulando
            then return $ state { playerVelocity = (vx, playerJumpSpeed) }  -- Inicia o pulo
            else return state  -- Ignora se já estiver pulando
        SpecialKey KeyLeft -> return $ state { playerVelocity = (-playerMovementSpeed, vy) }
        SpecialKey KeyRight -> return $ state { playerVelocity = (playerMovementSpeed, vy) }
        _ -> return state
    Up ->
      case key of
        SpecialKey KeyLeft -> return $ state { playerVelocity = (0, vy) }
        SpecialKey KeyRight -> return $ state { playerVelocity = (0, vy) }
        _ -> return state
  where
    (vx, vy) = playerVelocity state
handleEvent _ state = return state

-- Função para verificar se dois retângulos (jogador e plataforma) estão colidindo
rectanglesColliding :: GameEntity -> GameEntity -> Bool
rectanglesColliding playerEntity platformEntity =
  let (px, py) = entityPosition playerEntity
      (pw, ph) = entitySize playerEntity
      (px', py') = entityPosition platformEntity
      (pw', ph') = entitySize platformEntity
  in px - pw / 2 < px' + pw' / 2 &&
     px + pw / 2 > px' - pw' / 2 &&
     py - ph / 2 < py' + ph' / 2 &&
     py + ph / 2 > py' - ph' / 2

-- Função para atualizar o estado do jogo
updateGame :: Float -> GameState -> IO GameState
updateGame deltaTime state = do
  let (vx, vy) = playerVelocity state
      (px, py) = entityPosition (player state)
      (pw, ph) = entitySize (player state)

      -- Aplica a gravidade se o jogador estiver no ar
      vy' = vy - gravity * deltaTime

      -- Calcula a nova posição do jogador com base na velocidade e no tempo
      newPlayerPosX = px + vx
      newPlayerPosY = py + vy'

      -- Função para verificar colisões horizontais e ajustar posição
      checkHorizontalCollision :: Float -> GameEntity -> Float
      checkHorizontalCollision newX entity =
        let tempPlayer = (player state) { entityPosition = (newX, py) }
        in if rectanglesColliding tempPlayer entity
           then px  -- Se colidir horizontalmente, mantém a posição atual
           else newX  -- Se não colidir, atualiza para a nova posição

      -- Função para verificar colisões verticais e ajustar posição
      checkVerticalCollision :: Float -> GameEntity -> Float
      checkVerticalCollision newY entity =
        let tempPlayer = (player state) { entityPosition = (px, newY) }
        in if rectanglesColliding tempPlayer entity
           then py  -- Se colidir verticalmente, mantém a posição atual
           else newY  -- Se não colidir, atualiza para a nova posição

      -- Verifica colisões horizontais com plataformas e tiles
      finalPosX = foldl checkHorizontalCollision newPlayerPosX (platformList state ++ tileList state)

      -- Verifica colisões verticais com plataformas e tiles
      finalPosY = foldl checkVerticalCollision newPlayerPosY (platformList state ++ tileList state)

      -- Atualiza a velocidade vertical do jogador se estiver no chão
      finalVy = if finalPosY == py && vy' < 0 then 0 else vy'

      updatedPlayer = (player state) { entityPosition = (finalPosX, finalPosY) }

  -- Verifica se o jogador está no chão ou não
  let isOnGround = any (rectanglesColliding updatedPlayer) (platformList state ++ tileList state)
      finalVy' = if isOnGround && vy' < 0 then 0 else finalVy
      finalJumping = not isOnGround

  return $ state { player = updatedPlayer, playerVelocity = (vx, finalVy), playerJumping = finalJumping }


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
    updateGame
