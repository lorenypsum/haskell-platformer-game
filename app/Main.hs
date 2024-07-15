-- Importação dos Módulos
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- Constantes para largura e altura da tela
screenWidth, screenHeight :: Int
screenWidth = 1000
screenHeight = 650

-- Constante para título da tela
screenTitle :: String
screenTitle = "Cueio Lalao"

-- Constantes para escalamento dos sprites
characterScaling, chickenScaling, cloudScaling, eggScaling, mountainScaling, platformScaling, elementsScaling, skyScaling, tileScaling, treeScaling, wallScaling, waterScaling :: Float
characterScaling = 5
chickenScaling = 5
cloudScaling = 2
eggScaling = 0.2
mountainScaling = 1
platformScaling = 4
elementsScaling = 5
skyScaling = 1
tileScaling = 5
treeScaling = 5
wallScaling = 5
waterScaling = 6

-- Função genérica para carregar e escalar sprites a partir de arquivos BMP
loadSprite :: Float -> FilePath -> IO Picture
loadSprite scalingFactor filePath = do
  sprite <- loadBMP filePath
  return $ scale scalingFactor scalingFactor sprite

-- Carregar character sprites
loadPlayerSprites :: IO [Picture]
loadPlayerSprites = mapM (loadSprite characterScaling) 
  [ "./app/character/c_right.bmp"
  , "./app/character/c_walk_right_right_leg.bmp"
  , "./app/character/c_walk_right_left_leg.bmp"
  , "./app/character/c_jump_right.bmp"
  , "./app/character/c_left.bmp"
  , "./app/character/c_jump_left.bmp"
  , "./app/character/c_walk_left_left_leg.bmp"
  , "./app/character/c_walk_left_right_leg.bmp"
  ]

playerPositions :: [(Float, Float)]
playerPositions = [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]

-- Carregar chicken sprites
loadChickenSprites :: IO [Picture]
loadChickenSprites = mapM (loadSprite chickenScaling) 
  [ "./app/chickens/chicken_fly_1.bmp"
  , "./app/chickens/chicken_fly_2.bmp"
  , "./app/chickens/chicken_fly_3.bmp"
  , "./app/chickens/chicken_fly_4.bmp"
  ]

chickenPositions :: [(Float, Float)]
chickenPositions = [(200,200),(200,200),(200,200),(200,200)]

-- Carregar clouds sprites
loadCloudSprites :: IO [Picture]
loadCloudSprites = mapM (loadSprite cloudScaling) 
  [ "./app/clouds/cloud_1.bmp"
  , "./app/clouds/cloud_2.bmp"
  , "./app/clouds/cloud_3.bmp"
  , "./app/clouds/cloud_back.bmp"
  , "./app/clouds/cupcake.bmp"
  , "./app/clouds/props_cloud.bmp"
  , "./app/clouds/yellow_cloud.bmp"
  ]

cloudPositions :: [(Float, Float)]
cloudPositions = [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]

-- Carregar eggs sprites
loadEggSprites :: IO [Picture]
loadEggSprites = mapM (loadSprite eggScaling) 
  [ "./app/eggs/easter_egg_1.bmp"
  , "./app/eggs/easter_egg_2.bmp"
  , "./app/eggs/easter_egg_3.bmp"
  , "./app/eggs/easter_egg_4.bmp"
  , "./app/eggs/easter_egg_5.bmp"
  , "./app/eggs/easter_egg_rabbit.bmp"
  , "./app/eggs/eggs_basket.bmp"
  ]

eggPositions :: [(Float, Float)]
eggPositions = [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]

-- Carregar elements sprites
loadElementSprites :: IO [Picture]
loadElementSprites = mapM (loadSprite elementsScaling) 
  [ "./app/elements/brigadeiro.bmp"
  , "./app/elements/choco_ball.bmp"
  , "./app/elements/choco_donut.bmp"
  , "./app/elements/donut_strawberry.bmp"
  , "./app/elements/milk.bmp"
  ]

elementPositions :: [(Float, Float)]
elementPositions = [(0,0),(0,0),(0,0),(0,0),(0,0)] 

-- Carregar mountains sprites
loadMountainSprites :: IO [Picture]
loadMountainSprites = mapM (loadSprite mountainScaling) 
  [ "./app/mountains/anthill_cake_back_1.bmp"
  , "./app/mountains/anthill_cake_back_2.bmp"
  , "./app/mountains/anthill_cake_back_3.bmp"
  , "./app/mountains/choco_mountain.bmp"
  , "./app/mountains/mountains.bmp"
  ]

mountainPositions :: [(Float, Float)]
mountainPositions = [(0,0),(0,0),(0,0),(0,0),(0,0)]

-- Carregar platforms sprites
loadPlatformSprites :: IO [Picture]
loadPlatformSprites = mapM (loadSprite platformScaling) 
  [ "./app/platforms/choco_plat_big_flat.bmp"
  , "./app/platforms/choco_plat_big.bmp"
  , "./app/platforms/choco_plat_small_flat.bmp"
  , "./app/platforms/choco_plat_small.bmp"
  , "./app/platforms/choco_platform.bmp"
  , "./app/platforms/choco_soil.bmp"
  , "./app/platforms/platform.bmp"
  ]

platformPositions :: [(Float, Float)]
platformPositions = [(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)]

-- Carregar sky sprites
loadSkySprites :: IO [Picture]
loadSkySprites = mapM (loadSprite skyScaling) 
  [ "./app/sky/cloud_sky.bmp"
  , "./app/sky/cotton_cloud_back.bmp"
  , "./app/sky/cotton_cloud_front.bmp"
  , "./app/sky/cotton_cloud_middle.bmp"
  ]

skyPositions :: [(Float, Float)]
skyPositions = [(0,0),(0,0),(0,0),(0,0)]

-- Carregar spikes sprites
loadSpikeSprites :: IO [Picture]
loadSpikeSprites = mapM (loadSprite wallScaling) 
  [ "./app/spikes/choco_spike_2.bmp"
  , "./app/spikes/choco_spike.bmp"
  ]

spikePositions :: [(Float, Float)]
spikePositions = [(0,0),(0,0)]

-- Carregar tiles sprites
loadTileSprites :: IO [Picture]
loadTileSprites = mapM (loadSprite tileScaling) 
  [ "./app/tiles/choco_small_flat_platform.bmp"
  , "./app/tiles/choco_tile.bmp"
  , "./app/tiles/cookie_tile.bmp"
  , "./app/tiles/strawberry_bar.bmp"
  , "./app/tiles/choco_bar.bmp"
  ]

tilePositions :: [(Float, Float)]
tilePositions = [(0,0),(0,0),(0,0),(0,0),(0,0)]

-- Carregar trees sprites
loadTreeSprites :: IO [Picture]
loadTreeSprites = mapM (loadSprite treeScaling) 
  [ "./app/trees/choco_lollipop.bmp"
  , "./app/trees/choco_tree_2.bmp"
  , "./app/trees/choco_tree_3.bmp"
  , "./app/trees/choco_tree.bmp"
  , "./app/trees/sakura_mochi.bmp"
  ]

treePositions :: [(Float, Float)]
treePositions = [(0,0),(0,0),(0,0),(0,0),(0,0)]

-- Carregar walls sprites
loadWallSprites :: IO [Picture]
loadWallSprites = mapM (loadSprite wallScaling) 
  [ "./app/walls/choco_wall_big.bmp"
  , "./app/walls/choco_wall_small.bmp"
  ]

wallPositions :: [(Float, Float)]
wallPositions = [(0,0),(0,0)]  

-- Carregar water sprites
loadWaterSprites :: IO [Picture]
loadWaterSprites = mapM (loadSprite waterScaling) 
  [ "./app/water/choco_water.bmp"
  ]

waterPositions :: [(Float, Float)]
waterPositions = [(0,0)]

-- Definição de tipos de dados para os elementos do jogo
data GameEntity = GameEntity
  { entitySprite :: Picture
  , entityPosition :: (Float, Float)
  }

-- Definição do estado do jogo
data GameState = GameState
  { playerList :: [GameEntity]
  , chickenList :: [GameEntity]
  , cloudList :: [GameEntity]
  , eggList :: [GameEntity]
  , elementList :: [GameEntity]
  , mountainList :: [GameEntity]
  , platformList :: [GameEntity]
  , skyList :: [GameEntity]
  , spikeList :: [GameEntity]
  , tileList :: [GameEntity]
  , treeList :: [GameEntity]
  , wallList :: [GameEntity]
  , waterList :: [GameEntity]
  }

-- Estado inicial do jogo
initialState :: IO GameState
initialState = do
  playerSprites <- loadPlayerSprites
  chickenSprites <- loadChickenSprites
  cloudSprites <- loadCloudSprites
  eggSprites <- loadEggSprites
  elementSprites <- loadElementSprites
  mountainSprites <- loadMountainSprites
  platformSprites <- loadPlatformSprites
  skySprites <- loadSkySprites
  spikeSprites <- loadSpikeSprites
  tileSprites <- loadTileSprites
  treeSprites <- loadTreeSprites
  wallSprites <- loadWallSprites
  waterSprites <- loadWaterSprites


  return $ GameState
    { playerList = [GameEntity (head playerSprites) (head playerPositions)]
    , chickenList = [GameEntity (head chickenSprites) (head chickenPositions)]
    , cloudList = zipWith (\sprite pos -> GameEntity sprite pos) cloudSprites cloudPositions
    , eggList = zipWith (\sprite pos -> GameEntity sprite pos) eggSprites eggPositions
    , elementList = zipWith (\sprite pos -> GameEntity sprite pos) elementSprites elementPositions
    , mountainList = zipWith (\sprite pos -> GameEntity sprite pos) mountainSprites mountainPositions
    , platformList = zipWith (\sprite pos -> GameEntity sprite pos) platformSprites platformPositions
    , skyList = zipWith (\sprite pos -> GameEntity sprite pos) skySprites skyPositions
    , spikeList = zipWith (\sprite pos -> GameEntity sprite pos) spikeSprites spikePositions
    , tileList = zipWith (\sprite pos -> GameEntity sprite pos) tileSprites tilePositions
    , treeList = zipWith (\sprite pos -> GameEntity sprite pos) treeSprites treePositions
    , wallList = zipWith (\sprite pos -> GameEntity sprite pos) wallSprites wallPositions
    , waterList = zipWith (\sprite pos -> GameEntity sprite pos) waterSprites waterPositions
    }

-- Função para desenhar uma entidade de jogo na tela
drawEntity :: GameEntity -> Picture
drawEntity (GameEntity sprite (x, y)) = translate x y sprite

-- Função para renderizar o estado do jogo
render :: GameState -> Picture
render state = pictures $ concat
  [ map drawEntity (playerList state)
  , map drawEntity (chickenList state)
  , map drawEntity (cloudList state)
  , map drawEntity (eggList state)
  , map drawEntity (elementList state)
  , map drawEntity (mountainList state)
  , map drawEntity (platformList state)
  , map drawEntity (skyList state)
  , map drawEntity (spikeList state)
  , map drawEntity (tileList state)
  , map drawEntity (treeList state)
  , map drawEntity (wallList state)
  , map drawEntity (waterList state)
  ]

-- Função principal do jogo
main :: IO ()
main = do
  initialState' <- initialState
  play
    (InWindow screenTitle (screenWidth, screenHeight) (10, 10))
    white
    60
    initialState'
    render
    (\_ world -> world)
    (\_ world -> world)
