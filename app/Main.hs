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
characterScaling, chickenScaling, cloudScaling, eggScaling, mountainScaling, platformScaling, elementsScaling, skyScaling, tileScaling, treeScaling, wallScaling, waterScaling:: Float
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
loadPlayerJumpLeft, loadPlayerJumpRight, loadPlayerLeft, loadPlayerRight, loadPlayerWalkLeftLeftLeg, loadPlayerWalkLeftRightLeg, loadPlayerWalkRightLeftLeg, loadPlayerWalkRightRightLeg :: IO Picture
loadPlayerJumpLeft = loadSprite characterScaling "./app/character/c_jump_left.bmp"
loadPlayerJumpRight = loadSprite characterScaling "./app/character/c_jump_right.bmp"
loadPlayerLeft = loadSprite characterScaling "./app/character/c_left.bmp"
loadPlayerRight = loadSprite characterScaling "./app/character/c_right.bmp"
loadPlayerWalkLeftLeftLeg = loadSprite characterScaling "./app/character/c_walk_left_left_leg.bmp"
loadPlayerWalkLeftRightLeg = loadSprite characterScaling "./app/character/c_walk_left_right_leg.bmp"
loadPlayerWalkRightRightLeg = loadSprite characterScaling "./app/character/c_walk_right_right_leg.bmp"
loadPlayerWalkRightLeftLeg = loadSprite characterScaling "./app/character/c_walk_right_left_leg.bmp"

-- Carregar chicken sprites
loadChickenFly1, loadChickenFly2, loadChickenFly3, loadChickenFly4 :: IO Picture
loadChickenFly1 = loadSprite chickenScaling "./app/chickens/chicken_fly_1.bmp"
loadChickenFly2 = loadSprite chickenScaling "./app/chickens/chicken_fly_2.bmp"
loadChickenFly3 = loadSprite chickenScaling "./app/chickens/chicken_fly_3.bmp"
loadChickenFly4 = loadSprite chickenScaling "./app/chickens/chicken_fly_4.bmp"

-- Carregar clouds sprites
loadCloud1, loadCloud2, loadCloud3, loadClousBack, loadCupcake, loadPropsCloud, loadYellowCloud :: IO Picture
loadCloud1 = loadSprite cloudScaling "./app/clouds/cloud_1.bmp"
loadCloud2 = loadSprite cloudScaling "./app/clouds/cloud_2.bmp"
loadCloud3 = loadSprite cloudScaling "./app/clouds/cloud_3.bmp"
loadClousBack = loadSprite cloudScaling "./app/clouds/cloud_back.bmp"
loadCupcake = loadSprite cloudScaling "./app/clouds/cupcake.bmp"
loadPropsCloud = loadSprite cloudScaling "./app/clouds/props_cloud.bmp"
loadYellowCloud = loadSprite cloudScaling "./app/clouds/yellow_cloud.bmp"

-- Carregar eggs sprites
loadEasterEgg1, loadEasterEgg2, loadEasterEgg3, loadEasterEgg4, loadEasterEgg5, loadEasterEggRabbit, loadEggsBasket :: IO Picture
loadEasterEgg1 = loadSprite eggScaling "./app/eggs/easter_egg_1.bmp"
loadEasterEgg2 = loadSprite eggScaling "./app/eggs/easter_egg_2.bmp"
loadEasterEgg3 = loadSprite eggScaling "./app/eggs/easter_egg_3.bmp"
loadEasterEgg4 = loadSprite eggScaling "./app/eggs/easter_egg_4.bmp"
loadEasterEgg5 = loadSprite eggScaling "./app/eggs/easter_egg_5.bmp"
loadEasterEggRabbit = loadSprite eggScaling "./app/eggs/easter_egg_rabbit.bmp"
loadEggsBasket = loadSprite eggScaling "./app/eggs/eggs_basket.bmp"

-- Carregar elements sprites
loadBrigadeiro, loadChocoBall, loadChocoDonut, loadDonutStrawberry, loadMilk :: IO Picture
loadBrigadeiro = loadSprite elementsScaling "./app/elements/brigadeiro.bmp"
loadChocoBall = loadSprite elementsScaling "./app/elements/choco_ball.bmp"
loadChocoDonut = loadSprite elementsScaling "./app/elements/choco_donut.bmp"
loadDonutStrawberry = loadSprite elementsScaling "./app/elements/donut_strawberry.bmp"
loadMilk = loadSprite elementsScaling "./app/elements/milk.bmp"

-- Carregar mountains sprites
loadAnthillCakeBack1, loadAnthillCakeBack2, loadAnthillCakeBack3, loadChocoMountain, loadMountains :: IO Picture
loadAnthillCakeBack1 = loadSprite mountainScaling "./app/mountains/anthill_cake_back_1.bmp"
loadAnthillCakeBack2 = loadSprite mountainScaling "./app/mountains/anthill_cake_back_2.bmp"
loadAnthillCakeBack3 = loadSprite mountainScaling "./app/mountains/anthill_cake_back_3.bmp"
loadChocoMountain = loadSprite 5 "./app/mountains/choco_mountain.bmp"
loadMountains = loadSprite mountainScaling "./app/mountains/mountains.bmp"

-- Carregar platforms sprites
loadChocoPlatBigFlat, loadChocoPlatBig, loadChocoPlatSmallFlat, loadChocoPlatSmall, loadChocoPlatform, loadChocoSoil, loadPlatform :: IO Picture
loadChocoPlatBigFlat = loadSprite platformScaling "./app/platforms/choco_plat_big_flat.bmp"
loadChocoPlatBig = loadSprite platformScaling "./app/platforms/choco_plat_big.bmp"
loadChocoPlatSmallFlat = loadSprite platformScaling "./app/platforms/choco_plat_small_flat.bmp"
loadChocoPlatSmall = loadSprite platformScaling "./app/platforms/choco_plat_small.bmp"
loadChocoPlatform = loadSprite platformScaling "./app/platforms/choco_platform.bmp"
loadChocoSoil = loadSprite platformScaling "./app/platforms/choco_soil.bmp"
loadPlatform = loadSprite platformScaling "./app/platforms/platform.bmp"

-- Carregar sky sprites
loadCloudSky, loadCottonCloudBack, loadCottonCloudFront, loadCottonCloudMiddle, loadYellowBG :: IO Picture
loadCloudSky = loadSprite skyScaling "./app/sky/cloud_sky.bmp"
loadCottonCloudBack = loadSprite skyScaling "./app/sky/cotton_cloud_back.bmp"
loadCottonCloudFront = loadSprite skyScaling "./app/sky/cotton_cloud_front.bmp"
loadCottonCloudMiddle = loadSprite skyScaling "./app/sky/cotton_cloud_middle.bmp"
loadYellowBG = loadSprite skyScaling "./app/sky/yellow_bg.bmp"

-- Carregar spikes sprites
loadChocoSpike2, loadChocoSpike :: IO Picture
loadChocoSpike2 = loadSprite wallScaling "./app/spikes/choco_spike_2.bmp"
loadChocoSpike = loadSprite wallScaling "./app/spikes/choco_spike.bmp"

-- Carregar tiles sprites
loadChocoSmallFlatPlatform, loadChocoTile, loadCookieTile, loadStrawberryBar, loadChocoBar :: IO Picture
loadChocoSmallFlatPlatform = loadSprite tileScaling "./app/tiles/choco_small_flat_platform.bmp"
loadChocoTile = loadSprite tileScaling "./app/tiles/choco_tile.bmp"
loadCookieTile = loadSprite tileScaling "./app/tiles/cookie_tile.bmp"
loadStrawberryBar = loadSprite tileScaling "./app/tiles/strawberry_bar.bmp"
loadChocoBar = loadSprite tileScaling "./app/tiles/choco_bar.bmp"

-- Carregar trees sprites
loadChocoLollipop, loadChocoTree2, loadChocoTree3, loadChocoTree, loadSakuraMochi  :: IO Picture
loadChocoLollipop = loadSprite treeScaling "./app/trees/choco_lollipop.bmp"
loadChocoTree2 = loadSprite treeScaling "./app/trees/choco_tree_2.bmp"
loadChocoTree3 = loadSprite treeScaling "./app/trees/choco_tree_3.bmp"
loadChocoTree = loadSprite treeScaling "./app/trees/choco_tree.bmp"
loadSakuraMochi = loadSprite treeScaling "./app/trees/sakura_mochi.bmp"

-- Carregar walls sprites
loadChocoWallBig, loadChocoWallSmall :: IO Picture
loadChocoWallBig = loadSprite wallScaling "./app/walls/choco_wall_big.bmp"
loadChocoWallSmall = loadSprite wallScaling "./app/walls/choco_wall_small.bmp"

-- Carregar water sprites
loadChocoWater:: IO Picture
loadChocoWater = loadSprite waterScaling "./app/water/choco_water.bmp"

-- Definição de tipos de dados para o Player
data Player = Player
  { playerSprite :: Picture,
    playerPosition :: (Float, Float)
  }

-- Definição de tipos de dados para Chickens
data Chicken = Chicken
  { chickenSprite :: Picture,
    chickenPosition :: (Float, Float)
  }

-- Definição de tipos de dados para Clouds
data Cloud = Cloud
  { cloudSprite :: Picture,
    cloudPosition :: (Float, Float)
  }

-- Definição de tipos de dados para Eggs
data Egg = Egg
  { eggSprite :: Picture,
    eggPosition :: (Float, Float)
  }

-- Definição de tipos de dados para Elements
data Element = Element
  { elementSprite :: Picture,
    elementPosition :: (Float, Float)
  }

-- Definição de tipos de dados para Mountains
data Mountain = Mountain
  { mountainSprite :: Picture,
    mountainPosition :: (Float, Float)
  }

-- Definição de tipos de dados para Platforms
data Platform = Platform
  { platformSprite :: Picture,
    platformPosition :: (Float, Float)
  }

-- Definição de tipos de dados para Sky
data Sky = Sky
  { skySprite :: Picture,
    skyPosition :: (Float, Float)
  }

-- Definição de tipos de dados para Spikes
data Spike = Spike
  { spikeSprite :: Picture,
    spikePosition :: (Float, Float)
  }

-- Definição de tipos de dados para Tiles
data Tile = Tile
  { tileSprite :: Picture,
    tilePosition :: (Float, Float)
  }

-- Definição de tipos de dados para Trees
data Tree = Tree
  { treeSprite :: Picture,
    treePosition :: (Float, Float)
  }

-- Definição de tipos de dados para Walls
data Wall = Wall
  { wallSprite :: Picture,
    wallPosition :: (Float, Float)
  }

-- Definição de tipos de dados para Walls
data Water = Water
  { waterSprite :: Picture,
    waterPosition :: (Float, Float)
  }

-- Definição do Player inicial
initialPlayerList :: IO [Player]
initialPlayerList = do
  -- spritePlayerLeft <- loadPlayerLeft
  spritePlayerRight <- loadPlayerRight
  -- spritePlayerWalkLeftLeftLeg <- loadPlayerWalkLeftLeftLeg
  -- spritePlayerWalkLeftRightLeg <- loadPlayerWalkLeftRightLeg
  return
    [ Player {playerSprite = spritePlayerRight, playerPosition = (-400, -130)}
      -- Player {playerSprite = spritePlayerWalkLeftLeftLeg, playerPosition = (-300, -150)},
      -- Player {playerSprite = spritePlayerWalkLeftRightLeg, playerPosition = (-200, -150)}
    ]

-- Definição do Chicken inicial
initialChickenList :: IO [Chicken]
initialChickenList = do
  spriteChickenFly1 <- loadChickenFly1
  -- spriteChickenFly2 <- loadChickenFly2
  -- spriteChickenFly3 <- loadChickenFly3
  -- spriteChickenFly4 <- loadChickenFly4
  return
    [ Chicken {chickenSprite = spriteChickenFly1, chickenPosition = (-200, 100)}
      -- Chicken {chickenSprite = spriteChickenFly2, chickenPosition = (-400, 0)},
      -- Chicken {chickenSprite = spriteChickenFly3, chickenPosition = (400, 0)},
      -- Chicken {chickenSprite = spriteChickenFly4, chickenPosition = (-400, 0)}
    ]

-- Definição do Cloud inicial
initialCloudList :: IO [Cloud]
initialCloudList = do
  spriteCloud1 <- loadCloud1
  spriteCloud2 <- loadCloud2
  -- spriteCloud3 <- loadCloud3
  -- spriteClousBack <- loadClousBack
  -- spriteCupcake <- loadCupcake
  -- spritePropsCloud <- loadPropsCloud
  -- spriteYellowCloud <- loadYellowCloud
  return
    [ Cloud {cloudSprite = spriteCloud1, cloudPosition = (-250, 250)},
      Cloud {cloudSprite = spriteCloud2, cloudPosition = (200, 250)}
      -- Cloud {cloudSprite = spriteCloud3, cloudPosition = (200, 300)},
      -- Cloud {cloudSprite = spriteClousBack, cloudPosition = (300, 350)},
      -- Cloud {cloudSprite = spriteCupcake, cloudPosition = (400, 400)},
      -- Cloud {cloudSprite = spritePropsCloud, cloudPosition = (500, 450)},
      -- Cloud {cloudSprite = spriteYellowCloud, cloudPosition = (600, 500)}
    ]

-- Definição do Egg inicial
initialEggList :: IO [Egg]
initialEggList = do
  spriteEasterEgg1 <- loadEasterEgg1
  -- spriteEasterEgg2 <- loadEasterEgg2
  -- spriteEasterEgg3 <- loadEasterEgg3
  -- spriteEasterEgg4 <- loadEasterEgg4
  -- spriteEasterEgg5 <- loadEasterEgg5
  -- spriteEasterEggRabbit <- loadEasterEggRabbit
  -- spriteEggsBasket <- loadEggsBasket
  return
    [ Egg {eggSprite = spriteEasterEgg1, eggPosition = (0, 150)}
      -- Egg {eggSprite = spriteEasterEgg2, eggPosition = (50, 50)},
      -- Egg {eggSprite = spriteEasterEgg3, eggPosition = (100, 100)},
      -- Egg {eggSprite = spriteEasterEgg4, eggPosition = (150, 150)},
      -- Egg {eggSprite = spriteEasterEgg5, eggPosition = (200, 200)},
      -- Egg {eggSprite = spriteEasterEggRabbit, eggPosition = (250, 250)},
      -- Egg {eggSprite = spriteEggsBasket, eggPosition = (300, 300)}
    ]

-- Definição do Element inicial
initialElementList :: IO [Element]
initialElementList = do
  -- spriteBrigadeiro <- loadBrigadeiro
  spriteChocoBall <- loadChocoBall
  -- spriteChocoDonut <- loadChocoDonut
  -- spriteDonutStrawberry <- loadDonutStrawberry
  -- spriteMilk <- loadMilk
  return
    [ -- Element {elementSprite = spriteBrigadeiro, elementPosition = (-200, -200)},
      Element {elementSprite = spriteChocoBall, elementPosition = (-250, -150)}
      -- Element {elementSprite = spriteChocoDonut, elementPosition = (100, 100)},
      -- Element {elementSprite = spriteDonutStrawberry, elementPosition = (150, 150)},
      -- Element {elementSprite = spriteMilk, elementPosition = (200, 200)}
    ]

-- Definição do Mountain inicial
initialMountainList :: IO [Mountain]
initialMountainList = do
  -- spriteAnthillCakeBack1 <- loadAnthillCakeBack1
  -- spriteAnthillCakeBack2 <- loadAnthillCakeBack2
  -- spriteAnthillCakeBack3 <- loadAnthillCakeBack3
  spriteMountains <- loadMountains
  -- spriteChocoMountain <- loadChocoMountain
  return
    [ 
      Mountain {mountainSprite = spriteMountains, mountainPosition = (0, 100)}
      -- Mountain {mountainSprite = spriteAnthillCakeBack1, mountainPosition = (-300, 0)},
      -- Mountain {mountainSprite = spriteAnthillCakeBack2, mountainPosition = (0, 0)},
      -- Mountain {mountainSprite = spriteAnthillCakeBack3, mountainPosition = (300, 0)}
      -- Mountain {mountainSprite = spriteChocoMountain, mountainPosition = (-300, 100)},
      -- Mountain {mountainSprite = spriteChocoMountain, mountainPosition = (0, 100)},
      -- Mountain {mountainSprite = spriteChocoMountain, mountainPosition = (300, 100)}
    ]

-- Definição do Platform inicial
initialPlatformList :: IO [Platform]
initialPlatformList = do
  -- spriteChocoPlatBigFlat <- loadChocoPlatBigFlat
  -- spriteChocoPlatBig <- loadChocoPlatBig
  -- spriteChocoPlatSmall <- loadChocoPlatSmall
  -- spriteChocoSoil <- loadChocoSoil
  -- spriteChocoPlatform <- loadChocoPlatform
  spritePlatform <- loadPlatform
  return
    [ -- Platform {platformSprite = spriteChocoPlatBigFlat, platformPosition = (0, 0)},
      -- Platform {platformSprite = spriteChocoPlatBig, platformPosition = (50, 50)},
      -- Platform {platformSprite = spriteChocoPlatSmall, platformPosition = (150, 150)},
      -- Platform {platformSprite = spriteChocoPlatform, platformPosition = (200, 200)},
      -- Platform {platformSprite = spriteChocoSoil, platformPosition = (250, 250)},
      Platform {platformSprite = spritePlatform, platformPosition = (-400, -250)},
      Platform {platformSprite = spritePlatform, platformPosition = (400, -250)}
    ]

-- Definição do Sky inicial
initialSkyList :: IO [Sky]
initialSkyList = do
  -- spriteCloudSky <- loadCloudSky
  spriteCottonCloudBack <- loadCottonCloudBack
  spriteCottonCloudFront <- loadCottonCloudFront
  spriteCottonCloudMiddle <- loadCottonCloudMiddle
  return
    [ -- Sky {skySprite = spriteCloudSky, skyPosition = (0, 200)},
      Sky {skySprite = spriteCottonCloudBack, skyPosition = (0, 0)},
      Sky {skySprite = spriteCottonCloudMiddle, skyPosition = (0, -100)},
      Sky {skySprite = spriteCottonCloudFront, skyPosition = (0, -200)}
    ]

-- Definição do Spike inicial
initialSpikeList :: IO [Spike]
initialSpikeList = do
  spriteChocoSpike2 <- loadChocoSpike2
  spriteChocoSpike <- loadChocoSpike
  return
    [ Spike {spikeSprite = spriteChocoSpike2, spikePosition = (400, -125)},
      Spike {spikeSprite = spriteChocoSpike, spikePosition = (300, 70)}
    ]

-- Definição do Tile inicial
initialTileList :: IO [Tile]
initialTileList = do
  spriteSmallFlatPlatform <- loadChocoSmallFlatPlatform
  -- spriteChocoTile <- loadChocoTile
  -- spriteCookieTile <- loadCookieTile
  -- spriteStrawberryBar <- loadStrawberryBar
  spriteChocoBar <- loadChocoBar
  return
    [ Tile {tileSprite = spriteSmallFlatPlatform, tilePosition = (0, 0)},
      -- Tile {tileSprite = spriteChocoTile, tilePosition = (50, 50)},
      -- Tile {tileSprite = spriteCookieTile, tilePosition = (100, 100)},
      -- Tile {tileSprite = spriteStrawberryBar, tilePosition = (-200, 0)},
      Tile {tileSprite = spriteChocoBar, tilePosition = (300, 150)}
    ]

-- Definição do Tree inicial
initialTreeList :: IO [Tree]
initialTreeList = do
  spriteChocoLollipop <- loadChocoLollipop
  -- spriteChocoTree2 <- loadChocoTree2
  -- spriteChocoTree3 <- loadChocoTree3
  -- spriteChocoTree <- loadChocoTree
  -- spriteSakuraMochi <- loadSakuraMochi
  return
    [ Tree {treeSprite = spriteChocoLollipop, treePosition = (-200, -90)}
      -- Tree {treeSprite = spriteChocoTree2, treePosition = (-150, -90)}
      -- Tree {treeSprite = spriteChocoTree3, treePosition = (100, 100)},
      -- Tree {treeSprite = spriteChocoTree, treePosition = (150, 150)},
      -- Tree {treeSprite = spriteSakuraMochi, treePosition = (200, 200)}
    ]

-- Definição do Wall inicial
initialWallList :: IO [Wall]
initialWallList = do
  spriteChocoWallBig <- loadChocoWallBig
  -- spriteChocoWallSmall <- loadChocoWallSmall
  return
    [ Wall {wallSprite = spriteChocoWallBig, wallPosition = (300, -70)}
      -- Wall {wallSprite = spriteChocoWallSmall, wallPosition = (50, 50)}
    ]

-- Definição do Water inicial
initialWaterList :: IO [Water]
initialWaterList = do
  spriteChocoWater <- loadChocoWater
  return
    [ Water {waterSprite = spriteChocoWater, waterPosition = (0, -270)}
    ]

-- Definição de tipos de dados para o estado do jogo
data GameState = GameState
  {
    playerList :: [Player],
    chickenList :: [Chicken],
    cloudList :: [Cloud],
    eggList :: [Egg],
    elementList :: [Element],
    mountainList :: [Mountain],
    platformList :: [Platform],
    skyList :: [Sky],
    spikeList :: [Spike],
    tileList :: [Tile],
    treeList :: [Tree],
    wallList :: [Wall],
    waterList :: [Water]
  }

-- Estado inicial do jogo
initialState :: IO GameState
initialState = do
  player <- initialPlayerList
  chickens <- initialChickenList
  clouds <- initialCloudList
  eggs <- initialEggList
  elements <- initialElementList
  mountains <- initialMountainList
  platforms <- initialPlatformList
  sky <- initialSkyList
  spikes <- initialSpikeList
  tiles <- initialTileList
  trees <- initialTreeList
  walls <- initialWallList
  water <- initialWaterList
  return
    GameState
      {
        playerList = player,
        chickenList = chickens,
        cloudList = clouds,
        eggList = eggs,
        elementList = elements,
        mountainList = mountains,
        platformList = platforms,
        skyList = sky,
        spikeList = spikes,
        tileList = tiles,
        treeList = trees,
        wallList = walls,
        waterList = water
      }

-- Função para renderizar o estado do jogo
render :: GameState -> IO Picture
render gameState =
  return $
    pictures $
      concat
        [ map renderMountain (mountainList gameState),
          map renderSky (skyList gameState),
          map renderCloud (cloudList gameState),
          map renderWater (waterList gameState),
          map renderWall (wallList gameState),
          map renderPlatform (platformList gameState),
          map renderTile (tileList gameState),
          map renderElement (elementList gameState),
          map renderTree (treeList gameState),
          map renderSpike (spikeList gameState),
          map renderEggs (eggList gameState),
          map renderChicken (chickenList gameState),
          map renderPlayer (playerList gameState)          
        ]
  where
    renderSky sky = uncurry translate (skyPosition sky) $ skySprite sky
    renderMountain mountain = uncurry translate (mountainPosition mountain) $ mountainSprite mountain
    renderCloud cloud = uncurry translate (cloudPosition cloud) $ cloudSprite cloud
    renderWater water = uncurry translate (waterPosition water) $ waterSprite water
    renderWall wall = uncurry translate (wallPosition wall) $ wallSprite wall
    renderPlatform platform = uncurry translate (platformPosition platform) $ platformSprite platform
    renderTile tile = uncurry translate (tilePosition tile) $ tileSprite tile
    renderElement element = uncurry translate (elementPosition element) $ elementSprite element
    renderTree tree = uncurry translate (treePosition tree) $ treeSprite tree
    renderSpike spike = uncurry translate (spikePosition spike) $ spikeSprite spike
    renderEggs egg = uncurry translate (eggPosition egg) $ eggSprite egg
    renderChicken chicken = uncurry translate (chickenPosition chicken) $ chickenSprite chicken
    renderPlayer player = uncurry translate (playerPosition player) $ playerSprite player

-- Função para lidar com eventos de entrada
handleEvent :: Event -> GameState -> IO GameState
handleEvent _ = return -- Por enquanto, não tratamos eventos

-- Função para atualizar o estado do jogo
update :: Float -> GameState -> IO GameState
update _ = return -- Por enquanto, não atualizamos o estado

-- Função principal para iniciar o jogo
main :: IO ()
main = do
  initialState' <- initialState
  playIO
    (InWindow screenTitle (screenWidth, screenHeight) (100, 100)) -- Configurações da janela
    (makeColorI 185 213 188 255) -- Cor de fundo (Cornflower Blue)
    60 -- FPS (frames por segundo)
    initialState' -- Estado inicial do jogo
    render -- Função para renderizar o estado do jogo
    handleEvent -- Função para lidar com eventos de entrada
    update -- Função para atualizar o estado do jogo
