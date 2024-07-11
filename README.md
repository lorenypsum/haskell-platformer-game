# haskell-platformer-game
Haskell platformer game based on:
https://api.arcade.academy/en/latest/examples/platform_tutorial/index.html
from Python Arcade 2.6.17 Tutorial.

## Installation

1. Install System Dependencies
```bash
sudo apt-get install libgl1-mesa-dev
```
```bash
sudo apt-get install libglu1-mesa-dev
```

2. Reinstall GLUT

```bash
sudo apt-get remove --purge freeglut3-dev
sudo apt-get install freeglut3-dev
```

3. Install gloss library 
```bash 
cabal install gloss
```

4. Add dependencies to your project, if not found in the project's cabal file already.

    Your [file.cabal](./haskell-platformer-game.cabal) should look like this:
```cabal
build-depends:
    base >= 4.14 && < 5,
    gloss >= 1.13 && < 1.14,
    GLUT >= 2.7 && < 2.8
```

5. Clean the dependencies and build the project
```bash 
    cabal clean
    cabal update
    cabal build
```
6. Execute the project
```bash 
    cabal run
```

	
