# haskell-platformer-game
Haskell platformer game based on:
https://api.arcade.academy/en/latest/examples/platform_tutorial/index.html
from Python Arcade 2.6.17 Tutorial.

## Installation

1. Install System Dependencies
```bash
sudo apt-get install libgl1-mesa-dev
sudo apt-get install libglu1-mesa-dev
sudo apt-get install zlib1g-dev
```

2. Reinstall GLUT

```bash
sudo apt-get remove --purge freeglut3-dev
sudo apt-get install freeglut3-dev
```

3. Add dependencies to your project, if not found in the project's cabal file already.

    Your [file.cabal](./haskell-platformer-game.cabal) should look like this:
```cabal
build-depends:
    base >= 4.14 && < 5,
    gloss >= 1.13 && < 1.14,
    GLUT >= 2.7 && < 2.8,
    JuicyPixels >= 3.3 && < 3.4
```

4. Install cabal
```bash 
cabal install
```
or only cabal dependencies

```bash 
cabal install --only-dependencies
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

7. If your project run as expected and the syntax highlighter accuses error in the import statement, restart the language server
    * Press: <kbd> ctrl + shift + p </kbd> 
    * Type: `Restart Haskell LSP Server`
    * Click on the option that appears
    This might be necessary because the language server does not recognize the new dependencies added to the project.


	
