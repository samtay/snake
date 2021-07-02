# snake

A terminal interface for Snake. This is kind of a toy program, written as a demonstration of the
[brick](https://hackage.haskell.org/package/brick-0.18) library.

<p align="center">
  <img src="./docs/img/example.gif"/>
</p>

It is part of my [Introduction to Brick](https://samtay.github.io/posts/introduction-to-brick)
tutorial. Feel free to leave issues here or on
[samtay/samtay.github.io](https://github.com/samtay/samtay.github.io)
if you spot any issues or want to leave feedback.

## installation
Installation on MacOS can be accomplished via homebrew:
```shell
brew install samtay/tui/snake
```
Arch Linux users can install from the [AUR](https://aur.archlinux.org/packages/snake-terminal-git/):
```shell
yay -S snake-terminal-git # or yaourt -S snake-terminal-git, etc.
```
Others can install from source with [stack](https://docs.haskellstack.org/en/stable/README/#how-to-install):
```shell
git clone https://github.com/samtay/snake.git
cd snake
stack install snake
```
If you are on Debian and want to install via package manager, feel free to open an issue and I'll try to get around to it.

## playing the game

After launching the game, press any of the arrow keys or the letters 'k', 'j', 'l', or 'h' to start the game.
