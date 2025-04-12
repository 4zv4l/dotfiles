#!/bin/bash

# fix zellij config
perl -i.bak -pe 's/\$HOME/$ENV{HOME}/e' ./dotfiles/.config/zellij/{config.kdl,my-layout.kdl}

# install packer
git clone --depth 1 https://github.com/wbthomason/packer.nvim\
 ~/.local/share/nvim/site/pack/packer/start/packer.nvim
