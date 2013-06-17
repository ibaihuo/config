#!/bin/bash
######################################################################
## Filename:      init_config.sh
##                
## Copyright (C) 2013,  renewjoy
## Version:       
## Author:        renewjoy <oyea9le@gmail.com>
## Created at:    Sat Jun 15 22:51:46 2013
##                
## Modified by:   renewjoy <oyea9le@gmail.com>
## Modified at:   Sun Jun 16 00:02:14 2013
## Description:   初始化配置文件
##                
######################################################################

# 注意：会强制删除当前的配置文件，请备份

# 当前绝对路径
working_dir=`pwd`

# emacs
rm -rf ~/.emacs ~/.emacs.d
ln -s ${working_dir}/emacs/emacs ~/.emacs
ln -s ${working_dir}/emacs/emacs.d ~/.emacs.d

# qtile
rm -rf ~/.config/qtile/config.py
ln -s ${working_dir}/qtile/config.py ~/.config/qtile/config.py

# i3 
rm -rf ~/.i3/config
ln -s ${working_dir}/i3/config ~/.i3/config

# zsh
rm -rf ~/.zshrc ~/.oh-my-zsh
ln -s ${working_dir}/zsh/zshrc ~/.zshrc
ln -s ${working_dir}/zsh/oh-my-zsh ~/.oh-my-zsh

# git
rm -rf ~/.gitconfig
ln -s ${working_dir}/git/gitconfig ~/.gitconfig
