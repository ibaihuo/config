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
## Modified at:   Sat Jun 15 22:57:47 2013
## Description:   初始化配置文件
##                
######################################################################


# 注意：会强制删除当前的配置文件，请备份

# 当前绝对路径
working_dir=`pwd`

# emacs
ln -sf ${working_dir}/emacs/emacs ~/.emacs
ln -sf ${working_dir}/emacs/emacs.d ~/.emacs.d


