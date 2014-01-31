#!/usr/bin/env python
#-*- coding:utf-8 -*-
######################################################################
## Filename:	  config.py
##				
## Copyright (C) 2013,  renewjoy
## Version:	   
## Author:		renewjoy <oyea9le@gmail.com>
## Created at:	Fri May 17 22:39:38 2013
##				
## Modified by:   renewjoy <oyea9le@gmail.com>
## Modified at:   Thu Oct  3 21:14:28 2013
## Description:   qtile配置文件
##				
######################################################################

from libqtile.config import Key, Screen, Group
from libqtile.command import lazy
from libqtile import layout, bar, widget, hook

# you can run xmodmap to show what is mod4
mod = "mod4"

# 定义快捷键
keys = [
	# Switch between windows in current stack pane
	Key(
		[mod], "k",
		lazy.layout.down()
	),
	Key(
		[mod], "j",
		lazy.layout.up()
	),

	# Move windows up or down in current stack
	Key(
		[mod, "shift"], "k",
		lazy.layout.shuffle_down()
	),
	Key(
		[mod, "shift"], "j",
		lazy.layout.shuffle_up()
	),

	# Switch window focus to other pane(s) of stack
	Key(
		[mod], "space",
		lazy.layout.next()
	),

	# Swap panes of split stack
	Key(
		[mod, "shift"], "space",
		lazy.layout.rotate()
	),

	# Toggle between split and unsplit sides of stack.
	# Split = all windows displayed
	# Unsplit = 1 window displayed, like Max layout, but still with multiple stack panes
	Key(
		[mod, "shift"], "Return",
		lazy.layout.toggle_split()
	),


	Key([mod], "h",	  lazy.to_screen(1)),
	Key([mod], "l",	  lazy.to_screen(0)),


	Key([mod], "Return", lazy.spawn("gnome-terminal")), # 打开终端

	# Toggle between different layouts as defined below
	Key([mod], "Tab",	lazy.nextlayout()),
	Key([mod], "w",	  lazy.window.kill()),


	Key([mod, "shift"], "r", lazy.restart()), # 重启界面
	Key([mod], "r", lazy.spawncmd()),		  # 运行程序

	# 音量调节Fn+f11/Fn+f12
	Key(
		[], "XF86AudioRaiseVolume",
		lazy.spawn("amixer -c 0 -q set Master 1dB+")
	),
	Key(
		[], "XF86AudioLowerVolume",
		lazy.spawn("amixer -c 0 -q set Master 1dB-")
	),
	Key(
		[], "XF86AudioMute",
		lazy.spawn("amixer -c 0 -q set Master toggle")
	),
]

groups = [
	Group("all"),
	Group("server"),
	Group("doc"),
	Group("fchrome"),
	Group("udo"),
	Group("ifor"),
	Group("open"),
	Group("pX3"),
]

for i in groups:
	# mod1 + letter of group = switch to group
	letter = i.name[0]
	# lower
	keys.append(
		Key([mod], letter, lazy.group[i.name].toscreen())
	)
	# # for upper
	# upletter = letter.upper()
	# keys.append(
	# 	Key([mod], upletter, lazy.group[i.name].toscreen())
	# )

	# mod1 + shift + letter of group = switch to & move focused window to group
	keys.append(
		Key([mod, "shift"], letter, lazy.window.togroup(i.name)) # 将当前焦点的窗口移动到相应的屏幕
	)

dgroups_key_binder = None
dgroups_app_rules = []

border = dict(
    border_normal='#ffff00',
    border_width=1,
)

# Layout instances:
# layouts = [
#     layout.Max(),
#     layout.Stack(stacks=2, **border),
#     layout.Tile(**border),
#     layout.RatioTile(**border),
#     layout.MonadTall(**border),
# ]
layouts = [
	layout.Max(border_width = 2, border_focus='#ee0000'),						   # 默认最大化布局
	layout.Stack(stacks = 2, border_width = 2, border_focus='#ee0000') # 设置每个屏幕最多显示的栈数(布局的窗口数目)
]

screens = [Screen(
	# 顶部
	top = bar.Bar([widget.Prompt(),							  # 输入命令
				   widget.Sep(),
				   widget.WindowTabs(),                      # bugs
				   #widget.WindowName(foreground = "a0a0a0",), # 当前窗口名称
				   widget.Sep(),
				   widget.Volume(foreground = "70ff70"), # 音量
				   widget.Sep(),
				   # 电量指示
				   widget.Battery(
					   energy_now_file = 'charge_now',
					   energy_full_file = 'charge_full',
					   power_now_file = 'current_now',
					   update_delay = 5,
					   foreground = "7070ff",
					   charge_char = u'↑',
					   discharge_char = u'↓',
					   ),
				   widget.Sep(),
				   #widget.Wlan(),
				   widget.Sep(),
				   widget.Notify(),		   # 通知
				   widget.Sep(),
				   widget.Systray(),	   # 托盘
				   widget.Sep(),
				   widget.CurrentLayout(), # 当前布局名称
				   widget.Sep(),
				   widget.Clock(foreground = "a0a0a0",fmt = '%Y-%m-%d %a %H:%M:%S'), # 时钟
				   ],
				  18,
				  ),
	# 底部
	bottom = bar.Bar([widget.GroupBox(urgent_alert_method = 'text',
									  fontsize = 15, 
									  borderwidth = 1,
									  margin_y = 1,
									  margin_x = 1,
									  padding  =  1,),

					  widget.TextBox(text = "", width = 440),
					  widget.TextBox(text = "renewjoy@[Knownsec][Jiasule][GAC]", width = 400, foreground = "9aff9a"),
					  widget.TextBox(text = "[南无阿弥陀佛]", foreground = "ffff00"),
					  ],
					 20,			# 高度
					 ),
	),
	]

# def one():
# 	exec gnome-settings-daemon

#main = None
#main = one
follow_mouse_focus = True
cursor_warp = False
floating_layout = layout.Floating()
mouse = ()
auto_fullscreen = True
widget_defaults = {}

# import subprocess, re
# def is_running(process):
#     s = subprocess.Popen(["ps", "axuw"], stdout = subprocess.PIPE)
#     for x in s.stdout:
#         if re.search(process, x):
#             return True
#     return False

# def execute_once(process):
#     if not is_running(process):
#         return subprocess.Popen(process.split())

# start the applications at Qtile startup
# @hook.subscribe.startup
# def startup():
#     subprocess.Popen("sleep 3".split())
    #execute_once("nm-applet")
    # execute_once("synergys")
    # execute_once("xcompmgr")
    # execute_once("ibus-daemon --xim")
    # execute_once("hsetroot -tile /home/arkchar/Pictures/desktop.jpg")
    # execute_once("xsetroot -cursor_name left_ptr")

# 让对话框直接悬浮弹出
@hook.subscribe.client_new
def floating_dialogs(window):
	dialog = window.window.get_wm_type() == 'dialog'
	transient = window.window.get_wm_transient_for()
	if dialog or transient:
		window.floating = True
