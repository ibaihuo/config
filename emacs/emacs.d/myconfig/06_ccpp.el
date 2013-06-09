;;============================================================
;;C/C++ 

;;;; CC-mode配置 http://cc-mode.soucesorge.net/
(require 'cc-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)

;; 将.h文件关联为c++-mode(默认为c模式高亮显示)
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)) auto-mode-alist))

(defun my-c-mode-hook()
  ;;将回车代替C-j的功能，换行的同时对齐
  (define-key c-mode-map [return] 'newline-and-indent)
  (interactive)
  ;;设置C程序的对齐风格
  (c-set-style "K&R")
					;  (yas/minor-mode)			;(setq yas/minor-mode-on t)

  ;; 当你输入时自动缩进，自动换行
  ;; 自动模式，在此种模式下当你键入{时，会自动根据你设置的对齐风格对齐
  (c-toggle-auto-state)

  ;; 设置compile快捷键
  (global-set-key (kbd "C-c m") 'compile)

  ;;设置运行编译命令(Alt-x compile)后，默认显示的编译命令，原来为make -k
  (setq compile-command "gcc -ggdb3 -Wall -o ")

  ;; 此模式下，当按backspace时会删除最多的空格
  (c-toggle-hungry-state)

  ;; TAB键的宽度设置为4
  (setq c-basic-offset 4)

  ;; 在菜单中加入当前Buffer的函数索引
  ;; (imenu-add-menubar-index)

  ;; 在状态条上显示当前光标在哪个函数体内部
  (which-function-mode)
  )


;; 自定义C++模式
(defun my-c++-mode-hook()
  (define-key c++-mode-map [return] 'newline-and-indent)
  (interactive)
  (c-set-style "linux")
					;  (c-toggle-auto-state)
  (c-toggle-hungry-state)
;;;;Ctrl-c m 调用compile
  (global-set-key (kbd "C-c m") 'compile)

  ;;设置运行编译命令(Alt-x compile)后，默认显示的编译命令，原来为make -k
  (setq compile-command "g++ -ggdb3 -Wall -o ")

  (setq c-basic-offset 4)
  (imenu-add-menubar-index)

  ;; 设置gdb的多窗口调试,这是一个非常不错的功能，太好了。
  (setq gdb-many-windows t)

  (which-function-mode)
  )
 

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook) 
