;;========================================定制基本操作开始========================================
;; 设置自己的名字和邮件，在发邮件时会用到，当然像在html中自动添加标签时也会用到。
(setq user-full-name "renewjoy")
(setq user-mail-address "oyea9le@gmail.com")

;;打开ido的支持，这个可以用在C-x C-f打开文件的时候后面有提示;
(ido-mode t)

;; 设置(搜索）打开文件的缺省路径
(setq default-directory "~")

;; 关闭烦人的出错时提示声
(setq visible-bell t)

;; 没有提示音，也不闪屏
(setq ring-bell-function 'ignore)

;; 关闭启动时的LOGO信息
(setq inhibit-startup-message t)

;; 高亮显示选中的文本
(setq transient-mark-mode t)

;; 以y/n代替yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; 去掉菜单栏
(menu-bar-mode nil)

;; 显示当前光标的坐标
(setq line-number-mode t)	;;行号，从1开始
(setq column-number-mode t)	;;列号，从0开始

;; 取消C-SPC键设为mark
(global-set-key (kbd "C-SPC") 'nil)

;; 设置M-m来做为setmark
(global-set-key (kbd "M-m") 'set-mark-command)

;; 正则表达式替换
(global-set-key (kbd "C-c %") 'query-replace-regexp)

;; 允许emacs和外部其他程序的复制/粘贴
(setq x-select-enable-clipboard t)

;; 打开图片显示功能
(auto-image-file-mode t)

;; 删除整行及这行本身,和原来的C-a C-k的区别就是，这个删除行内容后还删除行本身
(global-set-key (kbd "C-c C-k") 'kill-whole-line)

;;去掉滚动条,但是这个功能在我机子上无法实现，不知道是为什么？
;;(set-scroll-bar-mode nil)

;; 设置语法高亮
(global-font-lock-mode  t)

;; 设置kill-ring-max为200
(setq kill-ring-max 200)

;; 设置高亮显示，为了加速显示大文件的程序
(setq font-lock-maximum-decoration t)

;; 不产生备份文件
(setq make-backup-files nil)
;;=========================================定制基本操作结束========================================

;;=========================================显示时间设置开始=========================================
;; 启用minibuffer时间显示设置
(display-time-mode t)

;; 时间显示包括日期和具体时间
(setq display-time-day-and-date t)

;; 时间使用24小时制
(setq display-time-24hr-format t)
;;=========================================显示时间设置结束=========================================

;; 设置Ctrl-x e 打开 eshell
(global-set-key (kbd "C-c e") 'eshell)

;;(setq frame-title-format "%b@Nubi")  ================================================================================
;; 为org-mode设置
(setq org-hide-leading-stars t)
(define-key global-map "\C-c a" 'org-agenda)
(setq org-log-done 'time)

(autoload 'nc "nc" "Emulate MS-DOG file shell" t)

;; for emacs-w3m
;; for trac-wiki emacs
;; for C-c C-p
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/")
;; (require 'w3m-load)
;; (setq w3m-home-page "http://www.google.com.hk")
