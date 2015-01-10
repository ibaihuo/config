;; 自动将py后缀的文件和python-mode关联
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

(setq py-indent-offset 4)		;自定义缩进为4

;; 兼容公司的TAB设置
;; (add-hook 'python-mode-hook
;;   (lambda () (setq indent-tabs-mode t)))

(setq py-shell-name 'ipython)

;; 调用pdb
(global-set-key (kbd "C-c p") 'pdb)

;; (load "~/.emacs.d/plugins/color-theme-molokai.el")
;; (color-theme-molokai)

;; ;; 自定义
;; (defun my-python-mode-hook()
;;   ;;(color-theme-molokai)
;;   ;;(yes/minor-mode)			;为python-mode增加snippet功能
;;   ;;(require 'color-theme)
;;   ;;(color-theme-initialize)
;;   ;;(color-theme-hober)
;;   ;;(color-theme-oswald)
;;   ;; (color-theme-billw)
;;   )

;; (add-hook 'python-hook 'my-python-mode-hook)

;; for pymacs
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)




;; for ropemacs
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)


;; 可以装一个ropemacs，然后在.emacs里面加入下面一段配置自己感受一下，alt+/ 就会自动补全了。 
;; python 
;; (defun load-ropemacs () 
;;    "Load pymacs and ropemacs" 
;;    (interactive) 
;;    (setenv "PYMACS_PYTHON" "python2.7") 
;;    (require 'pymacs) 
;;    (autoload 'pymacs-load "pymacs" nil t) 
;;    (autoload 'pymacs-eval "pymacs" nil t) 
;;    (autoload 'pymacs-apply "pymacs") 
;;    (autoload 'pymacs-call "pymacs") 
;;    (autoload 'pymacs-exec "pymacs" nil t) 
;;    (pymacs-load "ropemacs" "rope-") 
;;    (global-set-key [(meta ?/)] 'rope-code-assist) 
;;    (setq rope-confirm-saving 'nil) 
;;    )
  
;; (add-hook 'python-mode-hook 'load-ropemacs)
;; (add-to-list 'load-path "/home/renewjoy/.emacs.d/elisp/python-mode.el-6.1.3") 
;; (setq py-install-directory "/home/renewjoy/.emacs.d/elisp/python-mode.el-6.1.3")
;; (require 'python-mode)


;; 初始化
(load-file "~/.emacs.d/plugins/emacs-for-python/epy-init.el")
(require 'epy-setup)      ;; It will setup other loads, it is required!
(require 'epy-python)     ;; If you want the python facilities [optional]
(require 'epy-completion) ;; If you want the autocompletion settings [optional]
(require 'epy-editing)    ;; For configurations related to editing [optional]
(require 'epy-bindings)   ;; For my suggested keybindings [optional]
(require 'epy-nose)       ;; For nose integration

;; 拼写检查
(epy-setup-checker "pyflakes %f")

;; 当前行高亮
;(global-hl-line-mode t) ;; To enable
;(set-face-background 'hl-line "LightYellow") ;; change with the color that you like
                                       ;; for a list of colors: http://raebear.net/comp/emacscolors.html

;; 设置ipython
(epy-setup-ipython)

;; 符号自动匹配
;;(setq skeleton-pair nil)

;; 高亮缩进
(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation)
