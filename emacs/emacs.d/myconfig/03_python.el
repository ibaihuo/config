;; 自动将py后缀的文件和python-mode关联
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
(setq py-indent-offset 4)		;自定义缩进为4
(autoload 'python-mode "python-mode" "Python editing mode." t)
(setq py-shell-name 'ipython)
(global-set-key (kbd "C-c p") 'pdb)

;; (load "~/.emacs.d/plugins/color-theme-molokai.el")
;; (color-theme-molokai)

;; 自定义
(defun my-python-mode-hook()
  ;;(color-theme-molokai)
  ;;(yes/minor-mode)			;为python-mode增加snippet功能
  ;;(require 'color-theme)
  ;;(color-theme-initialize)
  ;;(color-theme-hober)
  ;;(color-theme-oswald)
  ;; (color-theme-billw)
  )

(add-hook 'python-hook 'my-python-mode-hook)


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