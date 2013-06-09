;; php的设置
(require 'php-mode)
;; (add-hook 'php-mode-user-hook 'turn-on-font-lock)

;; javascript模式的设置
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(setq auto-mode-alist (cons '("\\.js" . javascript-mode) auto-mode-alist))
;; javascript 的 js 文件使用HTML模式
;; (setq auto-mode-alist (cons '("\\.js" . html-mode) auto-mode-alist))


;;============================================================
;;============================================================
;; jsp-mode
;; Tell emacs where to find the code for jsp-mode
;; (require 'jsp-mode)
;; (autoload 'jsp-mode "jsp" "JSP" t)
;; ; Tell emacs to use jsp-mode for .jsp files
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . jsp-mode))
