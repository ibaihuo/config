;; for lisp
;; (add-to-list 'load-path "/usr/share/emacs23/site-lisp/slime/")
;; (setq inferior-lisp-program "/usr/bin/sbcl")
;;  (require 'slime)
;;  (slime-setup)

;; (defun lisp-indent-or-complete (&optional arg)
;;   (interactive "p")
;;   (if (or (looking-back "^\\s-*") (bolp))
;;       (call-interactively 'lisp-indent-line)
;;       (call-interactively 'slime-indent-and-complete-symbol)))

;; (eval-after-load "lisp-mode"
;;   '(progn
;;      (define-key lisp-mode-map (kbd "TAB") 'lisp-indent-or-complete)))
