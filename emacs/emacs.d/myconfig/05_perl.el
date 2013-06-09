;; pde的设置
;; (add-to-list 'load-path "~/.emacs.d/elisp/perl_lisp/")
;; (require 'pde-load)
;; (load "pde-load")

;; set for perl
(add-hook 'find-file-hooks 
	  '(lambda ()
	     (auto-insert)
	     ))

;; Use cperl-mode instead of the default perl-mode
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

;; Since I do not like the default indentations, I have the followings: 

(add-hook 'cperl-mode-hook 'renewjoy-cperl-mode-hook t)
(defun renewjoy-cperl-mode-hook ()
  (setq cperl-indent-level 8)
  (setq cperl-continued-statement-offset 0)
  (setq cperl-extra-newline-before-brace t)
  ;;   (set-face-background 'cperl-array-face "wheat")
  ;;   (set-face-background 'cperl-hash-face "wheat")
  )

;; (defalias 'perl-mode 'cperl-mode)
;; (defun pde-perl-mode-hook ()
;;   (abbrev-mode t)
;;   (add-to-list 'cperl-style-alist
;;                '("PDE"
;;                  (cperl-auto-newline                         . t)
;;                  (cperl-brace-offset                         . 0)
;;                  (cperl-close-paren-offset                   . -4)
;;                  (cperl-continued-brace-offset               . 0)
;;                  (cperl-continued-statement-offset           . 4)
;;                  (cperl-extra-newline-before-brace           . nil)
;;                  (cperl-extra-newline-before-brace-multiline . nil)
;;                  (cperl-indent-level                         . 4)
;;                  (cperl-indent-parens-as-block               . t)
;;                  (cperl-label-offset                         . -4)
;;                  (cperl-merge-trailing-else                  . t)
;;                  (cperl-tab-always-indent                    . t)))
;;   (cperl-set-style "PDE"))

