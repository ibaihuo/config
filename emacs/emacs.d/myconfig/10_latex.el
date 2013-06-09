;; 自定义latex-mode
(defun my-latex-mode-hook()
  (setq Tex-newline-function 'newline-and-indent) ;;回车时自动缩进
  (setq LaTex-basic-offset 8)
  )
(add-hook 'LaTex-mode-hook 'my-latex-mode-hook)

;; xelatex配置文件
(defun auctex ()
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t; %`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq TeX-command-default "XeLaTeX")
  (setq TeX-save-query nil)
  (setq TeX-show-compilation t))
(add-hook 'LaTeX-mode-hook 'auctex)

(custom-set-variables
 '(TeX-output-view-style
   (quote
	(
	 ("^dvi$nnnnnnn"
	  ("^landscape$" "^pstricks$\\|^pst-\\|^psfrag$")
	  "%(o?)dvips -t landscape %d -o && gv %f")
	 ("^dvi$" "^pstricks$\\|^pst-\\|^psfrag$" "%(o?)dvips %d -o && gv %f") ("^dvi$" ("^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "^landscape$") "%(o?)xdvi %dS -paper a4r -s 0 %d") ("^dvi$" "^a4\\(?:dutch\\|paper\\|wide\\)\\|sem-a4$" "%(o?)xdvi %dS -paper a4 %d") ("^dvi$" ("^a5\\(?:comb\\|paper\\)$" "^landscape$") "%(o?)xdvi %dS -paper a5r -s 0 %d") ("^dvi$" "^a5\\(?:comb\\|paper\\)$" "%(o?)xdvi %dS -paper a5 %d") ("^dvi$" "^b5paper$" "%(o?)xdvi %dS -paper b5 %d") ("^dvi$" "^letterpaper$" "%(o?)xdvi %dS -paper us %d") ("^dvi$" "^legalpaper$" "%(o?)xdvi %dS -paper legal %d") ("^dvi$" "^executivepaper$" "%(o?)xdvi %dS -paper 7.25x10.5in %d") ("^dvi$" "." "%(o?)xdvi %dS %d") ("^pdf$" "." "evince %o %(outpage)") ("^html?$" "." "netscape %o")))))
