(setq default-tab-width 4)
(setq tab-width 4)
(setq tab-stop-list ())

;; 用空格来代替tab（在python中非常必要）
;;(setq-default indent-tabs-mode nil)


;; 成对出现的符号自动完成：'' "" () [] {}
(setq skeleton-pair t)
;; (setq skeleton-pair-on-word t)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "<") 'skeleton-pair-insert-maybe)    ;写程序中的小于符号太多，这个不适合设置
(global-set-key (kbd "'") 'skeleton-pair-insert-maybe) 
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe) ;需要对"进行转义

(autoload 'html-fold-mode "html-fold" "Minor mode for hiding and revealing elements." t)

;; 调用 linum.el(line number)来显示行号
;; (require 'linum)
(global-linum-mode t)

;; 显示与之匹配的符号(,[,{
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;增大使用查找函数和变量的寻找范围
(setq apropos-do-all t)

;; 设置当光标处于最后一行时，再按C-n时将新建一行
;; (setq next-line-add-newlines t)

;; 大小写转换功能
;; 使用C-x-l (lower)可能使当前选中区域的单词变成小写
(put 'upcase-region 'disabled nil) 

;; 开启被禁用的功能，使用C-x-u (upper)可以把当前选中的区域变成大写
(put 'downcase-region 'disabled nil)

;;加载auto-header.el文件,自动添加文件头
(require 'auto-header)
;; 设置文件头中的姓名
(setq header-full-name "renewjoy")
;; 设置我的邮箱
(setq header-email-address "oyea9le@gmail.com")
;; 设置每次保存时要更新的项目
(setq header-update-on-save '(filename
							  modified
							  counter
							  copyright
							  ))

;; 自动插入
;; (setq auto-insert t)		   ;open the function
;; (setq auto-insert-query t)	   ;query before perform this function
;; (setq auto-insert-copyright (user-full-name))

;; 设置文件头的显示格式
(setq header-field-list '(
						  filename  ;;文件名
						  blank	    ;空行，下同
						  copyright ;版权
						  version
						  author	;作者
						  created	;创建时间
						  blank						  
						  modified_by	;更改者
						  modified      ;更改时间
						  ;;update	;更新次数
						  ;;status	;状态，是否发布						  
						  ;;blank
						  description	;描述
						  blank
						  ))
(autoload 'folding-mode          "folding" "Folding mode" t)
(autoload 'turn-off-folding-mode "folding" "Folding mode" t)
(autoload 'turn-on-folding-mode  "folding" "Folding mode" t)


;; 整行移动
;; ALT-up  会把光标所在的整行文字上移一行
;; ALT-down会把光标所在的整行文字下移一行
(global-set-key [(meta p)] 'move-line-up)
(global-set-key [(meta n)] 'move-line-down) 
(defun move-line (&optional n)
  "Move current line N (1) lines up/down leaving point in place."
  (interactive "p")
  (when (null n)
    (setq n 1))
  (let ((col (current-column)))
    (beginning-of-line)
    (next-line 1)
    (transpose-lines n)
    (previous-line 1)
    (move-to-column col)))
(defun move-line-up (n)
  "Moves current line N (1) lines up leaving point in place."
  (interactive "p")
  (move-line (if (null n) -1 (- n)))) 

(defun move-line-down (n)
  "Moves current line N (1) lines down leaving point in place."
  (interactive "p")
  (move-line (if (null n) 1 n))) 

;; 功能是：按M-w键就可以复制光标所在的这一行
(global-set-key (kbd "M-w") 'huangq-save-line-dwim)
(defun huangq-save-one-line (&optional arg)
  "save one line. If ARG, save one line from first non-white."
  (interactive "P")
  (save-excursion
    (if arg
        (progn
          (back-to-indentation)
          (kill-ring-save (point) (line-end-position)))
      (kill-ring-save (line-beginning-position) (line-end-position)))))
(defun huangq-kill-ring-save (&optional n)
  "If region is active, copy region. Otherwise, copy line."
  (interactive "p")
  (if (and mark-active transient-mark-mode)
      (kill-ring-save (region-beginning) (region-end))
    (if (> n 0)
        (kill-ring-save (line-beginning-position) (line-end-position n))
      (kill-ring-save (line-beginning-position n) (line-end-position)))))
(defun huangq-save-line-dwim (&optional arg)
  "If region is active, copy region.
If ARG is nil, copy line from first non-white.
If ARG is numeric, copy ARG lines.
If ARG is non-numeric, copy line from beginning of the current line."
  (interactive "P")
  (if (and mark-active transient-mark-mode)
      ;; mark-active, save region
      (kill-ring-save (region-beginning) (region-end))
    (if arg
        (if (numberp arg)
            ;; numeric arg, save ARG lines
            (huangq-kill-ring-save arg)
          ;; other ARG, save current line
          (huangq-save-one-line))
      ;; no ARG, save current line from first non-white
      (huangq-save-one-line t))))

;; 自动补全功能：snippet----yet another snippet
(add-to-list 'load-path "~/.emacs.d/plugins/")
(require 'yasnippet)
(yas/global-mode 1)

;; (load-file "~/.emacs.d/elisp/cedet-1.0.1/common/cedet.el")
;; ;;;; 具体说明可参考源码包下的INSTALL文件，或《A Gentle introduction to Cedet》
;; ;; Enabling Semantic (code-parsing, smart completion) features
;; ;; Select one of the following:
;; ;;(semantic-load-enable-minimum-features)
;; ;;(semantic-load-enable-code-helpers)
;; ;;(semantic-load-enable-gaudy-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)
;; ;;(semantic-load-enable-semantic-debugging-helpers)

;; ;;;; 使函数体能够折叠或展开
;; ;; Enable source code folding
;; (global-semantic-tag-folding-mode 1)

;; ;; Key bindings
;; (defun my-cedet-hook ()
;;   (local-set-key [(control return)] 'semantic-ia-complete-symbol)
;;   (local-set-key "/C-c?" 'semantic-ia-complete-symbol-menu)
;;   (local-set-key "/C-cd" 'semantic-ia-fast-jump)
;;   (local-set-key "/C-cr" 'semantic-symref-symbol)
;;   (local-set-key "/C-cR" 'semantic-symref))
;; (add-hook 'c-mode-common-hook 'my-cedet-hook)
;; ;;;; 当输入"."或">"时，在另一个窗口中列出结构体或类的成员
;; (defun my-c-mode-cedet-hook ()
;;   (local-set-key "." 'semantic-complete-self-insert)
;;   (local-set-key ">" 'semantic-complete-self-insert))
;; (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)


;;;; trac-wiki
(require 'trac-wiki)

;; 定义工程名字
(trac-wiki-define-project "wikijoy" "http://www.wikijoy.org/wikijoy/" t)
(trac-wiki-define-project "works" "http://www.wikijoy.org/works/" t)

;; 插入trac的sh代码高亮
(defun insert-trac-sh-highlight ()
  (interactive)
  (insert "{{{
#!sh
}}}"))
;; 插入转义标记
(defun insert-trac-escape ()
  (interactive)
  (insert "{{{}}}"))
(global-set-key (kbd "C-c }") 'insert-trac-sh-highlight)
(global-set-key (kbd "C-c {") 'insert-trac-escape)
(global-set-key (kbd "C-c w") 'trac-wiki) ;定义进入trac-wiki的快捷键

;; for piglatin
 (load-file "~/.emacs.d/elisp/piglatin.el")

;; for lua
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; for visual-regexp
;; https://github.com/benma/visual-regexp.el.git
;; (require 'visual-regexp)
;; (define-key global-map (kbd "C-c r") 'vr/replace)
;; (define-key global-map (kbd "C-c q") 'vr/query-replace)

;; (add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode/")
;; (require 'haskell-mode-autoloads)
;; (add-to-list 'Info-default-directory-list "~/.emacs.d/elisp/haskell-mode/")

;; (add-to-list 'load-path "~/.emacs.d/elisp/scala-mode2/")
;; (require 'scala-mode2)

;; go mode
(add-to-list 'load-path "~/.emacs.d/elisp/go-mode.el")
(require 'go-mode-autoloads)
;; 保存时自动格式化
(add-hook 'before-save-hook 'gofmt-before-save)
