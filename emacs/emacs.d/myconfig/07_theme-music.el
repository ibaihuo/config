;; ================================================================================
;; emms的配置文件,播放mp3
(add-to-list 'load-path "~/.emacs.d/plugins/emms/")
(require 'emms-setup)
(emms-standard)
;; (emms-default-players)
(require 'emms-player-simple)
(require 'emms-source-file)
(require 'emms-source-playlist)
(setq emms-player-list '(emms-player-mpg321
                         emms-player-ogg123
                         emms-player-mplayer))
;; ================================================================================

;; ================================================================================
;; 设置color-theme主题
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-oswald)
;; (color-theme-hober)
;; (color-theme-pok-wob)
;; (color-theme-vim-colors)
;; ================================================================================
