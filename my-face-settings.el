

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Font config
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (set-default-font "-unknown-Monaco-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")
;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;   (set-fontset-font (frame-parameter nil 'font) charset
;;                     (font-spec :family "Yahei Mono" )))

;; (set-language-environment 'UTF-8) 
;; (set-locale-environment "UTF-8")

;;; set new frame font
(add-to-list 'default-frame-alist
             '(font . "-unknown-Monaco-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"))
(set-frame-font "Monaco:pixelsize=15")
(set-fontset-font (frame-parameter nil 'font)
                  'han '("Microsoft Yahei" . "unicode-bmp"))

;; (set-frame-font "Yahei Mono:pixelsize=14") ;这个字体的14号配文泉驿微米黑的效果刚好等宽。
(dolist (charset '(han kana symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset (font-spec :family "文泉驿微米黑" :size 16))) ; 微软雅黑

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 '(emms-playlist-track-face ((t (:foreground "DarkSeaGreen" :height 109 :family "Monospace"))))
 '(org-formula ((t (:foreground "chocolate1" :height 109 :family "Yahei Mono"))))
 '(org-table ((t (:foreground "LightSkyBlue" :height 109 :family "Yahei Mono"))))
 ;;'(org-tag ((t (:weight bold :height 109 :family "Yahei Mono"))))
 '(tooltip ((t (:background "gray20" :foreground "wheat" :slant normal :family "文泉驿微米黑"))))
 )


(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "SeaGreen" "yellow4"
       "SkyBlue" "magenta4" "cyan4" "white"])

(set-face-background 'highlight-indentation-face "#2A2A2A")
(set-face-background 'highlight-indentation-current-column-face "#383838")

(provide 'my-face-settings)
