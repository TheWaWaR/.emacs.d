;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Load path etc.
(setq python-version-checked t)
(setq python-python-command "python2.7")


(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq starter-kit-dir (concat dotfiles-dir "emacs-starter-kit/"))
;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path starter-kit-dir)

(add-to-list 'load-path (concat starter-kit-dir "/elpa-to-submit"))
(add-to-list 'load-path (concat starter-kit-dir "/elpa-to-submit/auto-complete"))
(add-to-list 'load-path (concat starter-kit-dir "/elpa-to-submit/emacs-dbgr"))
(add-to-list 'load-path (concat starter-kit-dir "/elpa-to-submit/yasnippet"))
(message starter-kit-dir)

(setq autoload-file (concat starter-kit-dir "loaddefs.el"))
(setq package-user-dir (concat starter-kit-dir "elpa"))
(setq custom-file (concat starter-kit-dir "custom.el"))

(require 'package)
(dolist (source '(("gnu" . "http://elpa.gnu.org/packages/")
                  ("technomancy" . "http://repo.technomancy.us/emacs/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(require 'starter-kit-elpa)


;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'open-next-line)
(require 'tramp)
(require 'autopair)
(require 'yasnippet)

;; backport some functionality to Emacs 22 if needed
(require 'dominating-file)

;; Load up starter kit customizations
(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
;; (require 'starter-kit-perl)
;; (require 'starter-kit-ruby)

;; (require 'starter-kit-js)
;; (require 'starter-kit-python)
(require 'starter-kit-completion)


(regen-autoloads)
(load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
    (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;;; init.el ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; My custon [BEGIN]
(add-to-list 'load-path "~/.emacs.d/my-plugins/")
(require 'youdao)
(require 'my-defuns)
;; (require 'sr-speedbar)
(require 'my-key-bindings)
(require 'undo-tree)
(require 'jinja2-mode)
(require 'highlight-indentation)

(setq eshell-directory-name (concat dotfiles-dir ".eshell"))
(setq multi-eshell-shell-function '(eshell))
(setq py-indent-offset 4)
(setq org-log-done 'time)
(setq-default indent-tabs-mode nil)     ; 用空格替换 tab
(setq eshell-history-size 1000)
(setq display-time-24hr-format t)
(setq x-select-enable-clipboard t)
(setq shell-switcher-mode t)            ; shell switcher mode 和
                                        ; winner mode 有快捷键冲突
(winner-mode)
(display-time-mode t)
(global-undo-tree-mode)
(setq line-number-mode t)
(setq column-number-mode t)
(set-default 'truncate-lines t)

;; (use-the-sdcv)
(use-the-eim)
;; (use-the-git)
;; (use-the-erlang)
;; (use-the-emms)
(use-the-jedi)
(use-the-golang)
(use-the-javascript)
(use-the-coffee)
(use-the-tabbar)
(use-the-auto-complete)
;; (use-the-neotree)
;; (use-the-slime)
(set-frame-size-according-to-resolution)

;;; 设置 esehll 提示符样式
(setq eshell-prompt-function (make-eshell-prompt-function))

; example of setting env var named “path”
; by prepending new paths to existing paths


;;; FlySpell
;; (flyspell-mode nil)
;; use apsell as ispell backend
;; (setq-default ispell-program-name "aspell")
;; use American English as ispell default dictionary
;; (ispell-change-dictionary "american" t)


;; Face stuff

(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "SeaGreen" "yellow4"
       "SkyBlue" "magenta4" "cyan4" "white"])

(set-face-background 'highlight-indentation-face "#2A2A2A")
(set-face-background 'highlight-indentation-current-column-face "#383838")

;;; Python programming
(add-to-list 'load-path "~/.emacs.d/emacs-for-python/") ;; tell where to load the various files
(require 'epy-setup)      ;; It will setup other loads, it is required!
(require 'epy-python)     ;; If you want the python facilities [optional]
(require 'epy-completion) ;; If you want the autocompletion settings [optional]
;; (require 'epy-editing)    ;; For configurations related to editing [optional]
;; (require 'epy-bindings)   ;; For my suggested keybindings [optional]
;; (require 'epy-nose)       ;; For nose integration


(epy-setup-checker "pyflakes %f")


(setq edit-server-default-major-mode 'markdown-mode)
(when (require 'edit-server nil t)
  (setq edit-server-new-frame nil)
  (edit-server-start))
;; (when (and (require 'edit-server nil t) (daemonp))
;;   (edit-server-start))


;; (if (and (fboundp 'server-running-p)
;;          (not (server-running-p)))
;;     (prog
;;      (server-start)
;;      (message "Server Started"))
;;   (message "Server already started")
;;     )

(if (eq (window-system) 'x) ;; 仅在X环境下启用
    (progn
      (require 'my-face-settings)
      (custom-gui-key-bindings)
      (alpha-transparency)
      (my-fullscreen))
  (custom-terminal-key-bindings))


(require 'my-hooks)                     ; 加载各种自定义的 hook
;;;; My custom config [END]
