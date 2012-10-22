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

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(setq starter-kit-dir (concat dotfiles-dir "emacs-starter-kit"))
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
(message "Mark 0")
(require 'starter-kit-misc)
(message "Mark 1")
(require 'starter-kit-registers)
(require 'starter-kit-eshell)
(require 'starter-kit-lisp)
;; (require 'starter-kit-perl)
;; (require 'starter-kit-ruby)

(message "Mark 2")

(require 'starter-kit-js)
(require 'starter-kit-python)
(require 'starter-kit-completion)

(message "After requires")

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
(require 'sr-speedbar)
(require 'my-defuns)
(require 'my-key-bindings)
(require 'undo-tree)
(require 'jinja2-mode)
(require 'highlight-indentation)
(require 'my-face-settings)

(setq eshell-directory-name (concat dotfiles-dir ".eshell"))
(setq py-indent-offset 4)
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


(use-the-sdcv)
(use-the-eim)
(use-the-git)
(use-the-emms)
(use-the-tabbar)
(use-the-auto-complete)
;; (use-the-slime)
(set-frame-size-according-to-resolution)

;;; 设置 esehll 提示符样式
(setq eshell-prompt-function (make-eshell-prompt-function))

; example of setting env var named “path”
; by prepending new paths to existing paths
(setenv "PATH"
  (concat
   "/home/weet/Dropbox/Code/Python/bin:"
   (getenv "PATH") ; inherited from OS
  )
)

;;; FlySpell
;; (flyspell-mode nil)
;; use apsell as ispell backend
;; (setq-default ispell-program-name "aspell")
;; use American English as ispell default dictionary
;; (ispell-change-dictionary "american" t)


;; Theme
(require 'color-theme)
(require 'color-theme-zenburn-old)
(color-theme-zenburn-old)

;;; Python programming
(add-to-list 'load-path "~/.emacs.d/emacs-for-python/") ;; tell where to load the various files
(require 'epy-setup)      ;; It will setup other loads, it is required!
(require 'epy-python)     ;; If you want the python facilities [optional]
(require 'epy-completion) ;; If you want the autocompletion settings [optional]
;; (require 'epy-editing)    ;; For configurations related to editing [optional]
;; (require 'epy-bindings)   ;; For my suggested keybindings [optional]
;; (require 'epy-nose)       ;; For nose integration


(epy-setup-checker "pyflakes %f")

(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (prog
     (server-start)
     (message "Server Started"))
  (message "Server already started")
    )

(if (eq (window-system) 'x) ;; 仅在X环境下启用
    (progn
      (custom-gui-key-bindings)
      (alpha-transparency)
      (my-fullscreen)
      )
  (custom-terminal-key-bindings)
  )


(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4)                  ;; Default is 2
  (setq c-indent-level 4)                  ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)  ; use spaces only if nil
  )
(add-hook 'c++-mode-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;; Hooks
;; (add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)
(add-hook 'python-mode-hook 'highlight-indentation-mode)

;;;; My custom config [END]

