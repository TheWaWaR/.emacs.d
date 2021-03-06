
(defun my-c-mode-common-hook ()
  ;; my customizations for all of c-mode, c++-mode, objc-mode, java-mode
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here

  (setq c++-tab-always-indent t)
  (setq c-basic-offset 4) ;; Default is 2
  (setq c-indent-level 4) ;; Default is 2

  (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
  (setq tab-width 4)
  (setq indent-tabs-mode t)             ; use spaces only if nil
  )

(defun my-erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "C-c j") 'erlang-man-function))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'my-c-mode-common-hook)
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)


(defun on-after-init ()
  (interactive)
  (progn
    (load-theme 'wombat t)
     ;; (unless (display-graphic-p (selected-frame))
     ;; (set-face-background 'default "unspecified-bg" (selected-frame)))
    ))

(add-hook 'window-setup-hook 'on-after-init)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)
;; (add-hook 'emms-player-finished-hook 'emms-random)          ;当播放完当前的歌曲时随机选择下一首歌曲

(provide 'my-hooks)
