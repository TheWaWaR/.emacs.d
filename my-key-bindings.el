


(defun custom-gui-key-bindings ()
  (global-set-key (kbd "C-;") 'delete-other-windows)
  (global-set-key (kbd "C-c ;") 'delete-other-windows)
  (global-set-key (kbd "C-*") 'eshell)
  (global-set-key (kbd "C-&") 'sql-mysql)
  (global-set-key (kbd "C-'") 'winner-undo)
  (global-set-key (kbd "C-\"") 'winner-redo)
  (global-set-key (kbd "C-,") 'next-buffer)
  (global-set-key (kbd "C-.") 'previous-buffer)
  (global-set-key (kbd "C-}") 'tabbar-forward-group)
  (global-set-key (kbd "C-{") 'tabbar-backward-group)
  (global-set-key (kbd "C->") 'tabbar-forward)
  (global-set-key (kbd "C-<") 'tabbar-backward)
  (global-set-key (kbd "C-c g") 'my-find-grep)
  (global-set-key (kbd "C-c t") 'datetime)
  (global-set-key (kbd "C-c k") 'do-daily-task)
  (global-set-key [f6] 'emms-random)
  )

(defun custom-terminal-key-bindings ()
  (global-set-key (kbd "C-c ;") 'delete-other-windows)
  (global-set-key (kbd "C-c 8") 'eshell)
  (global-set-key (kbd "C-c 7") 'sql-mysql)
  (global-set-key (kbd "C-c '") 'winner-undo)
  (global-set-key (kbd "C-c \"") 'winner-redo)
  (global-set-key (kbd "C-c ,") 'jedi:goto-definition-pop-marker)
  (global-set-key (kbd "C-c .") 'jedi:goto-definition)
  (global-set-key (kbd "C-c }") 'tabbar-forward-group)
  (global-set-key (kbd "C-c {") 'tabbar-backward-group)
  (global-set-key (kbd "C-c >") 'tabbar-forward)
  (global-set-key (kbd "C-c <") 'tabbar-backward)
  (global-set-key (kbd "C-c g") 'my-find-grep)
  (global-set-key (kbd "C-c t") 'datetime)
  )

(global-set-key (kbd "C-x C-c") 'ask-before-closing)
(global-set-key (kbd "C-x j") 'set-mark-command)
(global-set-key (kbd "C-c j") 'set-mark-command)
(global-set-key (kbd "C-c i") 'toggle-input-method)

(global-set-key (kbd "C-c d") 'dired)
(global-set-key (kbd "C-c o") 'search-backward-regexp)

(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

(global-set-key (kbd "C-x C-\\") 'pop-global-mark)

(global-set-key (kbd "C-c f") 'flymake-mode)

;; (global-set-key (kbd "C-c H") 'hs-hide-all)
;; (global-set-key (kbd "C-c S") 'hs-show-all)
;; (global-set-key (kbd "C-c h") 'hs-hide-block)
;; (global-set-key (kbd "C-c s") 'hs-show-block)

(global-set-key [f11] 'my-fullscreen)
(global-set-key [f10] 'flymake-goto-prev-error)
(global-set-key [f12] 'flymake-goto-next-error)

;; (global-set-key [f9] 'shell-switcher-new-shell)
;; (global-set-key (kbd "M-[") 'shell-switcher-switch-buffer)
;; (global-set-key (kbd "M-]") 'shell-switcher-switch-buffer-other-window)

(global-set-key [(control tab)] 'my-indent-or-complete)

(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)
;; (global-set-key (kbd "C-c l") 'sdcv-search-pointer)
(global-set-key (kbd "C-c l") 'youdao-translate-word)

(provide 'my-key-bindings)
