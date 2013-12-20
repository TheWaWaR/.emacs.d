
;;; [2013-08-04 10:30] Created
;;; [2013-08-05 00:32] Basic completed

;;; TODO:
;;; =====
;; . 无法处理复数单词
;; . 单词的解释很长却没有换行
;; . 没有例句
;; . 未命名的 Async Shell Command *Buffer*
;; . 当启动多个 Emacs 实例时, 只存在一个 dict_server.py
;; . 当最后一个 Emacs 退出时, 杀死 dict_server.py


(defvar current-path (file-name-directory (or load-file-name buffer-file-name)))  ; Current file's path
(defvar dict-server-script (concat current-path "dict_server.py"))
(defvar dict-client-script (concat current-path "dict_client.py"))

(defun start-dict-server ()
  (async-shell-command (concat "python " dict-server-script)))


(defun get-current-word ()
  "Get the word to translate."
  (save-excursion
    (when (not mark-active)
      (forward-word)
      (backward-word)
      (mark-word))
    (buffer-substring-no-properties
     (region-beginning)
     (region-end))))


(defun lookup ()
  (interactive)
  (popup-tip (substring
              (let ((word (get-current-word)))
                (message word)
                (shell-command-to-string
                 (format "python %s %s" dict-client-script word)))
              0 -1)))


(provide 'my-dict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Just for test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; @weet [2013-08-03 08:35]
(shell-command-to-string "ls -Shla")
(format "<~[ %s %s ]~>" "The moon" "forgets")
(mapconcat (lambda (item) (upcase item))
           '("python" "elisp" "javascript") "; ")
;;; @weet [2013-08-03 08:35]

