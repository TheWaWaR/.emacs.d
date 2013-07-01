
(defun my-fullscreen ()
  (interactive)
  (when window-system
    (x-send-client-message
     nil 0 nil "_NET_WM_STATE" 32
     '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
  )


(defun my-maximized ()
  (interactive)
  (when window-system
    (x-send-client-message
     nil 0 nil "_NET_WM_STATE" 32
     '(1 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
  (interactive)
  (when window-system
    (x-send-client-message
     nil 0 nil "_NET_WM_STATE" 32
     '(1 "_NET_WM_STATE_MAXIMIZED_VERT" 0)))
  )


;;; Copy from grep.el.gz -> grep-find function
(defun my-find-grep (command-args)
  (interactive
   (progn
     (grep-compute-defaults)
     (setq file-regx (read-string "File regx: "))
     (setq str-regx (read-string "String regx: "))
     (setq my-grep-find-command
           (format "find . -type f \\( -name \"%s\" \\) -exec egrep -nH -e \"%s\" {} +"
                   file-regx str-regx))
     (if my-grep-find-command
	 (list (read-shell-command "Run find (like this): "
                                   my-grep-find-command 'grep-find-history))
       ;; No default was set
       (read-string
        "compile.el: No `grep-find-command' command available. Press RET.")
       (list nil))))
  (when command-args
    (let ((null-device nil))		; see grep
      (grep command-args))))


(defun make-eshell-prompt-function ()
  " 设置 esehll 提示符样式的函数.
    Create at: <2012-10-18 18:10:00>
    Finish at: <2012-10-18 23:24:53>
    Examples:
    ============================================================
    1. /                      --> (23:17) ➜ / $
    2. /etc                   --> (23:17) ➜ /etc $ 
    3. /home/weet             --> (23:17) ➜ ~ $ 
    4. /home/weet/Games       --> (23:17) ➜ ~/Games $ 
    5. /home/weet/Games/braid --> (23:17) ➜ Games/braid $
  "
  (interactive)
  (lambda ()
    (concat "(" (format-time-string "%H:%M") ") " "-> "
            (let ((eshell-pwd (eshell/pwd)) (home-dir (getenv "HOME")))
              (if (or (string= eshell-pwd home-dir) (string= eshell-pwd "~")) "~"
                ;; Not in HOME
                (let ((cur-path-list (cdr (split-string eshell-pwd "/")))
                      (home-path-list (cdr (split-string home-dir "/")))
                      )
                  (let ((cur-path-len (length cur-path-list)))
                    ;; (message (concat (number-to-string cur-path-len) ": "
                    ;;                  (car cur-path-list) "(...)"
                    ;;                  (car (last cur-path-list 1))))
                    (if (= cur-path-len 1)
                        (concat "/" (if (string< "" (car cur-path-list))
                                        (car cur-path-list)))
                      ;; Neither ( / ) nor ( /bar )
                      (concat
                       (if (and (= (- cur-path-len (length home-path-list)) 1)
                                (equal  (reverse  (cdr (reverse  cur-path-list))) home-path-list)) "~"
                         (nth (- cur-path-len 2) cur-path-list))
                       "/" (car (last cur-path-list))))))))
            (if (= (user-uid) 0) " # " " $ "))))


(defun clear-not-staged-delete-files (&rest args)
  (interactive)
  (setq rs (read-string "D: "))
  (message (concat rs (car args)))
  (get-buffer-create (concat "*" rs (car args) "*"))
  )
;; (clear-not-staged-delete-files "a")



(defun my-mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
           (or (and (eq last-command this-command) (mark t))
               (region-active-p)))
      (set-mark
       (save-excursion
         (when (< (mark) (point))
           (setq arg (- arg)))
         (goto-char (mark))
         (forward-word arg)
         (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
        (error "No word at point"))
      (if (>= arg 0)
          (goto-char (car wbounds))
        (goto-char (cdr wbounds)))
      (push-mark (save-excursion
                   (forward-word arg)
                   (point)))
      (activate-mark))))


(defun datetime ()
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %H:%M]"))
  )

(defun open-org-file (loc)
  (setq daily-dir (concat "/home/weet/Dropbox/Emacs/" loc "/"))
  (setq daily-file-name (concat (format-time-string "%Y-%m-%d") ".org"))
  (concat daily-dir daily-file-name)
  )

(defun do-daily-task ()
  (interactive)
  (find-file (open-org-file "daily-task")))

(defun do-diary ()
  (interactive)
  (find-file (open-org-file "diary")))



(defun alpha-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(85 80))
  )

(defun full-transparency ()
  (interactive)
  (set-frame-parameter (selected-frame) 'alpha '(96 80))
  )


(defun create-scratch-buffer nil
  "create a scratch buffer"
  (interactive)
  (kill-buffer "*scratch*")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (insert ";; Good day sir!")
  (lisp-interaction-mode))


(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 120))
          (add-to-list 'default-frame-alist (cons 'width 100)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist 
                     (cons 'height (/ (- (x-display-pixel-height) 300)
                                      (frame-char-height)))))))


(defun use-the-emms ()
  (require 'emms)
  (require 'emms-setup)
  (require 'emms-browser)
  (require 'emms-mode-line)
  (require 'emms-playing-time)
  (require 'emms-info-libtag)
  (emms-all)
  (emms-default-players)
  (emms-mode-line 1)
  (emms-playing-time 1)

  (setq emms-playlist-buffer-name "*Music*")
  (emms-add-directory-tree "/media/Apps/Archive/Music/")
  (setq emms-info-functions '(emms-info-libtag))
  (setq emms-use-scoring t)
  ;; (require 'init-emms)
  (require 'xwl-emms)
  )


(defun use-the-slime ()
  ;; slime
  (setq inferior-lisp-program "/usr/bin/clisp")
  (add-to-list 'load-path "/home/weet/.emacs.d/my-plugins/slime/")
  (add-to-list 'load-path "/home/weet/.emacs.d/my-plugins/slime/contrib/")
  (require 'slime)
  (slime-setup '(slime-fancy))
  )

(defun use-the-jedi ()
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:setup-keys t)                      ; optional
  (setq jedi:complete-on-dot t)                 ; optional
  (setq jedi:server-command
        (list "/usr/bin/python2" "/root/.emacs.d/emacs-starter-kit/elpa/jedi-0.1.2/jediepcserver.py"))
  )


(defun use-the-sdcv ()
  ;; 词典
  (require 'sdcv)
  (setq sdcv-dictionary-simple-list        ;; a simple dictionary list
        '(
          "懒虫简明英汉词典"
          "牛津英汉双解美化版"
          ))
  (setq sdcv-dictionary-complete-list      ;; a complete dictionary list
        '("KDic11万英汉词典"
          "牛津英汉双解美化版"
          "懒虫简明英汉词典"
          "朗道英汉字典5.0"
          "XDICT英汉辞典"
          "朗道汉英字典5.0"
          "XDICT汉英辞典"
          "懒虫简明汉英词典"
          "stardict1.3英汉辞典"
          "英汉汉英专业词典"
          "CDICT5英汉辞典"
          "Jargon"
          "FOLDOC"
          "WordNet"
          ))
  ;; (global-set-key (kbd "C-c l") 'sdcv-search-pointer)
  )


(defun tabbar-buffer-groups ()
  ;;   "Return the list of group names the current buffer belongs to.
  ;; This function is a custom function for tabbar-mode's tabbar-buffer-groups.
  ;; This function group all buffers into 3 groups:
  ;; Those Dired, those user buffer, and those emacs buffer.
  ;; Emacs buffer are those starting with “*”."
  (list
   (cond
    ((string-equal "*" (substring (buffer-name) 0 1)) '("Emacs Buffer"))
    ((eq major-mode 'dired-mode) '("Dired"))
    (t '("User Buffer"))
    )))

(defun use-the-tabbar ()
  (require 'tabbar)
  (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)
  (tabbar-mode t)
  (tabbar-local-mode t)
  
  ;;; 设置tabbar外观
  ;; 设置默认主题: 字体, 背景和前景颜色，大小
  (set-face-attribute 'tabbar-default nil
                      :family "DejaVu Sans Mono"
                      :background "#282828"
                      :foreground "#282828"
                      :height 0.95
                      )
  ;; 设置左边按钮外观：外框框边大小和颜色
  (set-face-attribute 'tabbar-button nil
                      :inherit 'tabbar-default
                      :box '(:line-width 1 :color "#303030")
                      )
  ;; 设置当前tab外观：颜色，字体，外框大小和颜色
  (set-face-attribute 'tabbar-selected nil
                      :inherit 'tabbar-default
                      :foreground "wheat"
                      :background "#3f3f3f"
                      :box '(:line-width 1 :color "#4f4f4f")
                      :overline "#282828"
                      :underline nil
                      :weight 'bold
                      )
  ;; 设置非当前tab外观：外框大小和颜色
  (set-face-attribute 'tabbar-separator nil
                      :background "#282828"
                      :foreground "gray70"
                      )
  (set-face-attribute 'tabbar-unselected nil
                      :inherit 'tabbar-default
                      :box '(:line-width 1 :color "#4f4f4f")
                      :overline "#282828"
                      :background "#303030"
                      :foreground "gray55"
                      ))

(defun use-the-eim ()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/my-plugins/viogus-eim/")
  ;;输入法
  (autoload 'eim-use-package "eim" "Another emacs input method")
  (setq eim-use-tooltip nil)
  (register-input-method
   "eim-py" "euc-cn" 'eim-use-package
   "拼音" "汉字拼音输入法" "py.txt")
  ;; 用 ; 暂时输入英文
  (require 'eim-extra)
  (global-set-key ";" 'eim-insert-ascii)
  (set-input-method "eim-py")             ; use Pinyin input method
  (eim-punc-translate-toggle nil)
  )

(defun use-the-erlang ()
  ;; Erlang mode
  (add-to-list 'load-path "/usr/lib/erlang/lib/tools-2.6.8/emacs/")
  (setq erlang-root-dir "/usr/lib/erlang")
  (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
  (require 'erlang-start)
  
  ;; Distel mode
  (add-to-list 'load-path "~/.emacs.d/my-plugins/distel/elisp")
  (require 'distel)
  (distel-setup)
  )

(defun use-the-git ()
  (interactive)
  (add-to-list 'load-path "~/.emacs.d/my-plugins/git/")  
  (require 'git)
  (require 'git-blame)
  )

(defun use-the-auto-complete ()
  (global-auto-complete-mode)
  (define-key ac-completing-map "\M-n" 'ac-next)  ;;; 列表中通过按M-n来向下移动
  (define-key ac-completing-map "\M-p" 'ac-previous)
  (define-key ac-mode-map (kbd "M-n") 'auto-complete)
  )


;;;; Eshell 命令绑定
;; cls 清屏   
(defun eshell/cls()   
  "to clear the eshell buffer."   
  (interactive)   
  (let ((inhibit-read-only t))   
    (erase-buffer)))

;; 打开文件
(defun eshell/em (files)
  (if (listp files)
      (progn
        (eshell-printn "Many Files:")
        (mapcar 'eshell-printn files)
        (let ((pwd (eshell/pwd)))
          (while files (find-file (concat pwd "/" (pop files))))
          ;; (mapcar '(lambda (file)
          ;;          (progn ;; Here is the problem caused !!!
          ;;            (eshell-printn file)
          ;;            (let ((abs-path (concat pwd "/" file)))
          ;;              (eshell-printn abs-path)
          ;;              (find-file abs-path)
          ;;              )
          ;;            (eshell-printn "DONE")
          ;;            ))
          ;;         files)
          ))
    (eshell-printn "One file:")
    (eshell-printn files)
    (find-file files)
    )
  )


;; (mapcar '(lambda  (item) (message (concat "-" item "-")))  '("AA" "BB" "CC"))
;; (setq alist '("A" "B"))
;; (elt ["A" "B" "C"] 1)


;; 调节音量
(defun eshell/v (opt)
  (shell-command (concat "amixer --quiet set Master " opt))
  )


(defun eshell/emo (file)
  (find-file-other-window file))

(provide 'my-defuns)
