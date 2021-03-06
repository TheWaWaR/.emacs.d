;;; xwl-emms.el --- emms config file

;; Copyright (C) 2007, 2008 William Xu

;; Author: William Xu <william.xwl@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
;; 02110-1301 USA

;;; Code:

;;; Basic

(require 'emms-setup)
(emms-standard)
(require 'emms-i18n)
(require 'emms-history)
(emms-devel)
;; (emms-lastfm-enable)


(setq emms-directory "~/.emacs.d/my-plugins/Emms/")                    ;设置EMMS的目录
(setq emms-history-file "~/.emacs.d/my-plugins/Emms/history")          ;播放列表历史记录
(setq emms-cache-file "~/.emacs.d/my-plugins/Emms/cache")              ;缓存文件
(setq emms-stream-bookmarks-file "~/.emacs.d/my-plugins/Emms/streams") ;网络电台保存文件
(setq emms-score-file "~/.emacs.d/my-plugins/Emms/scores")             ;分数文件
(setq my-lyrics-directory "~/.emacs.d/my-plugins/Emms/lyrics")         ;歌词目录


(setq emms-player-mpg321-command-name "mpg123"
      emms-player-mplayer-command-name "mplayer"
      emms-player-mplayer-parameters
      (append emms-player-mplayer-parameters
              ;; '( "-font" "Simsun") ;"/usr/share/fonts/truetype/microsoft/simsun.ttf")
              ;; "-idx"
              ;; '("-subcp" "cp936")
              )
      emms-player-list '(emms-player-mplayer
                         emms-player-mplayer-playlist
                         emms-player-ogg123
                         emms-player-mpg321
                         ))

(setq emms-info-mp3info-coding-system 'gbk
      emms-cache-file-coding-system 'utf-8
      emms-i18n-default-coding-system '(utf-8 . utf-8)
      )

(add-to-list 'file-coding-system-alist '("~/[mM]usic/.*" gbk . gbk))

(setq emms-playlist-buffer-name "*EMMS Playlist*"
      emms-source-file-default-directory "/media/Apps/Archive/Music/"
      emms-playing-time-style 'bar
      emms-info-asynchronously nil)

;(when (string-match "darwin" (xwl-os-type))
;  (setq emms-source-file-gnu-find "find"))

(when (file-exists-p emms-source-file-gnu-find)
  (setq emms-source-file-directory-tree-function
        'emms-source-file-directory-tree-find))

(set-face-foreground 'emms-playlist-selected-face "magenta")
;(set-face-foreground 'emms-playlist-track-face  "green")

;; (add-hook 'emms-player-started-hook 'emms-show)

(global-set-key (kbd "<f3>")
                (lambda ()
                  (interactive)
                  (if emms-playlist-buffer
                      (emms-playlist-mode-go)
                      ;; (emms-playlist-mode-go-popup)
                    (message "EMMS not started"))))


(setq emms-volume-amixer-control "PCM")

(global-set-key (kbd "M-0") 'emms-volume-raise)
(global-set-key (kbd "M-9") 'emms-volume-lower)

(setq emms-repeat-playlist nil)

(setq emms-playlist-mode-window-width (* 0.3 (frame-width)))

(add-hook 'emms-playlist-limit-hook
          'emms-playlist-sort-by-natural-order)

;; ;; Rebind sort map prefix key
;; (add-hook 'emms-playlist-mode-hook
;;           '(lambda ()
;;              (local-unset-key "s")
;;              (setq emms-playlist-sort-prefix "s")
;;              (emms-playlist-sort-map-setup)))

;; Update track info in playlist buffer when finished.

(add-hook 'emms-player-finished-hook
          (lambda ()
            (with-current-emms-playlist
              (save-excursion
                (emms-playlist-mode-center-current)
                (emms-playlist-update-track)))))

;; Mode Line Format
(setq emms-mode-line-format "[ %s ]"
      emms-lyrics-display-format "%s"
      emms-playing-time-display-format "%s")

(require 'emms-mode-line-icon)


;;; Track Show Format (for playlist buffer)

(setq emms-last-played-format-alist
      '(((emms-last-played-seconds-today) . "%a %H:%M")
        (604800                           . "%a %H:%M") ; this week
        ((emms-last-played-seconds-month) . "%d")
        ((emms-last-played-seconds-year)  . "%m/%d")
        (t                                . "%Y/%m/%d")))

(eval-after-load 'emms
  '(progn
     (setq xwl-emms-playlist-last-track nil)
     (setq xwl-emms-playlist-last-indent "\\")

     (defun xwl-emms-track-description-function (track)
       "Return a description of the current track."
       (let* ((name (emms-track-name track))
              (type (emms-track-type track))
              (short-name (file-name-nondirectory name))
              (play-count (or (emms-track-get track 'play-count) 0))
              (last-played (or (emms-track-get track 'last-played) '(0 0 0)))
              (empty "..."))
         (prog1
             (case (emms-track-type track)
               ((file url)
                (let* ((artist (or (emms-track-get track 'info-artist) empty))
                       (year (emms-track-get track 'info-year))
                       (playing-time (or (emms-track-get track 'info-playing-time) 0))
                       (min (/ playing-time 60))
                       (sec (% playing-time 60))
                       (album (or (emms-track-get track 'info-album) empty))
                       (tracknumber (emms-track-get track 'info-tracknumber))
                       (short-name (file-name-sans-extension
                                    (file-name-nondirectory name)))
                       (title (or (emms-track-get track 'info-title) short-name))

                       ;; last track
                       (ltrack xwl-emms-playlist-last-track)
                       (lartist (or (and ltrack (emms-track-get ltrack 'info-artist))
                                    empty))
                       (lalbum (or (and ltrack (emms-track-get ltrack 'info-album))
                                   empty))

                       (same-album-p (and (not (string= lalbum empty))
                                          (string= album lalbum))))
                  (format "%10s  %3d   %-20s%-60s%-35s%-15s%s"
                          (emms-last-played-format-date last-played)
                          play-count
                          ;;                           (if (and (not (string= lartist empty))
                          ;;                                    (string= artist lartist))
                          ;;                               empty
                          ;;                             artist)
                          artist

                          ;; Combine indention, tracknumber, title.
                          ;; (format "%s%s%-40s"
                          (concat
                           (if same-album-p ; indention by album
                               (setq xwl-emms-playlist-last-indent
                                     (concat " " xwl-emms-playlist-last-indent))
                             (setq xwl-emms-playlist-last-indent "\\")
                             "")
                           (if (and tracknumber ; tracknumber
                                    (not (zerop (string-to-number tracknumber))))
                               (format "%02d." (string-to-number tracknumber))
                             "")
                           title        ; title
                           )

                          ;; album
                          (cond ((string= album empty) empty)
                                ;; (same-album-p "  ")
                                (t (concat "《" album "》")))

                          (or year empty)
                          (if (or (> min 0)  (> sec 0))
                              (format "%02d:%02d" min sec)
                            empty))))
               ((url)
                (concat (symbol-name type)
                        ":"
                        (decode-coding-string
                         (encode-coding-string name 'utf-8)
                         'gbk)))
               (t
                (format "%-3d%s"
                        play-count
                        (concat (symbol-name type) ":" name))))
           (setq xwl-emms-playlist-last-track track))))

     (setq emms-track-description-function
           'xwl-emms-track-description-function)
     ))

;; Redefine this to use name only
(defun xwl-emms-mode-line-playlist-current ()
  "Format the currently playing song."
  (let* ((track (emms-playlist-current-selected-track))
         (type (emms-track-type track))
         (name (emms-track-name track))
         (artist (emms-track-get track 'info-artist))
         (title (emms-track-get track 'info-title)))
    (format "[ %s ]"
            (cond ((and artist title)
                   (concat artist " - " title))
                  (title
                   title)
                  ((eq type 'file)
                   (file-name-sans-extension (file-name-nondirectory name)))
                  (t
                   (concat (symbol-name type) ":" name))))))

(setq emms-mode-line-mode-line-function
      'xwl-emms-mode-line-playlist-current)

(setq emms-mode-line-titlebar-function nil)
;;      'xwl-emms-mode-line-playlist-current)


;;; Key Bindings

(defun xwl-emms-playlist-mode-hook ()
  (toggle-truncate-lines 1))

(add-hook 'emms-playlist-mode-hook 'xwl-emms-playlist-mode-hook)

(define-key emms-playlist-mode-map (kbd "x") 'emms-start)
(define-key emms-playlist-mode-map (kbd "v") 'emms-stop)
(define-key emms-playlist-mode-map (kbd "h") 'emms-shuffle)
(define-key emms-playlist-mode-map (kbd "o") 'emms-show)
(define-key emms-playlist-mode-map (kbd "F") 'emms-playlist-show-current-line)
(define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
(define-key emms-playlist-mode-map (kbd "r") 'emms-toggle-repeat-track)
(define-key emms-playlist-mode-map (kbd "R") 'emms-toggle-repeat-playlist)
(define-key emms-playlist-mode-map (kbd "q") 'next-buffer) ; 'delete-window)
(define-key emms-playlist-mode-map (kbd "<left>")  (lambda () (interactive) (emms-seek -10)))
(define-key emms-playlist-mode-map (kbd "<right>") (lambda () (interactive) (emms-seek +10)))
(define-key emms-playlist-mode-map (kbd "<down>")  (lambda () (interactive) (emms-seek -60)))
(define-key emms-playlist-mode-map (kbd "<up>")    (lambda () (interactive) (emms-seek +60)))

(define-key emms-playlist-mode-map (kbd "N") 'emms-next)
(define-key emms-playlist-mode-map (kbd "P") 'emms-previous)
(define-key emms-playlist-mode-map (kbd "E") 'emms-tag-editor-edit)

;; emms dired
(define-key emms-playlist-mode-map (kbd "n") 'next-line)
(define-key emms-playlist-mode-map (kbd "p") 'previous-line)
(define-key emms-playlist-mode-map (kbd "d") 'emms-playlist-mode-delete-selected-track)
;; (define-key emms-playlist-mode-map (kbd "D") 'emms-playlist-mode-delete)
;; (define-key emms-playlist-mode-map (kbd "m") 'emms-playlist-mode-mark)
;; (define-key emms-playlist-mode-map (kbd "u") 'emms-playlist-mode-unmark)

;; (global-set-key (kbd "C-c e x")   'emms-start)
;; (global-set-key (kbd "C-c e v")   'emms-stop)
;; (global-set-key (kbd "C-c e n")   'emms-next)
;; (global-set-key (kbd "C-c e p")   'emms-previous)
;; (global-set-key (kbd "C-c e o")   'emms-show)
;; (global-set-key (kbd "C-c e h")   'emms-shuffle)
;; (global-set-key (kbd "C-c e SPC") 'emms-pause)
;; (global-set-key (kbd "C-c e f")   'emms-no-next)
;; (global-set-key (kbd "C-c e F")   'emms-no-next-and-sleep)
;; (global-set-key (kbd "C-c e t")   'emms-add-directory-tree)
;; (global-set-key (kbd "C-c e d")   'emms-playlist-mode-delete-selected-track)
;; (global-set-key (kbd "C-c e r")   'emms-toggle-repeat-track)
;; (global-set-key (kbd "C-c e R")   'emms-toggle-repeat-playlist)
;; (global-set-key (kbd "C-c e l")   'emms-lyrics-visit-lyric)

;; (global-set-key (kbd "C-c e s")   'emms-lastfm-radio-similar-artists)
;; (global-set-key (kbd "C-c e k")   'emms-lastfm-radio-skip)


;;; Play Some Tracks, Then Stop

;; Repeat track N times, then stop.
(setq xwl-emms-no-next-p -1)
(setq xwl-sleep-p nil)                  ; make ibook go sleep

(defun emms-no-next (&optional n)
  "Repeat track N times, then stop."
  (interactive "P")
  (unless n (setq n 0))
  (setq xwl-emms-no-next-p n)
  (message "Will repeat track %d times, then stop" n))

(defun emms-no-next-and-sleep ()
  "Run `emms-no-next' first, then make ibook go to sleep."
  (interactive)
  (call-interactively 'emms-no-next)
  (if (y-or-n-p "Let ibook go sleep when EMMS finishes? ")
    (progn
      (setq xwl-sleep-p t)
      (message "Will repeat track desired times, then LET IBOOK GO TO SLEEP!"))
    (message "Will repeat track desired times, then stop")))

(defun xwl-emms-next-noerror ()
  "Wrap `emms-next-noerror' with `xwl-emms-no-next-p' check."
  (interactive)
  (cond ((> xwl-emms-no-next-p 0)
         (setq xwl-emms-no-next-p (1- xwl-emms-no-next-p))
         (emms-start))
        ((= xwl-emms-no-next-p 0)
         (setq xwl-emms-no-next-p -1)
         (emms-stop)
         (when xwl-sleep-p
           (setq xwl-sleep-p nil)
           (shell-command "sudo /sbin/snooze")))
        (t
         (emms-next-noerror))))

(setq emms-player-next-function 'xwl-emms-next-noerror)

(add-hook 'emms-player-stopped-hook
          '(lambda ()
             (setq xwl-emms-no-next-p -1
                   xwl-sleep-p nil)))


;;; Lyrics Crawler (not finished yet)

;; (setq emms-lyrics-find-lyric-function 'xwl-emms-lyrics-find-lyric)

(setq lyrics-crawler-running-p t)
(setq lyrics-crawler-dir "~/Music/lyrics/bycrawler/")
(setq lyrics-crawler-process nil)
(setq lyrics-crawler-buffer "*Lyrics Crawler*")

(defun lyrics-crawler-toggle ()
  (interactive)
  (setq lyrics-crawler-running-p (not lyrics-crawler-running-p))
  (if lyrics-crawler-running-p
      (message "lyrics crawler enabled")
    (message "lyrics crawler disabled")))

;; (global-set-key (kbd "C-c e L") 'xwl-emms-lyrics-find-lyric)

(defun xwl-emms-lyrics-find-lyric (file)
  "Download lrc FILE from baidu when `emms-lyrics-find-lyric' returns nil."
  ;; Note: FILE is ignored at the moment
  (let* ((track (emms-playlist-current-selected-track))
         (name (emms-track-get track 'name))
         (artist (emms-track-get track 'artist))
         (new-title (or (emms-track-get track 'info-title)
                        (replace-regexp-in-string
                         (concat "\\." (file-name-extension file) "\\'")
                         ""
                         file)))
         (new-artist (if (emms-track-get track 'info-artist)
                         (concat (emms-track-get track 'info-artist) "_")
                       "")))
    (or (emms-lyrics-find-lyric file)   ; name.lrc
        (emms-lyrics-find-lyric         ; title.lrc
         (format "%s.lrc" new-title))
        (emms-lyrics-find-lyric         ; artist_title.lrc
         (format "%s_%s.lrc" new-artist new-title))
        ;;         (and lyrics-crawler-running-p
        ;;              (with-current-buffer (get-buffer-create lyrics-crawler-buffer)
        ;;                (goto-char (point-max))
        ;;                (insert (concat cmd "\n"))
        (shell-command
         (format "cd ~/music/lyrics/bycrawler && ~/bin/lyrics-crawler.scm %s && cd -"
                 new-title)))))

;;                  (set-process-sentinel lyrics-crawler-process
;;                                        'lyrics-crawler-process-sentinel)
;;                  nil))))))

(defun lyrics-crawler-process-sentinel (process event)
  (case (process-status process)
    ((exit)
     (let* ((track (emms-playlist-current-selected-track))
            (name (emms-track-get track 'name))
            (new-title (or (emms-track-get track 'info-title)
                           (replace-regexp-in-string
                            (concat "\\." (file-name-extension name) "\\'")
                            ""
                            (file-name-nondirectory name))))
            (new-artist (if (emms-track-get track 'info-artist)
                            (concat (emms-track-get track 'info-artist) "_")
                          ""))
            (lrc-base (format "%s%s"
                              lyrics-crawler-dir
                              (if (string-match "/$" lyrics-crawler-dir) "" "/")))
            (lrc0 (format "%s%s"
                          lrc-base
                          (format "%s%s.lrc" new-artist new-title)))
            (lrc1 (format "%s%s"
                          lrc-base
                          (format "%s.lrc" new-title))))
       (cond ((file-exists-p lrc0)
              (emms-lyrics-catchup lrc0)
              (message "lyrics downloaded successfully"))
             ((file-exists-p lrc1)
              (emms-lyrics-catchup lrc1)
              (message "lyrics downloaded successfully"))
             (t
              (with-current-buffer
                  (get-buffer-create lyrics-crawler-buffer)
                (goto-char (point-max))
                (insert "downloading lyrics failed\n"))
              (message "downloading lyrics failed")))))
    ((signal)
     (with-current-buffer
         (get-buffer-create lyrics-crawler-buffer)
       (goto-char (point-max))
       (insert "downloading lyrics killed\n"))
     (message "lyrics crawler killed"))
    (t
     (with-current-buffer
         (get-buffer-create lyrics-crawler-buffer)
       (goto-char (point-max))
       (insert "downloading lyrics failed\n"))
     (message "downloading lyrics failed"))))

(defun emms-lyrics-save-w3m-buffer ()
  "Save *w3m* buffer as lyrics for the track."
  (interactive)
  (let ((w3m-buf (get-buffer "*w3m*")))
    (when w3m-buf
      (let* ((track (emms-playlist-current-selected-track))
             (name (emms-track-get track 'name)))
        (with-current-buffer w3m-buf
          (write-file
           (emms-replace-regexp-in-string
            (concat "\\." (file-name-extension name) "\\'")
            ".lrc"
            name))))
      (message "saving w3m lyrics buffer done"))))

;; (global-set-key (kbd "C-c e S") 'emms-lyrics-save-w3m-buffer)

(defadvice emms-lyrics-visit-lyric (around use-w3m-to-browse)
  "Use w3m to browse."
  (let ((browse-url-browser-function 'w3m-browse-url))
    ad-do-it))

(ad-activate 'emms-lyrics-visit-lyric)


;;; Misc

(defun emms-playlist-mode-jump ()
  "Jump to the directory of track at point in `emms-playlist-buffer'."
  (interactive)
  (let ((name
         (emms-track-get (emms-playlist-track-at) 'name)))
    (dired (file-name-directory name))
    (goto-char (point-min))
    (dired-search-forward (file-name-nondirectory name) nil t)))

(define-key emms-playlist-mode-map (kbd "C-x C-j") 'emms-playlist-mode-jump)

(defun emms-playlist-mode-delete-selected-track ()
  "Delete selected(playing) track."
  (interactive)
  (with-current-buffer emms-playlist-buffer
    (emms-playlist-ensure-playlist-buffer)
    (widen)
    (when (y-or-n-p "Delete selected(playing) track ")
      (emms-with-inhibit-read-only-t
       (save-excursion
         (emms-playlist-mode-center-current)
         (let ((track (emms-playlist-current-selected-track))
               (kill-whole-line t))
           (goto-char (line-beginning-position))
           (kill-line)
           (delete-file (emms-track-get track 'name)))
         (emms-playlist-mode-play-smart))))))

(defun emms-playlist-mode-copy-filename-as-kill ()
  "Like `dired-copy-filename-as-kill'."
  (interactive)
  (save-window-excursion
    (emms-playlist-mode-jump)
    (dired-copy-filename-as-kill 0)))

(define-key emms-playlist-mode-map (kbd "w") 'emms-playlist-mode-copy-filename-as-kill)


;;; mp3 crawler from http://mp3.baidu.com

(require 'wget)

(defun xwl-mp3-crawler (title)
  "Download mp3 with TITLE from http://mp3.baidu.com."
  (interactive "sTitle: ")
  (let* ((urlencoded-title (emms-url-quote-plus
                            (emms-i18n-iconv 'utf-8 'gbk title)))
         (url1 (concat "http://mp3.baidu.com/m?f=ms&rn=&tn=baidump3&ct=134217728&word="
                       urlencoded-title
                       "&lm=0")))
    (url-retrieve url1 'xwl-mp3-crawler-url1-callback (list title))))

(defun xwl-mp3-crawler-url1-callback (status title)
  (let (url2)
    (goto-char (point-min))
    (search-forward "<td class=tdn>" nil t 1)
    (re-search-forward "href=\"\\([^\"]+\\)\"" nil t 1)
    (setq url2 (replace-regexp-in-string
                "\\cc" ; This is baidu's trick, we can simply ignore non-ascii texts.
                ""
                (emms-i18n-iconv 'gbk 'utf-8 (match-string 1))))
    ;; hexify/urlencode reserved characters
    (mapc (lambda (i)
            (setq url2 (replace-regexp-in-string
                        i (url-hexify-string i) url2)))
          '(";" " "))
    (url-retrieve url2 'xwl-mp3-crawler-url2-callback (list title))
    (kill-buffer (current-buffer))))

(defun xwl-mp3-crawler-url2-callback (status title)
  (let (url3)
    (goto-char (point-min))
    (search-forward "<li class=\"li\" style=\"margin-right:10px;\">" nil t 1)
    (re-search-forward "href=\"\\([^\"]+\\)\"" nil t 1)
    (setq url3 (match-string 1))
    (let ((wget-default-options
           (append wget-default-options
                   (list "-O" (concat (replace-regexp-in-string " " "_" title)
                                      ".mp3")))))
      (wget url3))
    (kill-buffer (current-buffer))))

(provide 'xwl-emms)

;;; xwl-emms.el ends here
