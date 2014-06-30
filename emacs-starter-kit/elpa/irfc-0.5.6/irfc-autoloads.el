;;; irfc-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (irfc-visit irfc-follow irfc-mode irfc-assoc-mode
;;;;;;  irfc) "irfc" "irfc.el" (21327 16816 968610 64000))
;;; Generated autoloads from irfc.el

(let ((loads (get 'irfc 'custom-loads))) (if (member '"irfc" loads) nil (put 'irfc 'custom-loads (cons '"irfc" loads))))

(defvar irfc-assoc-mode nil "\
If non-nil, RFC documents are associated with `irfc-mode'.
Default is nil.")

(custom-autoload 'irfc-assoc-mode "irfc" nil)

(autoload 'irfc-mode "irfc" "\
Major mode for IETF RFC documents.

\(fn)" t nil)

(autoload 'irfc-follow "irfc" "\
Open RFC document around point.
Download and open RFC document if it
does not exist in `irfc-directory'.

\(fn)" t nil)

(autoload 'irfc-visit "irfc" "\
Open RFC document RFC-NUMBER.
Download and open RFC document if it
does not exist in `irfc-directory'.

\(fn &optional RFC-NUMBER)" t nil)

;;;***

;;;### (autoloads nil nil ("irfc-pkg.el") (21327 16817 96371 561000))

;;;***

(provide 'irfc-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; irfc-autoloads.el ends here
