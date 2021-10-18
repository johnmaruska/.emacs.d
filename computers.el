;;; package -- Summary:

;; Collection of predicate functions to determine which machine is currently
;; running Emacs for dynamic configuration.

;;; Commentary:

;; I have too many different windows-nt machines.  Only one gnu/linux and one
;; Darwin.  If it weren't for the windows-nt machines I could just use
;; system-type.

;;; Code:

(defun cygwin? ()
  "Iff this is Cygwin, basically Unix in Windows."
  (eq system-type 'cygwin))
(defun darwin? ()
  "Iff this is Darwin (OSX)."
  (eq system-type 'darwin))
(defun linux? ()
  "Iff this is Linux."
  (eq system-type 'gnu/linux))
(defun windows? ()
  "Iff this is windows-nt, basically raw Windows."
  (eq system-type 'windows-nt))

(defun macbook? ()
  "Is current machine Guaranteed Rate Macbook Pro 15\" 2019?"
  (string-prefix-p "3071-AL-05039" (system-name)))

(defun convertible? ()
  "Is current machine Dell 2-in-1 Windows machine?"
  ;; need to name the PC
  (string= (system-name) "pop-os"))

(defun vingtor? ()
  "Is current machine Vingtor desktop running Windows?"
  (string= (system-name) "VINGTOR"))

(defun current-machine ()
  "Return English name for machine instead of `system-name`."
  (cond ((macbook?)     "macbook")
        ((convertible?) "convertible")
        ((vingtor?)     "vingtor")))

;;;;;;;; Mac-only UI

(defun setup-windows ()
  (split-window-right -75)  ; create side-bar
  (other-window 1)  ; switch to sidebar
  (find-file "~/washu/notes/journal.org")  ; put journal in sidebar
  ;; (split-window-below 25)  ; vertical split sidebar
  ;; (other-window 1)  ; switch to lower window in sidebar
  ;; (find-file "~/dev/work/troubleshooting/TODO.org")
  (other-window 1)
  (switch-to-buffer "*dashboard*"))

(defun setup-main-display-frame ()
  (let ((f (make-frame)))
    (set-frame-height   f 1080 nil 't)
    (set-frame-width    f 1920 nil 't)
    (set-frame-position f 2880 0))
  (delete-other-frames))

(defun setup-mac-displays ()
  (setup-main-display-frame)
  (toggle-frame-fullscreen)
  (setup-windows))


(provide 'computers)
;;; computers.el ends here
