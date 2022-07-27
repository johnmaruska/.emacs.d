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
  "Is current machine work macbook?"
  ;; I only have one Mac so any MacOS means macbook
  (darwin?))

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

(defun setup-main-display-frame ()
  (let ((f (make-frame)))
    (set-frame-height   f 1080 nil 't)
    (set-frame-width    f 1920 nil 't)
    (set-frame-position f 2880 0))
  (delete-other-frames))

(defun setup-mac-displays ()
  (setup-main-display-frame)
  (toggle-frame-fullscreen))


(provide 'computers)
;;; computers.el ends here
