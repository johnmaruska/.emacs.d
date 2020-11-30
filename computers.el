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

(defun gr-macbook? ()
  "Is current machine Guaranteed Rate Macbook Pro 15\" 2019?"
  (or (string= (system-name) "GR-YR2TSLVCHmbp.local")
      (string= (system-name) "GR-YR2TSLVCHmbp.attlocal.net")))

(defun convertible? ()
  "Is current machine Dell 2-in-1 Windows machine?"
  (string= (system-name) "WINDOWS-EONKBPI"))

(defun vingtor? ()
  "Is current machine Vingtor desktop running Windows?"
  (string= (system-name) "VINGTOR"))

(defun current-machine ()
  "Return English name for machine instead of `system-name`."
  (cond ((gr-macbook?)          "GR-MacBook")
        ((convertible?) "Convertible-Windows")
        ((vingtor?)     (system-name))))

;;;;;;;; Mac-only UI

(defun setup-windows ()
  (split-window-right -75)  ; create side-bar
  (other-window 1)  ; switch to sidebar
  (find-file "~/dev/work/troubleshooting/journal.md")  ; put journal in sidebar
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
  (setup-windows)
  (toggle-frame-fullscreen))


(provide 'computers)
;;; computers.el ends here
