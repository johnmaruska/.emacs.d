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

(defun convertible-windows? ()
  "Is current machine Dell 2-in-1 Windows machine?"
  (string= (system-name) "WINDOWS-EONKBPI"))

(defun vingtor-windows? ()
  "Is current machine Vingtor desktop running Windows?"
  (string= (system-name) "DESKTOP-OSSHNUC"))

(defun vingtor-ubuntu? ()
  "Is current machine Vingtor desktop running Ubuntu?"
  (string= (system-name) "TODO-implement"))

(defun current-machine ()
  "Return English name for machine instead of `system-name`."
  (cond ((gr-macbook?)          "GR-MacBook")
        ((convertible-windows?) "Convertible-Windows")
        ((vingtor-windows?)     "Vingtor-Windows")
        ((vingtor-ubuntu?)      "Vingtor-Ubuntu")))


(provide 'computers)
;;; computers.el ends here
