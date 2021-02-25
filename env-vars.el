;;; package -- Summary:

;; set environment variables like PATH and JAVA_HOME as required, which will
;; vary between systems.

;;; Commentary:

;;; Code:

(defvar default-path (getenv "PATH"))

(defvar macbook-path-list
  (list "/usr/local/bin"
        default-path))

(defvar vingtor-path-list
  (list "C:/Program Files/LOVE"
        default-path))

(defvar path-list
  (cond ((macbook?) macbook-path-list)
        ((vingtor?) vingtor-path-list)
        (t          (list default-path))))

(require 'computers "~/.emacs.d/computers.el")
(defun set-default-envvars ()
  "Set envvars to include default settings."
  (interactive)
  (setenv "PAGER" "cat")
  (when window-system
    ;; this matters for sub-shell process (e.g. launch bash)
    (setenv "PATH" (mapconcat 'identity path-list (if (vingtor?) ";" ":")))
    (setenv "JAVA_HOME" "/usr/local/Cellar/openjdk/15.0.1"))
  ;; this matters for eshell
  (setq exec-path (append exec-path path-list)))


(provide 'env-vars)
;;; env-vars.el ends here
