;;; package -- Summary:

;; set environment variables like PATH and JAVA_HOME as required, which will
;; vary between systems.

;;; Commentary:

;;; Code:

(defvar default-path-list (getenv "PATH"))

(defvar gr-path-list
  (list "/usr/local/bin"
        (expand-file-name "~/bin")
        (expand-file-name "~/dev/work/kubeclj")
        "/usr/local/opt/python@3.7/bin"
        "/usr/local/opt/mongodb-community@3.6/bin"
        default-path-list))

(defvar vingtor-path-list
  (list "C:/Python38"
        "C:/Python38/Scripts"
        "C:/Program Files/Racket"
        default-path-list))

(defvar path-list)

(require 'computers "~/.emacs.d/computers.el")
(defun set-default-envvars ()
  "Set envvars to include default settings."
  (interactive)
  (setenv "PAGER" "cat")
  (setq path-list
        (cond ((gr-macbook?) gr-path-list)
              ((vingtor?) vingtor-path-list)
              (t             (list default-path-list))))
  (when window-system
    ;; this matters for sub-shell process (e.g. launch bash)
    (setenv "PATH" (mapconcat 'identity path-list (if (vingtor?) ";" ":")))
    (setenv "JAVA_HOME" "/usr/local/Cellar/openjdk/15.0.1"))
  (when (eq system-type 'darwin)
    (setenv "PYTHON" "/usr/bin/python"))
  ;; this matters for eshell
  (setq exec-path (append exec-path path-list)))


(provide 'env-vars)
;;; env-vars.el ends here
