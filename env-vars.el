;;; package -- Summary:

;; set environment variables like PATH and JAVA_HOME as required, which will
;; vary between systems.

;;; Commentary:

;;; Code:

(defvar default-path (getenv "PATH"))

(defvar macbook-path-list
  (list "/usr/local/bin"
        "/usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin"
        "/Users/maruska/Library/Python/3.8/bin"
        default-path))

(defvar vingtor-path-list
  (list "C:/Program Files/LOVE"
        default-path))

(require 'computers "~/.emacs.d/computers.el")
(defvar path-list
  (cond ((macbook?) macbook-path-list)
        ((vingtor?) vingtor-path-list)
        (t          (list default-path))))

(defun set-default-envvars ()
  "Set envvars to include default settings."
  (interactive)
  (setenv "PAGER" "cat")
  (when window-system
    ;; this matters for sub-shell process (e.g. launch bash)
    (setenv "PATH" (mapconcat 'identity path-list (if (vingtor?) ";" ":")))
    (setenv "JAVA_HOME" "/Users/maruska/.jenv/versions/11"))
  ;; this matters for eshell
  (setq exec-path (append exec-path path-list)))


(provide 'env-vars)
;;; env-vars.el ends here
