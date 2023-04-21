;;; package -- Summary:

;; set environment variables like PATH and JAVA_HOME as required, which will
;; vary between systems.

;;; Commentary:

;;; Code:

(defvar default-path (getenv "PATH"))

(defvar macbook-path-list)
(setq macbook-path-list
      (list  "/opt/homebrew/bin"
             "/usr/local/bin"
             "~/.jenv/shims"
             "~/.serverless/bin"
             "~/Library/Python/3.9/bin"
             "/opt/homebrew/Cellar/jenv/0.5.4/libexec/libexec"
             default-path))

(defvar vingtor-path-list
  (list "~/bin"
        default-path))

(require 'computers "~/.emacs.d/computers.el")
(defvar path-list)
(setq path-list
      (cond ((macbook?) macbook-path-list)
            ((vingtor?) vingtor-path-list)
            (t          (list default-path))))

(defvar ORIGINAL_EXEC_PATH exec-path)

(defun set-default-envvars ()
  "Set envvars to include default settings."
  (interactive)
  (setenv "PAGER" "cat")
  (when window-system
    ;; this matters for sub-shell process (e.g. launch bash)
    (setenv "PATH" (mapconcat 'identity path-list (if (vingtor?) ";" ":"))))
  ;; this matters for eshell
  (setq exec-path (append path-list ORIGINAL_EXEC_PATH)))


(provide 'env-vars)
;;; env-vars.el ends here
