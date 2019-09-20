;;; package -- Summary:

;; set environment variables like PATH and JAVA_HOME as required, which will
;; vary between systems.

;;; Commentary:

;;; Code:

(defvar path-list (list (getenv "PATH")))

(defun set-default-envvars ()
  "Set envvars to include default settings."
  (interactive)
  (setenv "PAGER" "cat")
  (when window-system
    ;; this matters for sub-shell process (e.g. launch bash)
    (setenv "PATH" (mapconcat 'identity path-list ":")))
  (when (eq system-type 'darwin)
    (setenv "JAVA_HOME"
            "/Library/Java/JavaVirtualMachines/jdk1.8.0_162.jdk/Contents/Home"))
  ;; this matters for eshell
  (setq exec-path (append exec-path path-list)))


(provide 'env-vars)
;;; env-vars.el ends here
