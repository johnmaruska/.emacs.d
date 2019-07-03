;;; package -- Summary:

;; set environment variables like PATH and JAVA_HOME as required, which will
;; vary between systems.

;;; Commentary:

;;; Code:

;;; $PATH settings for subsystem processes and for eshell

(defvar default-path (getenv "PATH"))
(defvar climate-path-list
  (list "/usr/local/opt/bin/go/libexec/bin"
        (expand-file-name "~/bin")
        "/usr/local/bin"
        "/usr/local/sbin"
        (expand-file-name "~/.rvm/bin")
        (expand-file-name "~/.rvm/sbin")
        "/usr/texbin"
        "/Library/TeX/texbin"
        "/usr/bin"
        "/usr/sbin"
        "/bin"
        "/sbin"
        "/opt/pixie"
        "/opt/sbin"
        "/usr/local/bin/exercism-mac-64bit"
        "/usr/local/opt/postgresql@9.6/bin"
        (expand-file-name "~/climate/bin")
        (expand-file-name "~/.cargo/bin")   ; Rust
        (expand-file-name "~/gatling/bin")  ; load test
        (expand-file-name "~/Documents/apache-maven-3.5.4")
        default-path))
(defvar ubuntu-path-list (list default-path))
(defvar path-list)

(defun set-default-envvars ()
  "Set envvars to include default settings."
  (interactive)
  ;; set emacs path-list
  (setq path-list
        (cond ((eq system-type 'gnu/linux) ubuntu-path-list)
              ((eq system-type 'darwin) climate-path-list)
              (t (list default-path))))
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
