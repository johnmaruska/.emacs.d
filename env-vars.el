;;; package -- Summary:

;; set environment variables like PATH as required, which will vary
;; between systems.

;;; Commentary:

;;; Code:
(defvar default-path (getenv "PATH"))

(defvar macbook-path-list)
(setq macbook-path-list
      (list
       "/nix/var/nix/profiles/default/bin/"  ; nix-blank commands
       "/Users/johnmaruska/.nix-profile/bin/"  ; installed by nix-env
       "/Users/johnmaruska/.sdkman/candidates/java/current/bin"
       "/opt/homebrew/bin"
       "/usr/local/bin"
       "~/.serverless/bin"
       "/opt/homebrew/Cellar/nvm/0.39.5"  ;
       "~/Library/Python/3.9/bin"
       default-path))

(defvar vingtor-path-list
  (list "~/bin"
        "C:\\Users\\jackm"
        default-path))

(require 'computers "~/.emacs.d/computers.el")
(defvar path-list)
(setq path-list
      (cond ((string= "macbook" MY--current-machine) macbook-path-list)
            ((string= "vingtor" MY--current-machine) vingtor-path-list)
            (t                                   (list default-path))))

(defvar MY--ORIGINAL_EXEC_PATH exec-path)

(defun set-default-envvars ()
  "Set envvars to include default settings."
  (interactive)
  (setenv "PAGER" "cat")
  (when window-system
    ;; this matters for sub-shell process (e.g. launch bash)
    (setenv "PATH"
            (mapconcat 'identity
                       path-list
                       (if (string= "vingtor" MY--current-machine) ";" ":"))))
  ;; this matters for eshell
  (setq exec-path (append path-list MY--ORIGINAL_EXEC_PATH)))

(setenv "M1" "TRUE")

(provide 'env-vars)
;;; env-vars.el ends here
