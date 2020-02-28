;;; package -- Summary:

;; Custom Emacs environment for John Maruska. Based initially off of Chris Gore's
;; Emacs configuration. Stripped out minimal useful configuration and built own
;; on top of this.

;;; Code:

(require 'packages "~/.emacs.d/packages.el")
(install-all-packages)

(require 'env-vars "~/.emacs.d/env-vars.el")
(set-default-envvars)

(require 'appearance "~/.emacs.d/appearance.el")
(default-appearance)

(require 'mode-hooks "~/.emacs.d/mode-hooks.el")
(configure-all-modes)

(require 'neotree) ; hides a warning
;; the following functions might not be defined at runtime:
;;   neo-buffer--unlock-width, neo-buffer--lock-width
(require 'global "~/.emacs.d/global.el")
(default-ui-configuration)

(require 'computers "~/.emacs.d/computers.el")
(when (gr-macbook?)
  (load "~/.emacs.d/secrets/gr.el"))

(require 'eshell)
(declare-function eshell/pwd "ext:eshell/pwd")
(defvar eshell-prompt-function
  (lambda ()
    (concat
     (propertize (format-time-string "%-m/%d/%Y %a %-I:%M:%S %p " (current-time))
                 'face `(:foreground "#aaaaff"))
     (propertize (abbreviate-file-name (eshell/pwd))
                 'face `(:foreground "#aaaa44"))
     (if (= (user-uid) 0) " # " " $ "))))

(require 'json)

;; TODO: find better spot
(defun configure-sbcl ()
  ;; look into installing slime/quicklisp-slime-helper
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl"))

(configure-sbcl)

;;; Commentary:

(provide 'init)
;;; init.el ends here
