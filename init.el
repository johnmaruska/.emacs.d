;;; package -- Summary:

;; Custom Emacs environment for John Maruska. Based initially off of Chris Gore's
;; Emacs configuration. Stripped out minimal useful configuration and built own
;; on top of this.

;;; Code:

(require 'packages "~/.emacs.d/packages.el")
(install-all-packages)

(require 'neotree) ; hides a warning
;; the following functions might not be defined at runtime:
;;   neo-buffer--unlock-width, neo-buffer--lock-width
(require 'global "~/.emacs.d/global.el")
(default-ui-configuration)

(require 'appearance "~/.emacs.d/appearance.el")
(default-appearance)

(require 'env-vars "~/.emacs.d/env-vars.el")
(set-default-envvars)

(require 'mode-hooks "~/.emacs.d/mode-hooks.el")
(configure-all-modes)

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
(require 'utils "~/.emacs.d/utils.el")

;; TODO: find better spot
(defun configure-sbcl ()
  ;; look into installing slime/quicklisp-slime-helper
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl"))

(require 'computers "~/.emacs.d/computers.el")
(when (convertible?)
  (configure-sbcl))

(require 'computers "~/.emacs.d/computers.el")
(when (gr-macbook?)
  (load "~/.emacs.d/secrets/gr.el")
  (setup-mac-displays))

(defun chris-zoom ()
  (interactive)
  (browse-url "https://us02web.zoom.us/j/5734523216?pwd=Y2JjN29pYkFwcVZKZUx4RnN6N0VmZz09"))

;;; Commentary:

(provide 'init)
;;; init.el ends here
