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

;; TODO: why do I have this?
(load "~/.emacs.d/multi-term.el")

;;; Commentary:

(provide 'init)
;;; init.el ends here
