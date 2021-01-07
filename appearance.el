;;; package -- Summary:

;; any appearance related configurations or functions. "appearance" here means
;; fonts, colors, sizes, etc. Not things like enabling/disabling displays.

;;; Commentary:

;;; Code:

;;; Background Appearance

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(defun dark-background ()
  "Change the appearance of Emacs to use a dark higher-contrast background."
  (interactive)
  (load-theme 'sanityinc-tomorrow-bright t))

(defun light-background ()
  "Change the appearance of Emacs to use a light background."
  (interactive)
  ;; TODO: Look up a theme with a more beige background
  (load-theme 'sanityinc-tomorrow-day t))

(defun default-appearance ()
  "Change the appearance of Emacs to preferred default settings."
  (interactive)
  (dark-background)
  (set-face-attribute 'default nil
                      :height  135
                      :weight  'medium
                      :width   'ultra-expanded
                      :font    "JetBrains Mono"))

(defun antman-uses-emacs ()
  "Make font tiny."
  (interactive)
  (set-face-attribute 'default nil :height 90))

(defun bigger-font ()
  "Make font larger."
  (interactive)
  (set-face-attribute 'default nil :height 160))

(provide 'appearance)
;;; appearance.el ends here
