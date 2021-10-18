;;; package -- Summary:

;; any appearance related configurations or functions. "appearance" here means
;; fonts, colors, sizes, etc. Not things like enabling/disabling displays.

;;; Commentary:

;;; Code:

;;; Background Appearance

(use-package color-theme-sanityinc-tomorrow :ensure t)
(use-package color-theme-sanityinc-solarized :ensure t)

(defun dark-background ()
  "Change the appearance of Emacs to use a dark higher-contrast background."
  (interactive)
  (load-theme 'sanityinc-tomorrow-bright t)
  (auto-dim-other-buffers-mode t))

(defun light-background ()
  "Change the appearance of Emacs to use a light background."
  (interactive)
  (load-theme 'sanityinc-solarized-light t)
  (auto-dim-other-buffers-mode 0))

(require 'computers "~/.emacs.d/computers.el")
(defun font-height ()
  (cond
   ((convertible?) 130)
   ((vingtor?) 120)
   ((macbook?) 150)
   (t 135)))

(defun default-appearance ()
  "Change the appearance of Emacs to preferred default settings."
  (interactive)
  (dark-background)
  (set-face-attribute 'default nil
                      :height  (font-height)
                      :weight  'medium
                      :width   'ultra-expanded
                      :font    "JetBrains Mono"))

(defun set-font-multiplier (multiplier)
  (set-face-attribute 'default nil
                      :height (floor (* multiplier (font-height)))))

(defun antman-uses-emacs ()
  "Make font tiny."
  (interactive)
  (set-font-multiplier 0.66))

(defun smaller-font ()
  "Make font smaller."
  (interactive)
  (set-font-multiplier 0.8))

(defun slightly-smaller-font ()
  (interactive)
  (set-font-multiplier 0.9))

(defun default-font-size ()
  "Restore default font size."
  (interactive)
  (set-font-multiplier 1.0))

(defun bigger-font ()
  "Make font larger."
  (interactive)
  (set-font-multiplier 1.5))

(provide 'appearance)
;;; appearance.el ends here
