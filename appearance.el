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

(require 'computers "~/.emacs.d/computers.el")
(defun font-height ()
  (cond
   ((convertible?) 130)
   ((vingtor?) 120)
   ((macbook?) 115)
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

(defun antman-uses-emacs ()
  "Make font tiny."
  (interactive)
  (set-face-attribute 'default nil :height (floor (* 0.66 (font-height)))))

(defun bigger-font ()
  "Make font larger."
  (interactive)
  (set-face-attribute 'default nil :height (floor (* 1.2 (font-height)))))

(provide 'appearance)
;;; appearance.el ends here
