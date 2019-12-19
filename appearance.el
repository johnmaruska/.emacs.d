;;; package -- Summary:

;; any appearance related configurations or functions. "appearance" here means
;; fonts, colors, sizes, etc. Not things like enabling/disabling displays.

;;; Commentary:

;; Probably I'll have to duplicate a lot of this stuff across different
;; platforms to use the same "feel" configuration everywhere...

;;; Code:

;;; Background Appearance
(defun dark-background ()
  "Change the appearance of Emacs to use a dark background."
  (interactive)
  (load-theme 'sanityinc-tomorrow-eighties t))

(defun dark-background-bright ()
  "Change the appearance of Emacs to use a dark higher-contrast background."
  (interactive)
  (load-theme 'sanityinc-tomorrow-bright t))

(defun light-background ()
  "Change the appearance of Emacs to use a light background."
  (interactive)
  ;; TODO: Look up a theme with a more beige background
  (load-theme 'sanityinc-tomorrow-day t))

(defun vingtor-ubuntu-appearance ()
  "Change the appearance of Emacs to preferred settings for docked home office."
  (interactive)
  (set-face-attribute 'default nil :height 110 :font "Ubuntu Mono")
  (dark-background))

(defun vingtor-windows-appearance ()
  "Change the appearance of Emacs to preferred settings for Desktop Windows."
  (interactive)
  (set-face-attribute 'default nil
                      :height 131
                      :font "Consolas")
  (dark-background))

(defun convertible-windows-appearance ()
  "Change the appearance of Emacs to preferred settings for convertible Windows."
  (interactive)
  (set-face-attribute 'default nil
                      :height 110
                      :font "Consolas")
  (dark-background))

(defun macbook-appearance ()
  "Change the appearance of Emacs to preferred settings for floating Macbook."
  (interactive)
  (set-face-attribute 'default nil
                      :height 135
                      :weight 'medium
                      :font "Andale Mono")
  (dark-background))

(require 'computers "~/.emacs.d/computers.el")
(defun default-appearance ()
  "Change the appearance of Emacs to preferred default settings."
  (interactive)
  (cond
   ((gr-macbook?)          (macbook-appearance))
   ((vingtor-ubuntu?)      (vingtor-ubuntu-appearance))
   ((vingtor-windows?)     (vingtor-windows-appearance))
   ((convertible-windows?) (convertible-windows-appearance))
   (t                      (dark-background))))

(provide 'appearance)
;;; appearance.el ends here
