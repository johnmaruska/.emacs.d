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
  (load-theme 'sanityinc-tomorrow-eighties t)
  ;; (reset-term-colors)
  )

(defun light-background ()
  "Change the appearance of Emacs to use a light background."
  (interactive)
  ;; Look up a theme with a more beige background
  (load-theme 'sanityinc-tomorrow-day t)
  ;; (reset-term-colors)
  )

(defun mac-default-font ()
  "Change the font of Emacs to the default for Mac."
  (set-face-attribute 'default
                      nil
                      :font "Fira Code"
                      :height 135))

(defun ubuntu-default-font ()
  "Change the appearance of Emacs to default for Ubuntu."
  (set-face-attribute 'default nil :font "Ubuntu Mono"))

(defun default-font ()
  "Change the font of Emacs to the default setting."
  (cond ((eq system-type 'gnu/linux) (ubuntu-default-font))
        ((eq system-type 'darwin) (mac-default-font))))

(defun default-appearance ()
  "Change the appearance of Emacs to preferred default settings."
  (interactive)
  (default-font)
  (dark-background))

(defun google-hangouts-sucks ()
  "Make screen viewable when presenting to a meeting.
Greatly increase the size of the font and change to a light scheme to present."
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil
                  :stipple nil
                  :inverse-video nil
                  :box nil
                  :strike-through nil
                  :overline nil
                  :underline nil
                  :slant normal
                  :weight normal
                  :height 220
                  :width normal
                  :family "Fira Code")))))
  (light-background))

(provide 'appearance)
;;; appearance.el ends here
