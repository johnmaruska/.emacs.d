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

(defun mac-default-font-and-theme ()
  "Change the appearance and font of Emacs to the default setting for Mac."
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil
                  :stipple nil
                  :inverse-video nil
                  :box nil
                  :strike-through nil
                  :over-line nil
                  :underline nil
                  :slant normal
                  :weight normal
                  :height 135
                  :width normal
                  :family "Fira Code")))))
  (dark-background))

(defun ubuntu-default-font-and-theme ()
  "Change the appearance and font of Emacs to use the default setting for Ubuntu."
  (interactive)
  (set-face-attribute 'default nil :font "Ubuntu Mono")
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

(cond ((eq system-type 'gnu/linux)
       (ubuntu-default-font-and-theme))
      ((eq system-type 'darwin)
       (mac-default-font-and-theme)))


(provide 'appearace)
;;; appearance.el ends here
