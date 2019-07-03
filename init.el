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

(defun json-format ()
  "Format block to adhere to readable JSON format."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))


;; EMBIGGEN
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; InteractivelyDoThings
(require 'ido)
(ido-mode t)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(neotree-dir "~/dev/")
(neotree-hide)

;; Autocomplete config
(require 'auto-complete)
(define-key ac-completing-map [return] nil) ; no enter (1.)
(define-key ac-completing-map "\r" nil) ; no enter (2.)
(define-key ac-completing-map "\t" 'ac-complete) ; use tab to complete
(put 'upcase-region 'disabled nil)

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face empty tabs trailing))
(global-whitespace-mode 1)

;; yafolding
(require 'yafolding)
(defvar yafolding-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
    (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
    (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
    map))

;; Miscellaneous settings
(setq-default indent-tabs-mode nil)  ; don't mix tabs and spaces
(tool-bar-mode -1)  ; hide tool bar
(scroll-bar-mode -1)  ; hide scroll bar
(menu-bar-mode 1)  ; show menu-bar. Doesn't affect Mac OS
(display-time-mode 1)  ; show clock in status bar
(column-number-mode 1)  ; show column numbers in addition to line numbers
(blink-cursor-mode 1)  ; cursor should blink
(global-linum-mode 1)
(show-paren-mode 1)  ; highlight matching paren
(setq inhibit-startup-screen t)  ; Don't display welcome screen
(setq default-directory "~/")
(setq scroll-step 1)  ; keyboard scroll one line at a time
(setq require-final-newline t)  ; files must end with a newline

;; work Mac + windows keyboard to use the menu->M-x shortcut
(when (eq system-type 'darwin)
  (define-key key-translation-map (kbd "C-p") (kbd "M-x")))

;;; Commentary:

(provide 'init)
;;; init.el ends here
