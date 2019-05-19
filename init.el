;;; package -- Summary:

;; Custom Emacs environment for John Maruska. Based initially off of Chris Gore's
;; Emacs configuration. Stripped out minimal useful configuration and built own
;; on top of this.

;;; Code:

(load "~/.emacs.d/env-vars.el")
(load "~/.emacs.d/packages.el")

;; EMBIGGEN
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; InteractivelyDoThings
(require 'ido)
(ido-mode t)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(neotree-toggle)
(neotree-dir "~/dev/")

;; Autocomplete config
(require 'auto-complete)
(define-key ac-completing-map [return] nil) ; no enter (1.)
(define-key ac-completing-map "\r" nil) ; no enter (2.)
(define-key ac-completing-map "\t" 'ac-complete) ; use tab to complete
(put 'upcase-region 'disabled nil)

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
;; TODO: did i need this for mac?
;; (define-key key-translation-map (kbd "C-p") (kbd "M-x"))

(load "~/.emacs.d/appearance.el")
(load "~/.emacs.d/mode-hooks.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(package-selected-packages
   (quote
    (discover yafolding bicycle fold-this multiple-cursors-mode w3m minesweeper magit-gitflow magit-flow multiple-cursors magit racket-mode geiser rjsx-mode ensime scala-mode command-log-mode neotree clojure-mode-extra-font-locking yaml-mode whitespace-cleanup-mode rust-mode rainbow-delimiters paredit markdown-mode flycheck-rust flycheck-pos-tip flycheck-clojure color-theme-sanityinc-tomorrow auto-complete ack-and-a-half))))

;;; Commentary:

(provide 'init)
;;; init.el ends here
