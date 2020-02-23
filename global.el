;;; package -- Summary:

;; Settings and configuration which I consider 'global' meaning these settings
;; are either core Emacs functionality, or additional functionality which is
;; applicable or available in all use cases (e.g. whitespace, autocomplete).

;;; Code:

(require 'auto-complete)
(defun configure-autocomplete ()
  "Configuration settings for `autocomplete` package."
  (define-key ac-completing-map [return] nil) ; no enter (1.)
  (define-key ac-completing-map "\r" nil) ; no enter (2.)
  (define-key ac-completing-map "\t" 'ac-complete) ; use tab to complete
  (put 'upcase-region 'disabled nil))

;; InteractivelyDoThings
(require 'ido)
(defun configure-ido ()
  "Configuration settings for `ido` package to interactively do things."
  (ido-mode t))

(require 'neotree)
(require 'computers "~/.emacs.d/computers.el")
(defun configure-neotree ()
  "Configuration settings for `neotree` package."
  (global-set-key [f8] 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neotree-dir "~/dev/")
  (if (vingtor-windows?)
      (neotree-dir "~/Documents/dev/")
      (neotree-dir "~/dev/"))
  (neotree-hide))

(require 'which-key)
(defun configure-which-key ()
  "Configure which-key for help with available hotkey completion."
  (which-key-mode)
  (which-key-setup-side-window-bottom)
  (setq which-key-show-early-on-C-h t)  ; pop up early if help is called
  (setq which-key-popup-type 'side-window))

(require 'computers "~/.emacs.d/computers.el")
(defun allow-menu-key ()
  "Allow use of the menu button on Mac for \\[execute-extended-command]."
  (when (darwin?)
    (global-set-key (kbd "C-p") 'execute-extended-command))
  (when (gr-macbook?)
    (global-set-key (kbd "≈") 'execute-extended-command))
  (when (windows?)
    (global-set-key (kbd "<apps>") 'execute-extended-command)))

(require 'dashboard)
(defun configure-dashboard ()
  "Configure the start-up dashboard package."
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents . 30)
                          ;; more options for org-mode
                          (projects . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t))

(defun default-displays ()
  "Configuration settings for what tools to display."
  (setq inhibit-startup-screen t)  ; Don't display welcome screen
  (tool-bar-mode -1)      ; hide tool bar
  (scroll-bar-mode -1)    ; hide scroll bar
  (menu-bar-mode 1)       ; show menu-bar. Doesn't affect Mac OS
  (display-time-mode 1)   ; show clock in status bar
  (column-number-mode 1)  ; show column numbers in addition to line numbers
  (blink-cursor-mode 1)   ; cursor should blink
  (show-paren-mode 1)     ; highlight matching paren
  (global-linum-mode 1))

(defun default-behaviors ()
  "Configuration settings for what behavior to assert."
  (setq-default indent-tabs-mode nil)  ; don't mix tabs and spaces
  (setq scroll-step 1)                 ; keyboard scroll one line at a time
  (setq require-final-newline t)       ; files must end with a newline
  (setq default-directory "~/")
  (allow-menu-key)
  (configure-autocomplete)
  (configure-ido)
  (configure-which-key))

(defun default-ui-configuration ()
  "Set values for various Emacs UI configuration settings."
  (interactive)
  (default-displays)
  (default-behaviors)
  (configure-neotree)
  ;; EMBIGGEN
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;;; Commentary:

(provide 'global)
;;; global.el ends here
