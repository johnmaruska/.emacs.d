;;; package -- Summary:

;; Settings and configuration which I consider 'global' meaning these settings
;; are either core Emacs functionality, or additional functionality which is
;; applicable or available in all use cases (e.g. whitespace, autocomplete).

;;; Code:

;; InteractivelyDoThings
(require 'ido)
(defun configure-ido ()
  "Configuration settings for `ido` package to interactively do things."
  (ido-mode t))

(require 'neotree)
(defun configure-neotree ()
  "Configuration settings for `neotree` package."
  (global-set-key [f8] 'neotree-toggle)
  (neotree-dir "~/dev/")
  (neotree-hide))

(require 'whitespace)
(defun configure-whitespace-mode ()
  "Configuration settings for global `whitespace-mode`."
  (setq whitespace-style '(face empty tabs trailing))
  (global-whitespace-mode 1))

(require 'auto-complete)
(defun configure-autocomplete ()
  "Configuration settings for `autocomplete` package."
  (define-key ac-completing-map [return] nil) ; no enter (1.)
  (define-key ac-completing-map "\r" nil) ; no enter (2.)
  (define-key ac-completing-map "\t" 'ac-complete) ; use tab to complete
  (put 'upcase-region 'disabled nil))

(defun allow-menu-key-on-mac ()
  "Allow use of the menu button on Mac for \\[execute-extended-command]."
  (when (eq system-type 'darwin)
    ;; MacBook Pro 15-inch 2019 on macOS Mojave did this to me
    (global-set-key (kbd "≈") 'execute-extended-command)
    (global-set-key (kbd "C-p") 'execute-extended-command)))

(defun default-displays ()
  "Configuration settings for what tools to display."
  (setq inhibit-startup-screen t)  ; Don't display welcome screen
  (tool-bar-mode -1)  ; hide tool bar
  (scroll-bar-mode -1)  ; hide scroll bar
  (menu-bar-mode 1)  ; show menu-bar. Doesn't affect Mac OS
  (display-time-mode 1)  ; show clock in status bar
  (column-number-mode 1)  ; show column numbers in addition to line numbers
  (blink-cursor-mode 1)  ; cursor should blink
  (show-paren-mode 1)  ; highlight matching paren
  (global-linum-mode 1))

(defun default-behaviors ()
  "Configuration settings for what behavior to assert."
  (setq-default indent-tabs-mode nil)  ; don't mix tabs and spaces
  (setq scroll-step 1)  ; keyboard scroll one line at a time
  (setq require-final-newline t) ; files must end with a newline
  (setq default-directory "~/")
  (allow-menu-key-on-mac)
  (configure-autocomplete)
  (configure-ido)
  (configure-whitespace-mode))

(defun default-ui-configuration ()
  "Set values for various Emacs UI configuration settings."
  (interactive)
  (default-displays)
  (default-behaviors)
  (configure-neotree)
  ;; EMBIGGEN
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))


(defun json-format ()
  "Format block to adhere to readable JSON format."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark)
                             (point)
                             "python -m json.tool"
                             (buffer-name)
                             t)))

;;; Commentary:

(provide 'global)
;;; global.el ends here
