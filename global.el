;;; package -- Summary:

;; Settings and configuration which I consider 'global' meaning these settings
;; are either core Emacs functionality, or additional functionality which is
;; applicable or available in all use cases (e.g. whitespace, autocomplete).

;;; Code:

(use-package auto-complete
  :bind (:map ac-completing-map
              ("RET" . nil)
              ("\r" . nil)
              ("\t" . ac-complete))
  :config (put 'upcase-region 'disabled nil))

(use-package auto-dim-other-buffers
  :hook (after-init . (lambda ()
                        (when (fboundp 'auto-dim-other-buffers-mode)
                          (auto-dim-other-buffers-mode t)))))

(use-package dashboard
  :init (setq dashboard-startup-banner 'logo
              dashboard-items '((recents . 20)
                                ;; more options for org-mode
                                (projects . 10))
              dashboard-banner-logo-title "Welcome! Everything is fine."
              dashboard-footer-messages '("Don't check the internet. Just start working."
                                          "Thank you for keeping trying."
                                          "Slow progress is still progress.")
              dashboard-set-heading-icons t
              dashboard-set-file-icons t
              dashboard-set-navigator t)
  :config (dashboard-setup-startup-hook))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package ido
  :config (ido-mode t))

(use-package neotree
  :bind ("f8" . neotree-toggle)
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
              neo-window-position 'left)
  :config
  (neotree-dir (expand-file-name "~/dev/"))
  (neotree-hide))

(use-package projectile
  :after  (neotree)
  :bind   (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :init   (setq projectile-enable-caching t
                projectile-switch-project-action 'neotree-projectile-action
                projectile-use-git-grep t
                projectile-project-root-files '(".git")
                projectile-project-root-files-bottom-up '(".projectile")
                projectile-file-exists-remote-cache-expire (* 10 60))
  :config (projectile-global-mode))

(use-package which-key
  :init (setq which-key-show-early-on-C-h t
              which-key-popup-type 'side-window)
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

(require 'computers "~/.emacs.d/computers.el")
(defun allow-menu-key ()
  "Allow use of the menu button for \\[execute-extended-command]."
  (when (windows?)
    (global-set-key (kbd "<apps>") 'execute-extended-command)))

(defun default-displays ()
  "Configuration settings for what tools to display."
  (setq inhibit-startup-screen t)  ; Don't display welcome screen
  (tool-bar-mode -1)      ; hide tool bar
  (scroll-bar-mode -1)    ; hide scroll bar
  (menu-bar-mode 1)       ; show menu-bar. Doesn't affect Mac OS
  (display-time-mode 1)   ; show clock in status bar
  (line-number-mode 1)    ; show line numbers
  (column-number-mode 1)  ; show column numbers
  (blink-cursor-mode 1)   ; cursor should blink
  (beacon-mode 1)         ; cursor lights beacon on windows change/scroll
  (show-paren-mode 1)     ; highlight matching paren
  (global-linum-mode 1)
  (global-hl-line-mode 1)  ; highlight line with cursor
  (minibuffer-electric-default-mode 1)  ; [DEFAULT-ARG] instead of (default DEFAULT-ARG)
  ;; TODO: resize-mini-windows
  )

(defun default-behaviors ()
  "Configuration settings for what behavior to assert."
  (setq default-directory "~/")
  (setq-default tab-width 4)
  ;; don't mix tabs and spaces
  (setq-default indent-tabs-mode nil)
  ;; keyboard scroll one line at a time
  (setq scroll-step 1)
  ;; files must end with a newline
  (setq require-final-newline t)
  ;; minibuffer history keeps only one of each unique entry
  (setq history-delete-duplicates t)
  ;; show key-binding suggestions for 5 seconds
  (setq suggest-key-bindings 5)
  (set-fill-column 80)
  (allow-menu-key))

(defun default-ui-configuration ()
  "Set values for various Emacs UI configuration settings."
  (interactive)
  (default-displays)
  (default-behaviors)
  (toggle-frame-maximized))

;;; Commentary:

(provide 'global)
;;; global.el ends here
