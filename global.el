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
  :bind ([f8] . neotree-toggle)
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

(defconst ON   1)  ; Can I make this syntax-highlight green? That'd be neat
(defconst OFF -1)  ; same Q but red

(defun default-ui-configuration ()
  "Set values for various Emacs UI configuration settings."
  (toggle-frame-maximized)     ; full-screen mode toggle. TODO: find enable not toggle
  (setq
   default-directory         "~/"
   history-delete-duplicates t  ; minibuffer history keeps only one of each unique entry
   inhibit-startup-screen    t  ; Don't display welcome screen
   require-final-newline     t
   scroll-step               1  ; keyboard scroll one line at a time
   suggest-key-bindings      5  ; show key-binding suggestions for 5 seconds
   )
  (setq-default
   indent-tabs-mode nil  ; don't mix tabs and spaces
   tab-width        4
   )

  (allow-menu-key)
  (beacon-mode            ON)  ; cursor lights beacon on windows change/scroll
  (blink-cursor-mode      ON)  ; cursor should blink
  (column-number-mode     ON)  ; show column numbers
  (display-time-mode      ON)  ; show clock in status bar
  (global-hl-line-mode    ON)  ; highlight line with cursor
  (global-linum-mode      ON)
  (line-number-mode       ON)  ; show line numbers
  (menu-bar-mode          OFF) ; show menu-bar. Doesn't affect Mac OS
  (scroll-bar-mode        OFF) ; hide scroll bar
  (set-fill-column        80)
  (show-paren-mode        ON)  ; highlight matching paren
  (tool-bar-mode          OFF) ; hide tool bar
  (minibuffer-electric-default-mode ON)  ; [DEFAULT-ARG] instead of (default DEFAULT-ARG)
  ;; TODO: resize-mini-windows
  )

;;; Commentary:

(provide 'global)
;;; global.el ends here
