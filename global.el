;;; package -- Summary:

;; Settings and configuration which I consider 'global' meaning these settings
;; are either core Emacs functionality, or additional functionality which is
;; applicable or available in all use cases (e.g. whitespace, autocomplete).

;;; Code:

(use-package ag :ensure t)

(use-package all-the-icons :ensure t)

(use-package auto-complete
  :ensure t
  :bind   (:map ac-completing-map
                ("RET" . nil)
                ("\r" . nil)
                ("\t" . ac-complete))
  :config (put 'upcase-region 'disabled nil))

(use-package auto-dim-other-buffers
  :ensure t
  :hook (after-init . (lambda ()
                        (when (fboundp 'auto-dim-other-buffers-mode)
                          (auto-dim-other-buffers-mode t)))))

;; cursor lights beacon on windows change/scroll)
(use-package beacon
  :ensure t
  :delight beacon-mode
  :config  (beacon-mode 1))

(require 'computers "~/.emacs.d/computers.el")
(use-package dashboard
  :ensure t
  :init (setq dashboard-startup-banner 'logo
              dashboard-items (cond
                               ((convertible?)
                                '((recents . 10) (projects . 3)))
                               (t
                                '((recents . 20) (projects . 10))))
              dashboard-banner-logo-title "Welcome! Everything is fine."
              dashboard-footer-messages '("Don't check the internet. Just start working."
                                          "Thank you for keeping trying."
                                          "Slow progress is still progress.")
              dashboard-set-heading-icons t
              dashboard-set-file-icons    t
              dashboard-set-navigator     t)
  :config (dashboard-setup-startup-hook))

(use-package delight :ensure t)

(use-package emacs
  :delight
  (auto-fill-function)
  (eldoc-mode)
  (eshell-mode)
  (lisp-interaction-mode)
  (page-break-lines-mode))

(use-package flymd
  :ensure t
  ;; use Firefox, not Chrome, for browser-open-function
  ;; <https://github.com/mola-T/flymd/blob/master/browser.md#user-content-chrome-macos>
  :init (setq flymd-browser-open-function
              (lambda (url)
                (let ((process-environment (browse-url-process-environment)))
                  (apply 'start-process
                         (concat "firefox " url)
                         nil
                         "/usr/bin/open"
                         (list "-a" "firefox" url))))))

(use-package ido
  :ensure t
  :config (ido-mode t))

(use-package magit-gitflow
  :ensure t
  :delight magit-mode
  :delight magit-status-mode
  :bind ("C-x g" . magit-status)
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package major-mode-icons
  :ensure t
  :after  (all-the-icons)
  :delight major-mode-icons-mode)

(use-package multi-term
  :ensure t
  :init
  (setq multi-term-program "bash")
  (when window-system
    (custom-set-variables
     '(term-bind-key-alist '(("C-c C-e" . term-send-escape)))))
  :config
  (defun bash ()
    (interactive)
    (let ((multi-term-program "bash")
          (multi-term-buffer-name "bash"))
      (multi-term)))

  (defun python ()
    (interactive)
    (let ((multi-term-program "python")
          (multi-term-buffer-name "python"))
      (multi-term)))

  (defun ssh (ssh-to)
    (interactive "sSSH to: ")
    (let ((multi-term-program "ssh")
          (multi-term-buffer-name ssh-to)
          (multi-term-program-switches ssh-to))
      (multi-term)))

  (defun su ()
    (interactive)
    (let ((multi-term-program "su")
          (multi-term-buffer-name "su")
          (multi-term-program-switches "--login"))
      (multi-term))))

(use-package multiple-cursors :ensure t)

(use-package neotree
  :ensure t
  :bind ([f8] . neotree-toggle)
  :init (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
              neo-window-position 'left)
  :config
  (neotree-dir (expand-file-name "~/"))
  (neotree-hide))

;; display ^L linefeed character as a horizontal line
(use-package page-break-lines
  :ensure t
  :config (global-page-break-lines-mode))

(use-package projectile
  :ensure t
  :after  (neotree)
  :delight '(:eval (concat " " (projectile-project-name)))
  :bind   (:map projectile-mode-map ("C-c p" . projectile-command-map))
  :init   (setq projectile-enable-caching t
                projectile-switch-project-action 'neotree-projectile-action
                projectile-use-git-grep t
                projectile-project-root-files '(".git")
                projectile-project-root-files-bottom-up '(".projectile")
                projectile-file-exists-remote-cache-expire (* 10 60))
  :config (projectile-global-mode))

(use-package restclient :ensure t)

(use-package uuidgen :ensure t)

(when (windows?)
  (use-package vterm :ensure t))

(use-package which-key
  :ensure t
  :delight which-key-mode
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
   display-time-default-load-average nil
   history-delete-duplicates t  ; minibuffer history keeps only one of each unique entry
   inhibit-startup-screen    t  ; Don't display welcome screen
   require-final-newline     t
   ring-bell-function        'ignore  ; mute the warning system sound
   scroll-step               1  ; keyboard scroll one line at a time
   suggest-key-bindings      5  ; show key-binding suggestions for 5 seconds
   )
  (setq-default
   indent-tabs-mode nil  ; don't mix tabs and spaces
   tab-width        4)

  (allow-menu-key)
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
