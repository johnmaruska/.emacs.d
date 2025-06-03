;; package -- Summary:

;; Settings and configuration which I consider 'global' meaning these settings
;; are either core Emacs functionality, or additional functionality which is
;; applicable or available in all use cases (e.g. whitespace, autocomplete).

;;; Code:

(use-package ag
  :ensure t
  :custom
  (ag-reuse-window 't)
  (ag-reuse-buffers 't))

(use-package all-the-icons
  :ensure t)

(use-package auto-dim-other-buffers
  :ensure t
  :hook (after-init . (lambda ()
                        (when (fboundp 'auto-dim-other-buffers-mode)
                          (auto-dim-other-buffers-mode t)))))

;; cursor lights beacon on windows change/scroll)
(use-package beacon
  :ensure t :delight
  :config  (beacon-mode 1))

(use-package company
  :ensure t :delight
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 5
        company-tooltip-flip-when-above t))

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

(use-package delight
  :ensure t)

(use-package direnv
  :ensure t
  :config (direnv-mode))

(use-package emacs
  :delight
  (auto-fill-function)
  (eldoc-mode)
  (eshell-mode)
  (lisp-interaction-mode)
  (page-break-lines-mode)
  :config
  (global-eldoc-mode))

(use-package files
  :init
  (setq
   auto-save-default nil
   backup-inhibited  t
   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   delete-old-versions t))

(use-package flycheck
  :ensure t :delight
  :custom
  (global-flycheck-mode +1))

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

(use-package hl-todo
  :ensure t :delight
  :custom
  (global-hl-todo-mode +1)
  :config
  (setq hl-todo-keyword-faces
        '(
          ("NOTE"   . "#FFFFFF")
          ;; yellow
          ("TODO"   . "#EED202")
          ;; orange
          ("WARN"   .  "#FF7900")
          ;; red
          ("ERROR"  . "#DD1D20"))))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

(use-package ido
  :ensure t
  :config (ido-mode t))

(use-package major-mode-icons
  :ensure t
  :after  (all-the-icons)
  :delight major-mode-icons-mode
  :custom
  (major-mode-icons-mode +1))

(use-package map :ensure t)

(use-package multiple-cursors :ensure t)

(use-package tree-sitter
  :ensure t
  :config (global-tree-sitter-mode)
  :hook (tree-sitter-mode . tree-sitter-hl-mode))


(use-package tree-sitter-langs
  :ensure t
  :after (tree-sitter)
  :config
  ;;;; Move grammars to directory that tree-sitter can see them.
  (let* ((files (directory-files (tree-sitter-langs--bin-dir)
                                 nil "\\.dylib$")))
    (dolist (grammar-file files)
      (copy-file (concat (tree-sitter-langs--bin-dir) grammar-file) (concat (expand-file-name user-emacs-directory) "tree-sitter/" "libtree-sitter-" grammar-file) t)
      (message "%s grammar files copied" (length files)))))


;;; Try out treemacs to replace neotree
(use-package treemacs
  :ensure t
  :bind ([f8] . treemacs)
  :custom (treemacs-position 'right))
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)
(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :ensure t)
(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once))
(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; display ^L linefeed character as a horizontal line
(use-package page-break-lines
  :ensure t :delight
  :config (global-page-break-lines-mode))

(use-package projectile
  :ensure t
  :bind   (:map projectile-mode-map ("C-c C-p" . projectile-command-map))
  :init   (setq projectile-enable-caching t
                projectile-use-git-grep t
                projectile-project-root-files '(".git")
                projectile-project-root-files-bottom-up '(".projectile")
                projectile-file-exists-remote-cache-expire (* 10 60)
                projectile-create-missing-test-files t)
  :config (projectile-mode))

(use-package restclient
  :ensure t)

(use-package uuidgen
  :ensure t)

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

(use-package yasnippet
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"  ;; Personal snippets
                                 "~/.emacs.d/elpa/yasnippet-snippets-1.0/snippets/"  ;; Official snippets
                                 ))
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

(require 'computers "~/.emacs.d/computers.el")
(defun allow-menu-key ()
  "Allow use of the menu button for \\[execute-extended-command]."
  (when (windows?)
    (global-set-key (kbd "<apps>") 'execute-extended-command)))

(defconst ON   1)  ; Can I make this syntax-highlight green? That'd be neat
(defconst OFF -1)  ; same Q but red

(defun default-ui-configuration ()
  "Set values for various Emacs UI configuration settings."
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
  (global-display-line-numbers-mode ON)
  (menu-bar-mode          OFF) ; show menu-bar. Doesn't affect Mac OS
  (scroll-bar-mode        OFF) ; hide scroll bar
  (set-fill-column        80)
  (show-paren-mode        ON)  ; highlight matching paren
  (tool-bar-mode          OFF) ; hide tool bar
  (minibuffer-electric-default-mode ON)  ; [DEFAULT-ARG] instead of (default DEFAULT-ARG)
  ;; TODO: resize-mini-windows

  ;; switch window reverse order
  (global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
  ;; Allow MacOS to use Cmd-Q
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

  )

;;; Commentary:

(provide 'global)
;;; global.el ends here
