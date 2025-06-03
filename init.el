;;; package -- Summary:

;; Custom Emacs environment for John Maruska. Based initially off of Chris Gore's
;; Emacs configuration. Stripped out minimal useful configuration and built own
;; on top of this.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;                    Per-Machine Variable Setup
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;;
;;    Variables
;;
;;;;;;;;;;;;;;;;

(defvar MY--operating-system
  (cond ((eq system-type 'darwin)
         "MacOS")
        ((string= (system-name) "pop-os")
         "Ubuntu")
        ((eq system-type 'windows-nt)
         "Windows")))

(defvar MY--current-machine
  (cond ((eq system-type 'darwin)
         "macbook")
        ((string= (system-name) "pop-os")
         "convertible")
        ((string= (system-name) "VINGTOR")
         "vingtor")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;    Environment Variables
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PAGER" "cat")
(when (string= "MacOS" MY--operating-system)
  (setenv "M1" "TRUE"))

;;;;;;;;;;;;;;;;
;;
;;    PATH variable
;;
;;;;;;;;;;;;;;;;

(defvar default-path (getenv "PATH"))

(defvar macbook-path-list
  (list
   "/nix/var/nix/profiles/default/bin/"  ; nix-blank commands
   "/Users/johnmaruska/.nix-profile/bin/"  ; installed by nix-env
   "/Users/johnmaruska/.sdkman/candidates/java/current/bin"
   "/opt/homebrew/bin"
   "/usr/local/bin"
   "~/.serverless/bin"
   "/opt/homebrew/Cellar/nvm/0.39.5"
   "~/Library/Python/3.9/bin"
   default-path))

(defvar vingtor-path-list
  (list "~/bin"
        "C:\\Users\\jackm"
        default-path))

(defvar path-list
  (cond ((string= "macbook" MY--current-machine) macbook-path-list)
        ((string= "vingtor" MY--current-machine) vingtor-path-list)
        (t                                   (list default-path))))
(defvar MY--ORIGINAL_EXEC_PATH exec-path)

(when window-system
  ;; this matters for sub-shell process (e.g. launch bash)
  (setenv "PATH"
          (mapconcat 'identity
                     path-list
                     (if (string= "vingtor" MY--current-machine) ";" ":"))))
;; this matters for eshell
(setq exec-path (append path-list MY--ORIGINAL_EXEC_PATH))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;                    Initialize Packages Installation
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("ELPA" . "http://tromey.com/elpa/")))

(setq create-lockfiles nil)
(setq package-check-signature nil)

(package-initialize)
;; (setq url-http-attempt-keepalives nil)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;                    Emacs UI/UX
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 default-directory         "~/"
 display-time-default-load-average nil
 ;; minibuffer history keeps only one of each unique entry
 history-delete-duplicates t
 ;; Don't display welcome screen
 inhibit-startup-screen    t
 require-final-newline     t
 ;; mute the warning system sound
 ring-bell-function        'ignore
 ;; keyboard scroll one line at a time
 scroll-step               1
 ;; show key-binding suggestions for 5 seconds
 suggest-key-bindings      5
 )
(setq-default
 indent-tabs-mode nil  ; don't mix tabs and spaces
 tab-width        4)

;; cursor should blink
(blink-cursor-mode      t)
;; show column numbers
(column-number-mode     t)
;; show clock in status bar
(display-time-mode      t)
;; highlight line with cursor
(global-hl-line-mode    t)
(global-display-line-numbers-mode t)
;; show menu-bar. Doesn't affect Mac OS
(menu-bar-mode          (if (string= "Windows" MY--operating-system)
                            -1 +1))
;; hide scroll bar
(scroll-bar-mode        -1)
(set-fill-column        80)
;; highlight matching paren
(show-paren-mode        t)
;; hide tool bar
(tool-bar-mode          -1)
;; [DEFAULT-ARG] instead of (default DEFAULT-ARG)
(minibuffer-electric-default-mode t)


(use-package auto-dim-other-buffers
  :ensure t
  :hook (after-init . (lambda ()
                        (when (fboundp 'auto-dim-other-buffers-mode)
                          (auto-dim-other-buffers-mode t)))))

;; cursor lights beacon on windows change/scroll)
(use-package beacon
  :ensure t :delight
  :config  (beacon-mode 1))

;;;;;;;;
;;
;; Text completion backend
;;
;;;;;;;;
(use-package company
  :ensure t :delight
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 5
        company-tooltip-flip-when-above t))

;; Hide mode labels where configured
(use-package delight :ensure t)

(use-package emacs
  :delight
  (auto-fill-function)
  (eldoc-mode)
  (eshell-mode)
  (lisp-interaction-mode)
  (page-break-lines-mode))

(use-package files
  :config
  (setq
   auto-save-default nil
   backup-inhibited  t
   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   delete-old-versions t))

(use-package ibuffer  ;; Interactive buffer management pane
  :bind ("C-x C-b" . ibuffer))

(use-package ido  ;; interactive buffer management actions
  :ensure t
  :config (ido-mode t))

(use-package multiple-cursors :ensure t)

;; display ^L linefeed character as a horizontal line
(use-package page-break-lines
  :ensure t :delight
  :config (global-page-break-lines-mode))

;;;;;;;;;;;;;;;;
;;
;;    Icons
;;
;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :ensure t)

(use-package major-mode-icons
  :ensure t
  :after  (all-the-icons)
  :delight major-mode-icons-mode
  :custom
  (major-mode-icons-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;    Color Themes
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (defun dark-background ()
    "Change the appearance of Emacs to use a dark higher-contrast background."
    (interactive)
    (load-theme 'sanityinc-tomorrow-bright t)
    (auto-dim-other-buffers-mode t)
;;; These values are _basically_sanityinc-tomorrow-bright, but made
;;; brighter for higher contrast
    (set-face-foreground 'font-lock-comment-face "#c090d0")
    (set-face-foreground 'font-lock-doc-face "#e0b0f0")
    (set-face-foreground 'font-lock-function-name-face "#f5a060")
    ;; macros
    (set-face-foreground 'font-lock-keyword-face "#d8e858")
    ;; includes clojure keywords
    (set-face-foreground 'font-lock-constant-face "#a0d0f0")
    ;; namespaces, incl namespaced keywords
    (set-face-foreground 'font-lock-type-face "#a0d0f0")
    (set-face-foreground 'font-lock-string-face "#80d0c0")))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :init
  (defun light-background ()
    "Change the appearance of Emacs to use a light background."
    (interactive)
    (load-theme 'sanityinc-solarized-light t)
    (set-face-foreground 'font-lock-comment-face "black")
    (auto-dim-other-buffers-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;    Font Customization
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun font-height ()
  "Value to use for font-height, varies with machine."
  (cond
   ((string= "convertible" MY--current-machine) 130)
   ((string= "vingtor" MY--current-machine) 105)
   (t 135)))

(defun default-font ()
  "Set font attributes to preferred defaults."
  (interactive)
  (set-face-attribute 'default nil
                      :height  (font-height)
                      :weight  'medium
                      :width   'ultra-expanded
                      :font    "JetBrains Mono"))

(defun set-font-multiplier (multiplier)
  "Adjust font height from current setting by MULTIPLIER."
  (set-face-attribute 'default nil
                      :height (floor (* multiplier (font-height)))))

(defun antman-uses-emacs ()
  "Make font tiny."
  (interactive)
  (set-font-multiplier 0.66))

(defun smaller-font ()
  "Make font smaller."
  (interactive)
  (set-font-multiplier 0.8))

(defun slightly-smaller-font ()
  "Set font size to a slight-smaller size.
Intended for changing monitors with competing resolutions."
  (interactive)
  (set-font-multiplier 0.9))

(defun default-font-size ()
  "Restore default font size."
  (interactive)
  (set-font-multiplier 1.0))

(defun bigger-font ()
  "Make font larger.
Intended for screen-sharing and pair programming."
  (interactive)
  (set-font-multiplier 1.5))


(defun default-appearance ()
  "Change the appearance of Emacs to preferred default settings."
  (interactive)
  (default-font)
  (dark-background))

(default-appearance)
(dark-background)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;                    Capabilities
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package restclient :ensure t)
(use-package uuidgen :ensure t)
(when (string= "Windows" MY--operating-system)
  (use-package vterm :ensure t))

;;;;;;;;
;;
;;   Text Search
;;
;;;;;;;;
(use-package ag
  :ensure t
  :custom
  (ag-reuse-window 't)
  (ag-reuse-buffers 't))

;;;;;;;;
;;
;;  Display a welcome dashboard instead of the scratch buffer
;;
;;;;;;;;
(use-package dashboard
  :ensure t
  :init (setq  dashboard-startup-banner 'logo
               dashboard-items
               (cond
                ((string= "convertible" MY--current-machine)
                 '((recents . 10) (projects . 3)))
                (t
                 '((recents . 20) (projects . 10))))
              dashboard-banner-logo-title "Welcome! Everything is fine."
              dashboard-footer-messages
              '("Don't check the internet. Just start working."
                "Thank you for keeping trying."
                "Slow progress is still progress.")
              dashboard-set-heading-icons t
              dashboard-set-file-icons    t
              ;; dashboard-startupify-list   t
              )
  :config (dashboard-setup-startup-hook))

;;;;;;;;
;;
;;    Directory-based environment settings
;;
;;;;;;;;
(use-package direnv
  :ensure t
  :config (direnv-mode))

(use-package eldoc
  :config
  (global-eldoc-mode))

;;;;;;;;
;;
;;  Popup warnings
;;
;;;;;;;;
(use-package flycheck
  :ensure t :delight
  :custom
  (global-flycheck-mode +1))

;;;;;;;;
;;
;;  Warn against using non-Emacsy keybindings
;;
;;;;;;;;
(use-package guru-mode
  :ensure t :delight
  :custom (guru-warn-only t)
  :hook   (prog-mode . guru-mode))

;;;;;;;;
;;
;;  TO-DO Highlighting
;;
;;;;;;;;
(use-package hl-todo
  :ensure t :delight
  :custom
  (global-hl-todo-mode +1)
  :config
  (setq hl-todo-keyword-faces
        '(
          ;; Default settings
          ("HOLD"   . "#d0bf8f")
          ("NEXT"   . "#dca3a3")
          ("THEM"   . "#dc8cc3")
          ("PROG"   . "#7cb8bb")
          ("OKAY"   . "#7cb8bb")
          ("DONT"   . "#5f7f5f")
          ("FAIL"   . "#8c5353")
          ("DONE"   . "#afd8af")
          ("MAYBE"  . "#d0bf8f")
          ("KLUDGE" . "#d0bf8f")
          ("HACK"   . "#d0bf8f")
          ("TEMP"   . "#d0bf8f")
          ("FIXME"  . "#cc9393")
          ("XXXX*"  . "#cc9393")
          ;; My custom settings
          ("NOTE"   . "#FFFFFF")
          ("TODO"   . "#EED202")
          ("WARN"   . "#FF7900")
          ("ERROR"  . "#DD1D20"))))

;;;;;;;;
;;
;;  Project Management
;;
;;;;;;;;
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

;;;;;;;;
;;
;; Tree-based file navigation
;;
;;;;;;;;
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

;;;;;;;;
;;
;;  Parse-tree based syntax highlighting
;;
;;;;;;;;
(use-package tree-sitter
  :ensure t
  :config (global-tree-sitter-mode)
  :hook (tree-sitter-mode . tree-sitter-hl-mode))

;;
;; Pre-defined language grammars for tree-sitter
;;
(use-package tree-sitter-langs
  :ensure t
  :after (tree-sitter)
  :config
  ;;;; Move grammars to directory that tree-sitter can see them.
  (let* ((files (directory-files (tree-sitter-langs--bin-dir)
                                 nil "\\.dylib$")))
    (dolist (grammar-file files)
      (copy-file (concat (tree-sitter-langs--bin-dir) grammar-file)
                 (concat (expand-file-name user-emacs-directory)
                         "tree-sitter/" "libtree-sitter-" grammar-file)
                 t)
      (message "%s grammar files copied" (length files)))))

;;;;;;;;
;;
;;  Pop-up display shows hotkeys for prefix
;;
;;;;;;;;
(use-package which-key
  :ensure t
  :delight which-key-mode
  :init (setq which-key-show-early-on-C-h t
              which-key-popup-type 'side-window)
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;;;;;;;;
;;
;;  Snippets
;;
;;;;;;;;
(use-package yasnippet
  :init (setq yas-snippet-dirs
              '(;; Personal snippets
                "~/.emacs.d/snippets"
                ;; Official snippets
                "~/.emacs.d/elpa/yasnippet-snippets-1.0/snippets/"))
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;                    Hotkeys and Keybindings
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allow use of the menu button for \\[execute-extended-command]. aka M-x
(global-set-key (kbd "<apps>") 'execute-extended-command)

;; Allow changing windows in reverse order
(defun other-window-reverse ()
  "Switch focused window in reverse order from default."
  (other-window -1))

(global-set-key (kbd "C-x O") 'other-window-reverse)

;;;;;;;;;;;;;;;;
;;
;;    MacBook Hotkeys
;;
;;;;;;;;;;;;;;;;

(when (string= "macbook" MY--current-machine)
  ;; Allow Cmd-Q to close the program like anything else.
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

  (setq select-enable-clipboard t)
  ;; paste-from-system
  (global-set-key (kbd "s-v") 'clipboard-yank)
  ;; copy-to-system
  (global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
  ;; cut-to-system
  (global-set-key (kbd "s-x") 'clipboard-kill-region)

  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "<home>") 'beginning-of-buffer)
  (global-set-key (kbd "<end>") 'end-of-buffer)

  (setq
   ;; Change modifier keys to fit muscle memory from previous installs.
   mac-command-modifier 'super
   mac-control-modifier 'control
   mac-option-modifier  'meta
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;                      Language Modes
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package aggressive-indent
  :ensure t :delight)

(use-package autorevert :delight auto-revert-mode)

(use-package paredit
  :ensure t :delight
  :hook   ((emacs-lisp-mode . paredit-mode)
           (eval-expression-minibuffer-setup . paredit-mode)
           (lisp-mode . paredit-mode)
           (lisp-interaction-mode . paredit-mode))
  :bind (:map paredit-mode-map
              ("C-S-f" . forward-sexp)
              ("C-S-b" . backward-sexp)))

(use-package rainbow-delimiters
  :ensure t
  :hook   (prog-mode . rainbow-delimiters-mode))

(use-package whitespace
  :delight
  :config
  (setq whitespace-style '(face empty tabs trailing))
  (global-whitespace-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t :delight whitespace-cleanup-mode
  :hook   ((prog-mode . whitespace-cleanup-mode)
           (text-mode . whitespace-cleanup-mode)))

(use-package yafolding
  :ensure t :delight
  :hook   ((prog-mode . yafolding-mode)
           (conf-mode . yafolding-mode))
  :bind   (:map yafolding-mode-map
                ("C-S-<tab>" . yafolding-hide-parent-element)
                ("C-M-<tab>" . yafolding-toggle-all)
                ("C-<tab>" . yafolding-toggle-element)))

;;;;;;;;;;;;;;;;
;;
;;    Miscellaneous, low frequency
;;
;;;;;;;;;;;;;;;;

(use-package csv-mode :ensure t
  :bind (:map csv-mode-map ("C-c C-a" . csv-align-fields)))
(use-package dockerfile-mode :ensure  t :delight dockerfile-mode)
(use-package elisp-mode :delight emacs-lisp-mode)
(use-package graphviz-dot-mode :ensure t
  :custom (graphviz-dot-indent-width 4))
(use-package kotlin-mode :ensure t :diminish kotlin-mode)
(use-package lua-mode :ensure t :delight lua-mode)
(use-package php-mode :ensure t :delight)
(use-package scheme :delight scheme-mode
  :hook (scheme-mode . paredit-mode))
(use-package terraform-mode :ensure t  :delight terraform-mode)
(use-package text-mode :delight text-mode
  :hook (text-mode . auto-fill-mode))
(use-package web-mode :ensure t
  :mode (("\\.jinja\\'" . web-mode)))

(use-package geiser :ensure t
  :custom
  (geiser-active-implementations '(mit))
  (geiser-set-default-implementation 'mit))

;;;;;;;;;;;;;;;;
;;
;;    Clojure
;;
;;;;;;;;;;;;;;;;

(use-package cider
  :after  (aggressive-indent)
  :ensure t :delight
  :init   (setq nrepl-use-ssh-fallback-for-remote-hosts t
                cider-save-file-on-load t
                nrepl-log-messages t)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'cider-repl-mode))

(use-package cider-repl
  :after  (clojure-mode paredit-mode)
  :delight
  :config (setq cider-repl-use-pretty-printing t)
  :hook   (cider-repl-mode . (lambda () (paredit-mode 1)))
  :bind   (:map cider-repl-mode-map
                ("C-c M-i" . cider-inspect)
                :map clojurescript-mode-map
                ("C-c M-i" . cider-inspect)
                :map clojure-mode-map
                ("C-c M-i" . cider-inspect)))

(use-package clojure-mode
  :ensure t :delight clojure-mode
  :after  (paredit aggressive-indent)
  :hook   ((clojure-mode . (lambda ()
                             (clj-refactor-mode 1)))
           (clojure-mode . eldoc-mode)
           (clojure-mode . paredit-mode)
           (clojure-mode . (lambda () (aggressive-indent-mode 1)))
           (clojurescript-mode . paredit-mode))
  :config (define-clojure-indent
            (defroutes 'defun)
            (alet 1)
            (GET 2)
            (POST 2)
            (PUT 2)
            (DELETE 2)
            (HEAD 2)
            (ANY 2)
            (OPTIONS 2)
            (PATCH 2)
            (rfn 2)
            (let-routes 1)
            (context 2)))

(use-package clj-refactor
  :ensure t :delight
  :hook   (clojure-mode . (lambda () (clj-refactor-mode 1)))
  :config
  (setq cljr-warn-on-eval nil
        cljr-magic-require-namespaces
        '(("io"   . "clojure.java.io")
          ("set"  . "clojure.set")
          ("str"  . "clojure.string")
          ("walk" . "clojure.walk")
          ("zip"  . "clojure.zip")
          ("time" . "clj-time.core")
          ("log"  . "clojure.tools.logging")
          ("json" . "cheshire.core")))
  (cljr-add-keybindings-with-prefix "C-c C-m"))

;;;;;;;;;;;;;;;;
;;
;;    HTML
;;
;;;;;;;;;;;;;;;;

(defun html-format ()
  "Machine-format text to human-readable form assuming HTML syntax."
  (interactive)
  (sgml-pretty-print (mark) (point)))

(defun base64-decode-region-tobuff ()
  (interactive)
  (with-output-to-temp-buffer "*<base64> decoded*"
    (when (use-region-p)
      (print (base64-decode-string (buffer-substring (region-beginning) (region-end)))))))


;;;;;;;;;;;;;;;;
;;
;;    JavaScript
;;
;;;;;;;;;;;;;;;;

(use-package nvm
  :ensure t
  :config
  ;; Optionally set a default node version
  (nvm-use "18"))

(use-package typescript-mode
  :ensure t
  :after (flycheck)
  :delight typescript-mode
  :custom
  (typescript-indent-level 2))

(use-package tide
  :ensure t
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode))
  :hook  ((typescript-mode . tide-setup)
          (typescript-ts-mode . tide-setup)
          (tsx-ts-mode . tide-setup)))


;;;;;;;;;;;;;;;;
;;
;;    JSON
;;
;;;;;;;;;;;;;;;;

(use-package json-mode :ensure t :delight)

(defun json-format ()
  "Format JSON block to adhere to readable format."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark)
                             (point)
                             "python3 -m json.tool"
                             (buffer-name)
                             t)))

(defun json-parse ()
  (interactive)
  (json-read-from-string (buffer-substring (mark) (point))))

;;;;;;;;;;;;;;;;
;;
;;    Markdown
;;
;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :ensure  t
  :delight markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . auto-fill-mode)
  :init (setq markdown-command "multimarkdown"))

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

;;;;;;;;;;;;;;;;
;;
;;    Org-Mode
;;
;;;;;;;;;;;;;;;;

(use-package org-mode
  :hook (org-mode . turn-on-font-lock)
  :delight org
  :custom
  (org-src-fontify-natively t "Apply font to code blocks")
  (org-confirm-babel-evaluate nil)
  (org-adapt-indentation nil)
  (org-hide-leading-stars t)
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((scheme . t)
                                 (shell  . t))))

;;;;;;;;;;;;;;;;
;;
;;    Python
;;
;;;;;;;;;;;;;;;;

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-python-command "python3"
        python-indent-guess-indent-offset-verbose nil
        python-shell-completion-native-enable nil
        python-shell-interpreter "python3")
  :config
  (elpy-enable))

;;;;;;;;;;;;;;;;
;;
;;    Shell
;;
;;;;;;;;;;;;;;;;

(use-package sh-script :delight sh-mode
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

;;;;;;;;;;;;;;;;
;;
;;    SQL
;;
;;;;;;;;;;;;;;;;

(use-package sqlformat
  :ensure t
  :hook (sql-mode . sqlformat-on-save)
  :bind (:map sql-mode-map
              ("C-c C-f" . 'sqlformat))
  :config
  (setq sqlformat-command 'sqlformat)
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat))

;;;;;;;;;;;;;;;;
;;
;;    XML
;;
;;;;;;;;;;;;;;;;

(defun xml-format ()
  "Format XML block to a readable format."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark)
                             (point)
                             "xmllint --format --encode utf-8 -"
                             (buffer-name)
                             t)))

;;;;;;;;;;;;;;;;
;;
;;    YAML
;;
;;;;;;;;;;;;;;;;

(use-package yaml-mode
  :ensure  t
  :delight yaml-mode
  :after   (yafolding)
  :mode    (("\\.jinja\\.schema\\'" . yaml-mode))
  :hook    ((yaml-mode . yafolding-mode)
            (yaml-mode . turn-off-auto-fill)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;                    
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'mode-line "~/.emacs.d/mode-line.el")

(require 'eshell)
(declare-function eshell/pwd "ext:eshell/pwd")
(defvar eshell-prompt-function
  (lambda ()
    (concat
     (propertize (format-time-string "%-I:%M:%S%p " (current-time))
                 'face `(:foreground "#aaaaff"))
     (propertize (abbreviate-file-name (eshell/pwd))
                 'face `(:foreground "#aaaa44"))
     (if (= (user-uid) 0) " # " " $ "))))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(require 'json)

(when (file-exists-p "~/.emacs.d/secrets/tokens.el")
  (load "~/.emacs.d/secrets/tokens.el"))



;;; Commentary:

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-check-syntax-automatically
   '(save idle-change idle-buffer-switch new-line mode-enabled))
 '(js-chain-indent nil)
 '(package-check-signature nil)
 '(package-selected-packages
   '(ag aggressive-indent alchemist auto-dim-other-buffers beacon
        clj-refactor color-theme-sanityinc-solarized
        color-theme-sanityinc-tomorrow csv-mode dashboard delight
        direnv dockerfile-mode elpy exercism flymd geiser
        graphviz-dot-mode guru-mode hl-todo json-mode jtsx kotlin-mode
        lua-mode major-mode-icons markdown-mode nvm page-break-lines
        php-mode rainbow-delimiters restclient sqlformat
        terraform-mode tide tree-sitter-langs treemacs-all-the-icons
        treemacs-icons-dired treemacs-magit treemacs-projectile
        typescript-mode uuidgen web-mode whitespace-cleanup-mode
        yafolding yaml-mode yasnippet-snippets))
 '(safe-local-variable-values
   '((eval progn
           (add-to-list 'exec-path
                        (concat
                         (locate-dominating-file default-directory
                                                 ".dir-locals.el")
                         "node_modules/.bin/"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
