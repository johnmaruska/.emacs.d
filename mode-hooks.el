;;; package -- Summary:

;; management for major and minor mode hooks and settings

;;; Commentary:

;;; Code:

;;; Generic modes

(use-package aggressive-indent
  :ensure t :delight)

(use-package autorevert :delight auto-revert-mode)

(use-package guru-mode
  :ensure t :delight guru-mode
  :custom (guru-warn-only t)
  :hook   (prog-mode . guru-mode))

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

;;;; Specific modes

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

(use-package csv-mode
  :ensure t
  :bind (:map csv-mode-map
              ("C-c C-a" . csv-align-fields)))

(use-package dockerfile-mode
  :ensure  t
  :delight dockerfile-mode)

(use-package graphviz-dot-mode
  :ensure t
  :config (setq graphviz-dot-indent-width 4))

(use-package elisp-mode
  :delight emacs-lisp-mode)

(use-package elpy
  :ensure t
  :init
  (setq elpy-rpc-python-command "python3"
        python-indent-guess-indent-offset-verbose nil
        python-shell-completion-native-enable nil
        python-shell-interpreter "python3")
  :config
  (elpy-enable))

(use-package geiser
  :ensure t
  :custom
  (geiser-active-implementations '(mit))
  (geiser-set-default-implementation 'mit))

(use-package json-mode
  :ensure  t
  :delight json-mode)

(use-package kotlin-mode
  :ensure t
  :diminish kotlin-mode)

(use-package lua-mode
  :ensure t
  :delight lua-mode)

(use-package markdown-mode
  :ensure  t
  :delight markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . auto-fill-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package org-mode
  :hook (org-mode . turn-on-font-lock)
  :delight org
  :custom
  (org-src-fontify-natively t "Apply font to code blocks")
  (org-confirm-babel-evaluate nil)
  (org-adapt-indentation nil)
  (org-hide-leading-stars t)
  :config
  (setq org-indent-indentation-per-level 2)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((scheme . t)
                                 (shell  . t))))

(use-package php-mode
  :ensure t
  :delight)

(use-package scheme
  :delight scheme-mode
  :hook    (scheme-mode . paredit-mode))

(use-package sh-script
  :delight sh-mode
  :config
  (defvar sh-basic-offset 2)
  (defvar sh-indentation 2))

(use-package sqlformat
  :ensure t
  :hook (sql-mode . sqlformat-on-save)
  :bind (:map sql-mode-map
              ("C-c C-f" . 'sqlformat))
  :config
  (setq sqlformat-command 'sqlformat)
  (define-key sql-mode-map (kbd "C-c C-f") 'sqlformat))

(use-package terraform-mode
  :ensure t  :delight terraform-mode)

(use-package text-mode
  :delight text-mode
  :hook    (text-mode . auto-fill-mode))

;; web templates e.g. jinja, jsx, mustache
(use-package web-mode
  :ensure t
  :mode   (("\\.jinja\\'" . web-mode)))

(use-package yaml-mode
  :ensure  t
  :delight yaml-mode
  :after   (yafolding)
  :mode    (("\\.jinja\\.schema\\'" . yaml-mode))
  :hook    ((yaml-mode . yafolding-mode)
            (yaml-mode . turn-off-auto-fill)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Elixir
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package exercism :ensure t)

(use-package elixir-mode
  :ensure t
  :delight elixir-mode
  :diminish elixir-mode)

;;; https://alchemist.readthedocs.io/en/latest/
;;; Alchemist is a pile of tools for working with Elixir as an IDE
(use-package alchemist
  :ensure t
  :init
  (setq alchemist-hooks-test-on-save t
        alchemist-hooks-compile-on-save t)
  :mode (("\\.ex\\'" . alchemist-mode)
         ("\\.exs\\'" . alchemist-test-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; JavaScript Modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package prettier
  :ensure t
  :hook  ((rjsx-mode . prettier-mode)
          (jtsx-tsx-mode . prettier-mode)
          (jtsx-jsx-mode . prettier-mode)
          (typescript-mode . prettier-mode))
  ;; M-x customize-group prettier
  )

(use-package js2-mode
  :ensure t :delight js2-mode
  :hook (js2-mode . js2-refactor-mode)
  :init
  (setq js-switch-indent-offset 2
        js2-basic-offset 2
        js2-bounce-indent-p t)
  :custom
  (js-indent-level 2))

(use-package rjsx-mode
  :ensure t
  :delight rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode)))

(use-package js2-refactor
  :ensure t :delight)

(use-package typescript-mode
  :ensure t
  :delight typescript-mode
  :mode (("\\.ts\\'" . typescript-mode))
  :custom
  (typescript-indent-level 2))

(use-package jtsx
  :ensure t
  :mode (("\\.tsx\\'" . jtsx-tsx-mode)
         ("\\.jsx\\'" . jtsx-jsx-mode)))

(defun setup-tide-mode ()
  (interactive)
  (message "Fired function.")
  (tide-setup))

(use-package tide
  :ensure t
  :hook  ((tsx-ts-mode . setup-tide-mode)
          (jtsx-tsx-mode . setup-tide-mode)
          (typescript-mode . setup-tide-mode)
          (typescript-ts-mode . setup-tide-mode))
  :init
  (add-hook 'typescript-ts-mode-hook #'setup-tide-mode)
  (add-hook 'jtsx-tsx-mode-hook #'setup-tide-mode)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  :config
  (flycheck-add-mode 'typescript-tide 'jtsx-tsx-mode)
  (flycheck-add-mode 'typescript-tide 'rjsx-mode))

(provide 'mode-hooks)
;;; mode-hooks.el ends here
