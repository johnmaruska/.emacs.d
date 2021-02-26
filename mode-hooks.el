;;; package -- Summary:

;; management for major and minor mode hooks and settings

;;; Commentary:

;; use-package is neat

;;; Code:

(use-package aggressive-indent
  :ensure t
  :hook (prog-mode . (lambda () (aggressive-indent-mode 1))))

(use-package all-the-icons :ensure t)

(use-package cider :ensure t)

(use-package cider-repl
  :after  (clojure-mode paredit-mode)
  :config (setq cider-repl-use-pretty-printing t)
  :hook   (cider-repl-mode . paredit-mode)
  :bind   (:map cider-repl-mode-map
                ("C-c M-i" . cider-inspect)
                :map clojurescript-mode-map
                ("C-c M-i" . cider-inspect)
                :map clojure-mode-map
                ("C-c M-i" . cider-inspect)))

(use-package clojure-mode
  :ensure t
  :after  (paredit)
  :hook   ((clojure-mode . (lambda ()
                             (clj-refactor-mode 1)))
           (clojure-mode . eldoc-mode)
           (clojure-mode . paredit-mode)
           (clojurescript-mode . paredit-mode))
  :config
  (define-clojure-indent
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
  :ensure t
  :hook (clojure-mode . (lambda () (clj-refactor-mode 1)))
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

(use-package dockerfile-mode
  :ensure t)

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

(use-package guru-mode
  :ensure t
  :init (setq guru-warn-only t)
  :hook (prog-mode . guru-mode))

(use-package json-mode :ensure t)

(use-package magit-gitflow
  :ensure t
  :bind ("C-x g" . magit-status)
  :hook (magit-mode . turn-on-magit-gitflow))

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  :hook (markdown-mode . auto-fill-mode))

(use-package nix-mode :ensure t)

(use-package org-mode
  :hook (org-mode . turn-on-font-lock)
  :config
  (setq org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-indent-indentation-per-level 2
        org-adapt-indentation nil
        org-hide-leading-stars 't)
  (org-babel-do-load-languages 'org-babel-load-languages '((scheme . t))))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package restclient :ensure t)

(use-package rjsx-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(use-package sh-script
  :config
  (defvar sh-basic-offset 2)
  (defvar sh-indentation 2))

(use-package terraform-mode :ensure t)

(use-package text-mode
  :hook (text-mode . auto-fill-mode))

(use-package whitespace
  :config
  (setq whitespace-style '(face empty tabs trailing))
  (global-whitespace-mode 1))

(use-package whitespace-cleanup-mode
  :ensure t
  :hook ((prog-mode . whitespace-cleanup-mode)
         (text-mode . whitespace-cleanup-mode)))

(use-package yafolding
  :ensure t
  :hook (prog-mode . yafolding-mode)
  :bind (:map yafolding-mode-map
              ("C-S-RET" . yafolding-hide-parent-element)
              ("C-M-RET" . yafolding-toggle-all)
              ("C-RET" . yafolding-toggle-element)))

(use-package yaml-mode
  :ensure t
  :after (yafolding)
  :hook  ((yaml-mode . linum-mode)
          (yaml-mode . yafolding-mode)))

(provide 'mode-hooks)
;;; mode-hooks.el ends here
