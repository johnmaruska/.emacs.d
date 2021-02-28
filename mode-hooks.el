;;; package -- Summary:

;; management for major and minor mode hooks and settings


;;; Generic modes

(use-package aggressive-indent
  :ensure t
  :hook   (prog-mode . (lambda () (aggressive-indent-mode 1))))

(use-package autorevert
  :delight auto-revert-mode)

(use-package guru-mode
  :ensure  t
  :delight guru-mode
  :init    (setq guru-warn-only t)
  :hook    (prog-mode . guru-mode))

(use-package paredit
  :ensure  t
  :delight (paredit-mode " ()")
  :hook    ((emacs-lisp-mode . paredit-mode)
            (eval-expression-minibuffer-setup . paredit-mode)
            (lisp-mode . paredit-mode)
            (lisp-interaction-mode . paredit-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook   (prog-mode . rainbow-delimiters-mode))

(use-package whitespace
  :delight global-whitespace-mode
  :config
  (setq whitespace-style '(face empty tabs trailing))
  (global-whitespace-mode 1))

(use-package whitespace-cleanup-mode
  :ensure  t
  :delight whitespace-cleanup-mode
  :hook    ((prog-mode . whitespace-cleanup-mode)
            (text-mode . whitespace-cleanup-mode)))

(use-package yafolding
  :ensure t
  :hook   (prog-mode . yafolding-mode)
  :bind   (:map yafolding-mode-map
                ("C-S-RET" . yafolding-hide-parent-element)
                ("C-M-RET" . yafolding-toggle-all)
                ("C-RET" . yafolding-toggle-element)))

;;;; Specific modes
>>>>>>> origin/modeline-customization

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
  :ensure  t
  :delight clojure-mode
  :after   (paredit)
  :hook    ((clojure-mode . (lambda ()
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

(use-package dockerfile-mode
  :ensure  t
  :delight dockerfile-mode)

(use-package elisp-mode
  :delight emacs-lisp-mode)

(use-package json-mode
  :ensure  t
  :delight json-mode)

(use-package markdown-mode
  :ensure  t
  :delight markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  :hook (markdown-mode . auto-fill-mode))

(use-package nix-mode
  :ensure  t
  :delight nix-mode)

(use-package org-mode
  :hook (org-mode . turn-on-font-lock)
  :delight org
  :config
  (setq org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-indent-indentation-per-level 2
        org-adapt-indentation nil
        org-hide-leading-stars 't)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((scheme . t))))

(use-package rjsx-mode
  :ensure  t
  :delight rjsx-mode
  :config  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(use-package sh-script
  :delight sh-mode
  :config
  (defvar sh-basic-offset 2)
  (defvar sh-indentation 2))

(use-package terraform-mode
  :ensure  t
  :delight terraform-mode)

(use-package text-mode
  :delight text-mode
  :hook    (text-mode . auto-fill-mode))

(use-package yaml-mode
  :ensure  t
  :delight yaml-mode
  :after   (yafolding)
  :hook    ((yaml-mode . linum-mode)
            (yaml-mode . yafolding-mode)))

(provide 'mode-hooks)
;;; mode-hooks.el ends here
