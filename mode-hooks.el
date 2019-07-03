;;; package -- Summary:

;; management for major and minor mode hooks and settings

;;; Commentary:

;; mostly these are pretty straightforward.  Every new language is gonna need
;; to get a new entry when I start using it, and any commonly shared modes
;; will need that new language added.

;;; Code:

(defun configure-prog-mode ()
  "Configures `prog-mode` major mode which informs most programming modes."
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (require 'yafolding)
  (add-hook 'prog-mode-hook 'yafolding-mode))

(require 'clj-refactor)
(defun my-clojure-mode-hook ()
  "Handles all configuration for Clojure mode."
  (require 'clojure-mode)
  (require 'cider)
  (aggressive-indent-mode 1)
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  (paredit-mode 1)
  (whitespace-mode 1)
  (define-clojure-indent
    (defroutes 'defun)
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
(defun configure-clojure ()
  "Configures hooks for interacting with Clojure."
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

(defun configure-org-mode ()
  "Configures necessary for interacting with `org-mode`."
  (add-hook 'org-mode-hook #'turn-on-font-lock))

(defun configure-javascript ()
  "Configures necessary for interacting with JavaScript."
  (require 'rjsx-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(defun configure-magit ()
  "Configures magit."
  (require 'magit-gitflow)
  (global-set-key (kbd "C-x g") 'magit-status)
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(defun configure-markdown ()
  "Configures necessary for interacting with Markdown."
  (require 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

(defun configure-rust ()
  "Configures necessary for interacting with Rust."
  (require 'rust-mode)
  (add-hook 'rust-mode-hook 'flycheck-rust-setup))

(defun my-shell-script-mode-hook ()
  "Handles all configuration for Clojure mode."
  (defvar sh-basic-offset 2)
  (defvar sh-indentation 2))
(defun configure-shell-script ()
  "Configures necessary for interacting with Shell scripts."
  (add-hook 'sh-mode-hook 'my-shell-script-mode-hook))

(defun configure-yaml ()
  "Configures necessary for interacting with YAML files."
  (require 'yaml-mode)
  (add-hook 'yaml-mode-hook 'linum-mode))

(defun attach-paredit-minor-mode ()
  "Attaches minor mode Paredit to all major modes which use it."
  (mapc #'(lambda (mode-hook)
            (add-hook mode-hook #'paredit-mode))
        '(clojure-mode-hook
          emacs-lisp-mode-hook
          eval-expression-minibuffer-setup-hook
          json-mode-hook
          lisp-mode-hook
          lisp-interaction-mode-hook)))

(defun configure-major-modes ()
  "Configures all custom modified major modes."
  (configure-prog-mode)
  (configure-clojure)
  (configure-javascript)
  (configure-magit)
  (configure-markdown)
  (configure-org-mode)
  (configure-rust)
  (configure-shell-script)
  (configure-yaml))

(defun configure-minor-modes ()
  "Configures all custom modified minor modes."
  (attach-paredit-minor-mode))

(defun configure-all-modes ()
  "Configures all custom modified modes."
  (configure-major-modes)
  (configure-minor-modes))

(provide 'mode-hooks)
;;; mode-hooks.el ends here
