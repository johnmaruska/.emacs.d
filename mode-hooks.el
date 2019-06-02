;;; package -- Summary:

;; management for major and minor mode hooks and settings

;;; Commentary:

;; mostly these are pretty straightforward.  Every new language is gonna need
;; to get a new entry when I start using it, and any commonly shared modes
;; will need that new language added.

;;; Code:

;;; aggressive indent
(add-hook 'prog-mode-hook 'aggressive-indent-mode)

;;; Clojure
(require 'clojure-mode)
(require 'cider)
(defun my-clojure-mode-hook ()
  "Handles all configuration for Clojure mode."
  (paredit-mode 1)
  (whitespace-mode 1))
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
  (context 2))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'clojure-mode-hook 'cider-mode)
(eval-after-load 'cider
  '(progn
     (add-hook 'cider-repl-mode-hook 'paredit-mode)
     (local-set-key (kbd "M-<return>") 'newline)))

(defun clj-refactor-mode-hook ()
  "Handles configuration for clojure's refactor mode."
  (clj-refactor-mode 1)
  ;; for adding require/use/import statements
  (yas-minor-mode 1))
(add-hook 'clojure-mode-hook #'clj-refactor-mode-hook)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Org
(add-hook 'org-mode-hook 'turn-on-font-lock)

;;; JavaScript
(require 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;;; JSON
(defun json-format ()
  "Format block to adhere to readable JSON format."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

;;; Markdown
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)


;;; Paredit
(mapc #'(lambda (mode-hook)
          (add-hook mode-hook 'paredit-mode))
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        eval-expression-minibuffer-setup-hook
        json-mode-hook
        lisp-mode-hook
        lisp-interaction-mode-hook))

;; Rainbow delimiters (colored parens)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;; Rust
(require 'rust-mode)
(add-hook 'rust-mode-hook 'flycheck-rust-setup)

;;; shell script mode
(defun my-shell-script-mode-hook ()
  "Handles all configuration for Clojure mode."
  (sh-basic-offset 2))
(add-hook 'shell-script-mode-hook 'my-shell-script-mode-hook)

;; yafolding
(require 'yafolding)
(add-hook 'prog-mode-hook 'yafolding-mode)

;;; YAML
(require 'yaml-mode)
(add-hook 'yaml-mode-hook 'linum-mode)

;;; Whitespace mode
(require 'whitespace)
(setq whitespace-style '(face empty tabs trailing))
(global-whitespace-mode 0)
(mapc (lambda (mode-hook)
        (add-hook mode-hook 'whitespace-mode))
      '(c-mode-hook
        c++-mode-hook
        clojure-mode-hook
        emacs-lisp-mode-hook
        html-mode-hook
        lisp-mode-hook
        prog-mode-hook
        python-mode-hook
        ruby-mode-hook
        rust-mode-hook
        scala-mode-hook
        shell-script-mode-hook))

(provide 'mode-hooks)
;;; mode-hooks.el ends here
