;;; package -- Summary:

;; management for major and minor mode hooks and settings

;;; Code:

;;; Clojure
(require 'clojure-mode)
(require 'cider)
(require 'clj-refactor)
(defun my-clojure-mode-hook ()
  "Handles all configuration for Clojure mode."
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  (paredit-mode 1)
  (whitespace-mode 1))
(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

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
        python-mode-hook
        ruby-mode-hook
        rust-mode-hook
        shell-script-mode-hook))

;;; Commentary:
(provide 'mode-hooks)
;;; mode-hooks.el ends here
