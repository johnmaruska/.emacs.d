;;; package -- Summary:

;; management for major and minor mode hooks and settings

;;; Commentary:

;; mostly these are pretty straightforward.  Every new language is gonna need
;; to get a new entry when I start using it, and any commonly shared modes
;; will need that new language added.

;;; Code:

(require 'clojure-mode)
(require 'clj-refactor)
(defun configure-clojure ()
  "Configures hooks for interacting with Clojure."
  (define-key cider-repl-mode-map (kbd "C-c M-i") #'cider-inspect)
  (define-key clojurescript-mode-map (kbd "C-c M-i") #'cider-inspect)
  (define-key clojure-mode-map (kbd "C-c M-i") #'cider-inspect)
  (setq cider-repl-use-pretty-printing t)
  (setq cljr-magic-require-namespaces
      '(("io"   . "clojure.java.io")
        ("set"  . "clojure.set")
        ("str"  . "clojure.string")
        ("walk" . "clojure.walk")
        ("zip"  . "clojure.zip")
        ("time" . "clj-time.core")
        ("log"  . "clojure.tools.logging")
        ("json" . "cheshire.core")))
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook
            (lambda ()
              ;; clojure refactor
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-m")
              ;; auto indenting
              (aggressive-indent-mode 1)
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
                (context 2)))))

(defun configure-dired ()
  "Configures dired settings."
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(require 'flycheck)
(require 'flycheck-pos-tip)
(defun configure-flycheck ()
  "Configures general flycheck settings - not language specific setup."
  ;; (add-hook 'after-init-hook #'global-flycheck-mode)

  ;; flycheck-pos-tip prevents linting and type errors from clashing with
  ;; cider's eldoc information
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(defun configure-javascript ()
  "Configures necessary for interacting with JavaScript."
  (require 'rjsx-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (setq js-indent-level 2)
  (add-hook 'js-mode-hook
            (lambda ()  ; bind nodejs-repl commands
              (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
              (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
              (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
              (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
              (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl))))

(defun configure-magit ()
  "Configures magit."
  (require 'magit-gitflow)
  (global-set-key (kbd "C-x g") 'magit-status)
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(defun my-flymd-browser-function (url)
  "Use Mozilla Firefox for flymd instead of Google Chrome.

Google Chrome has support issues with flymd. This is the recommended solution.
<https://github.com/mola-T/flymd/blob/master/browser.md#user-content-chrome-macos>"
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           (concat "firefox " url)
             nil
             "/usr/bin/open"
             (list "-a" "firefox" url))))

(defun configure-markdown ()
  "Configures necessary for interacting with Markdown."
  (require 'markdown-mode)
  (require 'flymd)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-hook 'markdown-mode #'auto-fill-mode)
  (setq flymd-browser-open-function 'my-flymd-browser-function))

(defun configure-org-mode ()
  "Configures necessary for interacting with `org-mode`."
  (add-hook 'org-mode-hook #'turn-on-font-lock)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-indent-indentation-per-level 2)
  (setq org-adapt-indentation nil)
  (setq org-hide-leading-stars 't))

(defun configure-prog-mode ()
  "Configures `prog-mode` major mode which informs most programming modes."
  (require 'yafolding)
  (aggressive-indent-mode 1)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (setq yafolding-mode-map
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "<C-S-return>") #'yafolding-hide-parent-element)
          (define-key map (kbd "<C-M-return>") #'yafolding-toggle-all)
          (define-key map (kbd "<C-return>") #'yafolding-toggle-element)
          map))
  (add-hook 'prog-mode-hook 'yafolding-mode))

(defun configure-rust ()
  "Configures necessary for interacting with Rust."
  (require 'rust-mode)
  (add-hook 'rust-mode-hook 'flycheck-rust-setup))

(defun configure-shell-script ()
  "Configures necessary for interacting with Shell scripts."
  (add-hook 'sh-mode-hook
            (lambda ()
              (defvar sh-basic-offset 2)
              (defvar sh-indentation 2))))

(defun configure-text ()
  "Configures necessary for interacting with text."
  (add-hook 'text-mode-hook #'auto-fill-mode))

(require 'whitespace)
(defun configure-whitespace-mode ()
  "Configuration settings for global `whitespace-mode`."
  (setq whitespace-style '(face empty tabs trailing))
  (global-whitespace-mode 1))


(defun configure-yaml ()
  "Configures necessary for interacting with YAML files."
  (require 'yaml-mode)
  (add-hook 'yaml-mode-hook 'linum-mode)
  (add-hook 'yaml-mode-hook 'yafolding-mode))

(defun attach-paredit-minor-mode ()
  "Attaches minor mode Paredit to all major modes which use it."
  (mapc #'(lambda (mode-hook)
            (add-hook mode-hook #'paredit-mode))
        '(cider-repl-mode-hook
          clojure-mode-hook
          clojurescript-mode-hook
          emacs-lisp-mode-hook
          eval-expression-minibuffer-setup-hook
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
  (attach-paredit-minor-mode)
  (configure-flycheck)
  (configure-text)
  (configure-whitespace-mode))

(defun configure-all-modes ()
  "Configures all custom modified modes."
  (configure-major-modes)
  (configure-minor-modes))

(provide 'mode-hooks)
;;; mode-hooks.el ends here
