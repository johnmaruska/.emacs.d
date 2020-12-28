;;; package -- Summary:

;; installs required packages

;;; Commentary:

;;; Code:

;;; Package handling
(require 'package)
(setq package-archives
      '(("ELPA" . "http://tromey.com/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(defvar package-list)
(setq package-list
  '(ag
    aggressive-indent
    all-the-icons
    all-the-icons-dired
    auto-complete
    auto-dim-other-buffers
    beacon
    cider
    clojure-mode
    clj-refactor
    color-theme-sanityinc-tomorrow
    dashboard  ; emacs-dashboard
    editorconfig
    flycheck
    flycheck-clojure
    flycheck-pos-tip
    flymd
    geiser
    groovy-mode
    guru-mode
    ht
    inf-clojure
    json-mode
    markdown-mode
    magit-gitflow
    multi-term
    multiple-cursors
    nix-mode
    neotree
    nodejs-repl
    page-break-lines
    paredit
    projectile
    rainbow-delimiters
    restclient
    rjsx-mode
    rust-mode
    terraform-doc
    terraform-mode
    use-package
    uuidgen
    which-key
    whitespace-cleanup-mode
    wsd-mode
    w3m
    yafolding
    yaml-mode))

(require 'url-http)
(defun install-all-packages ()
  "Attempts to `package-install` each package in `package-list`."
  (package-initialize)
  ;; (setq url-http-attempt-keepalives nil)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(provide 'packages)
;;; packages.el ends here
