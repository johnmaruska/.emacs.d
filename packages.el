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
      '(;; ag
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
        geiser
        guru-mode
        ht
        inf-clojure
        json-mode
        markdown-mode
        magit-gitflow
        multiple-cursors
        neotree
        page-break-lines
        paredit
        projectile
        rainbow-delimiters
        restclient
        rjsx-mode
        rust-mode
        terraform-mode
        use-package
        uuidgen
        which-key
        whitespace-cleanup-mode
        yafolding
        yaml-mode))

(defun install-missing (package)
  (unless (package-installed-p package)
    (package-install package)))

(require 'url-http)
(defun install-all-packages ()
  "Attempts to `package-install` each package in `package-list`."
  (package-initialize)
  ;; (setq url-http-attempt-keepalives nil)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-list)
    (install-missing package)))

(provide 'packages)
;;; packages.el ends here
