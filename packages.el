;;; package -- Summary:

;; installs required packages

;;; Commentary:

;;; Code:

;;; Package handling
(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

(defvar package-list
  '(ack-and-a-half
    aggressive-indent
    auto-complete
    cider
    clojure-mode
    clj-refactor
    color-theme-sanityinc-tomorrow
    ensime
    emacs-xkcd
    flycheck
    flycheck-clojure
    flycheck-pos-tip
    geiser
    groovy-mode
    json
    markdown-mode
    magit-gitflow
    multi-term
    multiple-cursors
    neotree
    paredit
    rainbow-delimiters
    restclient
    rjsx-mode
    rust-mode
    scala-mode
    terraform-doc
    terraform-mode
    uuidgen
    which-key
    whitespace-cleanup-mode
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
