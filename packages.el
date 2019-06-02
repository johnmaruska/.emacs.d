;;; package -- Summary:

;; installs required packages

;;; Commentary:

;; Package install code is lengthy and gross, ship it off over here.

;;; Code:

;;; Package handling
(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

(defvar package-list)
(setq package-list '(ack-and-a-half
                     aggressive-indent
                     auto-complete
                     cider
                     clojure-mode
                     color-theme-sanityinc-tomorrow
                     ensime
                     flycheck
                     flycheck-clojure
                     flycheck-pos-tip
                     flycheck-rust
                     geiser
                     groovy-mode
                     markdown-mode
                     magit-gitflow
                     multiple-cursors
                     neotree
                     paredit
                     rainbow-delimiters
                     rjsx-mode
                     rust-mode
                     scala-mode
                     whitespace-cleanup-mode
                     w3m
                     yafolding
                     yaml-mode))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(require 'url-http)
(setq url-http-attempt-keepalives nil)
(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(provide 'packages)
;;; packages.el ends here
