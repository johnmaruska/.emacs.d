;;; package -- Summary:

;; Custom Emacs environment for John Maruska. Based initially off of Chris Gore's
;; Emacs configuration. Stripped out minimal useful configuration and built own
;; on top of this.

;;; Code:

;; $PATH settings for subsystem processes and for eshell
(defvar path-list)
(setq path-list (list "/usr/local/opt/bin/go/libexec/bin"
                        (expand-file-name "~/bin")
                        "/usr/local/bin"
                        "/usr/local/sbin"
                        (expand-file-name "~/.rvm/bin")
                        (expand-file-name "~/.rvm/sbin")
                        "/usr/texbin"
                        "/Library/TeX/texbin"
                        "/usr/bin"
                        "/usr/sbin"
                        "/bin"
                        "/sbin"
                        "/opt/pixie"
                        "/opt/sbin"
                        "/usr/local/bin/exercism-mac-64bit"
                        "/usr/local/opt/postgresql@9.6/bin"
                        (expand-file-name "~/climate/bin")
                        (expand-file-name "~/.cargo/bin") ; Rust
                        (expand-file-name "~/gatling/bin")    ; Gatling
                        (expand-file-name "~/Documents/apache-maven-3.5.4")
                        (getenv "PATH")))

(when window-system
  ;; this matters for sub-shell process (e.g. launch bash)
  (setenv "PATH" (mapconcat 'identity path-list ":")))

(setenv "JAVA_HOME"
        "/Library/Java/JavaVirtualMachines/jdk1.8.0_162.jdk/Contents/Home")

;; this matters for eshell
(setq exec-path (append exec-path path-list))


;;; Package handling
(require 'package)
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(setq url-http-attempt-keepalives nil)
(unless package-archive-contents
  (package-refresh-contents))

(defvar package-list)
(setq package-list '(ack-and-a-half
                     auto-complete
                     cider
                     clj-refactor
                     clojure-mode
                     color-theme-sanityinc-tomorrow
                     ensime
                     flycheck
                     flycheck-clojure
                     flycheck-pos-tip
                     flycheck-rust
                     geiser
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
                     yaml-mode))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; EMBIGGEN
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; InteractivelyDoThings
(require 'ido)
(ido-mode t)

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(neotree-toggle)
(neotree-dir "~/dev/")

;; Autocomplete config
(require 'auto-complete)
(define-key ac-completing-map [return] nil) ; no enter (1.)
(define-key ac-completing-map "\r" nil) ; no enter (2.)
(define-key ac-completing-map "\t" 'ac-complete) ; use tab to complete
(put 'upcase-region 'disabled nil)

;; Miscellaneous settings
(setq-default indent-tabs-mode nil)  ; don't mix tabs and spaces
(tool-bar-mode -1)  ; hide tool bar
(scroll-bar-mode -1)  ; hide scroll bar
(menu-bar-mode 1)  ; show menu-bar. Doesn't affect Mac OS
(display-time-mode 1)  ; show clock in status bar
(column-number-mode 1)  ; show column numbers in addition to line numbers
(blink-cursor-mode 1)  ; cursor should blink
(global-linum-mode 1)
(show-paren-mode 1)  ; highlight matching paren
(setq inhibit-startup-screen t)  ; Don't display welcome screen
(setq default-directory "~/")
(setq scroll-step 1)  ; keyboard scroll one line at a time
(setq require-final-newline t)  ; files must end with a newline
;; TODO: did i need this for mac?
;; (define-key key-translation-map (kbd "C-p") (kbd "M-x"))

(load "~/.emacs.d/appearance.el")
(load "~/.emacs.d/mode-hooks.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(package-selected-packages
   (quote
    (multiple-cursors-mode w3m minesweeper magit-gitflow magit-flow multiple-cursors magit racket-mode geiser rjsx-mode ensime scala-mode command-log-mode neotree clojure-mode-extra-font-locking yaml-mode whitespace-cleanup-mode rust-mode rainbow-delimiters paredit markdown-mode flycheck-rust flycheck-pos-tip flycheck-clojure color-theme-sanityinc-tomorrow auto-complete ack-and-a-half))))

;;; Commentary:

(provide 'init)
;;; init.el ends here
