My Drive

;;; package -- Summary:

;; Custom Emacs environment for John Maruska. Based initially off of Chris Gore's
;; Emacs configuration. Stripped out minimal useful configuration and built own
;; on top of this.
;;
;; Things left to do:
;;   flycheck - want to work with `eastwood` and `yagni`.
;;     Currently it exists but gives a weird response "error in process filter:
;;       Wrong number of arguments: (4 . 4), 0
;;
;; Language Handling Needed:
;;   Ruby
;;   Python
;;   JavaScript / React
;;   (?) Scala
;;
;; Things for much later:
;;   autocomplete  https://github.com/auto-complete/auto-complete/blob/master/doc/manual.md
;;   magit
;;   projectile
;;   multiple-cursors
;;   rest-client
;;
;; Look into:
;;   helm-cider

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
;; TODO: Find a better way to do this
(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/jdk1.8.0_162.jdk/Contents/Home")

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

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Rainbow delimiters (colored parens)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; shell script mode
(defun my-shell-script-mode-hook ()
  "Handles all configuration for Clojure mode."
  (sh-basic-offset 2))
(add-hook 'shell-script-mode-hook #'my-shell-script-mode-hook)

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

;;; Paredit
(mapc #'(lambda (mode-hook)
          (add-hook mode-hook 'paredit-mode))
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        eval-expression-minibuffer-setup-hook
        json-mode-hook
        lisp-mode-hook
        lisp-interaction-mode-hook))

;; Miscellaneous settings
(setq-default indent-tabs-mode nil)  ; don't mix tabs and spaces
(tool-bar-mode -1)  ; hide tool bar
(scroll-bar-mode -1)  ; hide scroll bar
(menu-bar-mode -1)  ; Hide menu-bar. Doesn't affect Mac OS
(display-time-mode 1)  ; show clock in status bar
(column-number-mode 1)  ; show column numbers in addition to line numbers
(blink-cursor-mode 0)  ; cursor shouldn't blink
(global-linum-mode 1)
(show-paren-mode 1)  ; highlight matching paren
(setq inhibit-startup-screen t)  ; Don't display welcome screen
(setq default-directory "~/")
(setq scroll-step 1)  ; keyboard scroll one line at a time
(setq require-final-newline t)  ; files must end with a newline
(define-key key-translation-map (kbd "C-p") (kbd "M-x"))

;;; Background Appearance
(defun dark-background ()
  "Change the appearance of Emacs to use a dark background."
  (interactive)
  (load-theme 'sanityinc-tomorrow-eighties t)
  ;; (reset-term-colors)
  )

(defun light-background ()
  "Change the appearance of Emacs to use a light background."
  (interactive)
  ;; Look up a theme with a more beige background
  (load-theme 'sanityinc-tomorrow-day t)
  ;; (reset-term-colors)
  )

(defun default-font-and-theme ()
  "Change the appearance and font of Emacs to the default setting."
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil
                           :over-line nil :underline nil :slant normal :weight normal
                           :height 135 :width normal :family "Fira Code")))))
  (dark-background))

(defun google-hangouts-sucks ()
  "Greatly increase the size of the font and change to a light scheme to present on Google Hangouts."
  (interactive)
  (custom-set-faces
   '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil
                           :underline nil :slant normal :weight normal
                           :height 220 :width normal :family "Fira Code")))))
  (light-background))

(default-font-and-theme)

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

;;; Org
;; if you don't use font-lock globally
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

;;; Rust
(require 'rust-mode)
(add-hook 'rust-mode-hook 'flycheck-rust-setup)

;;; YAML
(require 'yaml-mode)
(add-hook 'yaml-mode-hook 'linum-mode)


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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :over-line nil :underline nil :slant normal :weight normal :height 135 :width normal :family "Fira Code")))))

;;; Commentary:

(provide '.init)
;;; init.el ends here
