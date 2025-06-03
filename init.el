;;; package -- Summary:

;; Custom Emacs environment for John Maruska. Based initially off of Chris Gore's
;; Emacs configuration. Stripped out minimal useful configuration and built own
;; on top of this.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;                    Per-Machine Variable Setup
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;
;;
;;    Variables
;;
;;;;;;;;;;;;;;;;

(defvar MY--operating-system
  (cond ((eq system-type 'darwin)
         "MacOS")
        ((string= (system-name) "pop-os")
         "Ubuntu")
        ((eq system-type 'windows-nt)
         "Windows")))

(defvar MY--current-machine
  (cond ((eq system-type 'darwin)
         "macbook")
        ((string= (system-name) "pop-os")
         "convertible")
        ((string= (system-name) "VINGTOR")
         "vingtor")))

;;;;;;;;;;;;;;;;
;;
;;    MacBook Hotkeys
;;
;;;;;;;;;;;;;;;;

(when (string= "macbook" MY--current-machine)
  ;; Allow Cmd-Q to close the program like anything else.
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)

  (setq select-enable-clipboard t)
  ;; paste-from-system
  (global-set-key (kbd "s-v") 'clipboard-yank)
  ;; copy-to-system
  (global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
  ;; cut-to-system
  (global-set-key (kbd "s-x") 'clipboard-kill-region)

  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "<home>") 'beginning-of-buffer)
  (global-set-key (kbd "<end>") 'end-of-buffer)

  (setq
   ;; Change modifier keys to fit muscle memory from previous installs.
   mac-command-modifier 'super
   mac-control-modifier 'control
   mac-option-modifier  'meta
   mac-right-option-modifier 'control)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;                    Initialize Packages Installation
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("ELPA" . "http://tromey.com/elpa/")))

(setq create-lockfiles nil)
(setq package-check-signature nil)


(package-initialize)
;; (setq url-http-attempt-keepalives nil)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)



(require 'env-vars "~/.emacs.d/env-vars.el")
(set-default-envvars)

(require 'global "~/.emacs.d/global.el")
(default-ui-configuration)

(require 'appearance "~/.emacs.d/appearance.el")
(default-appearance)

(require 'mode-hooks "~/.emacs.d/mode-hooks.el")
(require 'mode-line "~/.emacs.d/mode-line.el")

(require 'eshell)
(declare-function eshell/pwd "ext:eshell/pwd")
(defvar eshell-prompt-function
  (lambda ()
    (concat
     (propertize (format-time-string "%-I:%M:%S%p " (current-time))
                 'face `(:foreground "#aaaaff"))
     (propertize (abbreviate-file-name (eshell/pwd))
                 'face `(:foreground "#aaaa44"))
     (if (= (user-uid) 0) " # " " $ "))))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(require 'json)
(require 'utils "~/.emacs.d/utils.el")

(when (file-exists-p "~/.emacs.d/secrets/tokens.el")
  (load "~/.emacs.d/secrets/tokens.el"))



;;; Commentary:

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-check-syntax-automatically
   '(save idle-change idle-buffer-switch new-line mode-enabled))
 '(js-chain-indent nil)
 '(package-check-signature nil)
 '(package-selected-packages
   '(ag aggressive-indent alchemist auto-dim-other-buffers beacon
        clj-refactor color-theme-sanityinc-solarized
        color-theme-sanityinc-tomorrow csv-mode dashboard delight
        direnv dockerfile-mode elpy exercism flymd geiser
        graphviz-dot-mode guru-mode hl-todo json-mode jtsx kotlin-mode
        lua-mode major-mode-icons markdown-mode nvm page-break-lines
        php-mode rainbow-delimiters restclient sqlformat
        terraform-mode tide tree-sitter-langs treemacs-all-the-icons
        treemacs-icons-dired treemacs-magit treemacs-projectile
        typescript-mode uuidgen web-mode whitespace-cleanup-mode
        yafolding yaml-mode yasnippet-snippets))
 '(safe-local-variable-values
   '((eval progn
           (add-to-list 'exec-path
                        (concat
                         (locate-dominating-file default-directory
                                                 ".dir-locals.el")
                         "node_modules/.bin/"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
