;;; package -- Summary:

;; Custom Emacs environment for John Maruska. Based initially off of Chris Gore's
;; Emacs configuration. Stripped out minimal useful configuration and built own
;; on top of this.

;;; Code:

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

(require 'computers "~/.emacs.d/computers.el")
(when (macbook?)
  (setup-mac-displays))
(when (file-exists-p "~/.emacs.d/secrets/tokens.el")
  (load "~/.emacs.d/secrets/tokens.el"))

(defun chris-zoom ()
  (interactive)
  (browse-url "https://us02web.zoom.us/j/5734523216?pwd=Y2JjN29pYkFwcVZKZUx4RnN6N0VmZz09"))

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
 '(package-selected-packages nil)
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
