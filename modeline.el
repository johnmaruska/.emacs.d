(defun custom-modeline-modified ()
  (let* ((config-alist
          '(("-"
             all-the-icons-faicon-family
             all-the-icons-faicon
             "link"
             :height 1.2
             :v-adjust -0.0)
            ("*"
             all-the-icons-faicon-family
             all-the-icons-faicon
             "chain-broken"
             :height 1.2
             :v-adjust -0.0)
            ("%"
             all-the-icons-octicon-family
             all-the-icons-octicon
             "lock"
             :height 1.2
             :v-adjust 0.1)))
         (result (cdr (assoc (format-mode-line "%*") config-alist))))
    (apply (cadr result) (cddr result))
    (propertize (apply (cadr result) (cddr result))
                'face `(:family ,(funcall (car result))))))

(defun custom-modeline-mode-icon ()
  (format " %s"
          (propertize icon
                      'help-echo (format "Major-mode: `%s`" major-mode)
                      'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))

(defun custom-modeline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                 'display '(raise -0.1))
     (propertize (format "(%s)" branch)))))

(setq mode-line-format '(" "
                         mode-line-frame-identification
                         (:eval (custom-modeline-modified)) " "
                         mode-line-buffer-identification "%e(%l,%c) "
                         (:eval (custom-modeline-github-vc))
                         " %P %I "
                         global-mode-string))
