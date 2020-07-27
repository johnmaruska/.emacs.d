;;; package -- Summary:

;; Miscellaneous interactive functions that don't fit into other categories.

;;; Code:

(defun json-format ()
  "Format JSON block to adhere to readable format."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark)
                             (point)
                             "python -m json.tool"
                             (buffer-name)
                             t)))

(defun json-parse ()
  (interactive)
  (json-read-from-string (buffer-substring (mark) (point))))

(defun xml-format ()
  "Format XML block to a readable format."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark)
                             (point)
                             "xmllint --format --encode utf-8 -"
                             (buffer-name)
                             t)))

;;; Commentary:

(provide 'utils)
;;; utils.el ends here
