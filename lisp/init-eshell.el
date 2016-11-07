;;; init-eshell --- Eshell customizations
;;; Commentary:
;;; Provides various eshell utilities and configurations.
;;;
;;; Code:

;; Aliases
(setq eshell-aliases-file (expand-file-name "eshell.aliases" user-emacs-directory))

;; Customize functions by advicing
(defun find-file-in-next-frame (filename &optional wildcards)
  "Run 'find-file with FILENAME and WILDCARDS in next frame."
  (call-interactively 'other-frame)
  (if (listp filename)
      (loop for f in filename do (find-file f wildcards))
    (apply 'find-file filename wildcards)))

(provide 'init-eshell)
;;; init-eshell ends here
