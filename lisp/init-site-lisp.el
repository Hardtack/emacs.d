;;; init-site-lisp --- Loads packages from site-lisp
;;; Commentary:
;;; Loads packages from site-lisp
;;; Code:

;; Set load path
(defun add-subdirs-to-load-path (parent-dir)
  "Add every subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (let ((load-path  (copy-sequence load-path))) ;; Shadow
             (normal-top-level-add-subdirs-to-load-path))
           load-path))))

(add-subdirs-to-load-path (expand-file-name "site-lisp/" user-emacs-directory))

(provide 'init-site-lisp)
;;; init-site-lisp ends here
