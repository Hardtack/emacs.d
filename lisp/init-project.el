;;; init-project --- Settings for projectile
;;; Commentary:
;;; Code:

(require 'projectile)

(projectile-global-mode t)

;; Speedbar setup
(require 'sr-speedbar)
(require 'projectile-speedbar)

(defun open-speedbar-for-current-buffer ()
  "Open speedbar and show current buffer in tree."
  (interactive)
  (unless (condition-case nil
              (projectile-speedbar-open-current-buffer-in-tree)
            (error nil))
    (sr-speedbar-open)))

;; Redef
(defun projectile-speedbar-toggle ()
  "Redefinition of projectile-speedbar-toggle."
  (interactive)
  (if (sr-speedbar-exist-p)
      (sr-speedbar-close)
    (progn
      (open-speedbar-for-current-buffer)
      (sr-speedbar-select-window))))

(defalias 'my-speedbar-toggle 'projectile-speedbar-toggle)

;; Bottom-up VCS
(setq projectile-project-root-files-bottom-up (append projectile-project-root-files
                                                      projectile-project-root-files-bottom-up))
(setq projectile-project-root-files '())

(provide 'init-project)
;;; init-project ends here
