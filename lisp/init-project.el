(require-package 'projectile)
(require 'projectile)

(projectile-global-mode t)

;; Speedbar setup
(require-package 'projectile-speedbar)
(require-package 'sr-speedbar)
(require 'sr-speedbar)
(require 'projectile-speedbar)

(defun open-speedbar-for-current-buffer ()
  (interactive)
  (unless (condition-case nil
              (projectile-speedbar-open-current-buffer-in-tree)
            (error nil))
    (sr-speedbar-open)))

;; Redef
(defun projectile-speedbar-toggle ()
  (interactive)
  (if (sr-speedbar-exist-p)
      (sr-speedbar-close)
    (progn
      (open-speedbar-for-current-buffer)
      (sr-speedbar-select-window))))

(defalias 'my-speedbar-toggle 'projectile-speedbar-toggle)

(provide 'init-project)
