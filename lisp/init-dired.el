;;; init-dired --- Settings for dired
;;; Commentary:
;;; Provides settings for dired, such as dired+
;;; Code:
(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%")))

  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(provide 'init-dired)
;;; init-dired ends here
