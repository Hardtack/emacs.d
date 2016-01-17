(require-package 'project-explorer)
(require 'project-explorer)

;; Key C-x p
(global-set-key (kbd "C-x p") 'project-explorer-toggle)

(setq pe/omit-enabled nil)

(provide 'init-project)
