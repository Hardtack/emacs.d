(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)
(require-package 'virtualenvwrapper)

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells)
(venv-initialize-eshell)

;; Project specific virtualenv
(add-hook 'python-mode-hook (lambda ()
                              (hack-local-variables)
                              (when (boundp 'project-venv-name)
                                (venv-workon project-venv-name))))

;; Displaying the currently active virtualenv on the mode line
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))


(provide 'init-python-mode)
