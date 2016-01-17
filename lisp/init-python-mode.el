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

;; RST for docstring
(require 'python)

(defun rst-python-docstrings-find-front (bound)
  (when (not bound)
    (setq bound (point-max))
    )
  (message "Start %s" (point))
  (loop
   while (< (point) bound)
   do (progn
        (message "Search at %s" (point))
        (when (re-search-forward "\\(\"\"\"\\|\'\'\'\\)" bound 'limit)
          (let* ((start (match-beginning 0)))
            (save-excursion
              (goto-char start)
              (save-match-data
                (python-nav-beginning-of-statement)
                )
              (when (and (= (point) start))
                (return (match-end 0)))))))))

(defun rst-python-docstrings-find-back (bound)
  (when (not bound)
    (setq bound (point-max)))
  (loop
   while (< (point) bound)
   do (when (re-search-forward "\\(\"\"\"\\|\'\'\'\\)" bound t)
        (let* ((delim (match-string 0)))
          (save-excursion
            (save-match-data (python-nav-beginning-of-statement))
            (when (looking-at-p delim)
              (return (match-end 0))))))))

(require 'mmm-mode)

(add-to-list 'mmm-save-local-variables 'adaptive-fill-regexp)
(add-to-list 'mmm-save-local-variables 'fill-paragraph-function)

(mmm-add-classes
 '((rst-python-docstrings
    :submode rst-mode
    :face mmm-comment-submode-face
    :front rst-python-docstrings-find-front
    :back rst-python-docstrings-find-back
    :save-matches 1
    :insert ((?d embdocstring nil @ "\"\"\"" @ _ @ "\"\"\"" @))
    :delimiter-mode nil)))
;;;(assq-delete-all 'rst-python-docstrings mmm-classes-alist)

(mmm-add-mode-ext-class 'python-mode nil 'rst-python-docstrings)


(provide 'init-python-mode)
