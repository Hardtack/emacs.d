(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

(require-package 'pip-requirements)


;;; 79 columns
(require-package 'fill-column-indicator)
(require 'fill-column-indicator)

(defun python-fci-hook ()
  (setq-local fci-rule-column 79)
  (fci-mode))
(add-hook 'python-mode-hook 'python-fci-hook)

;;; Virtualenv setup
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
(setq-default mode-line-format
              (cons
               '(:eval
                 (if venv-current-name
                     (format "venv:%s" venv-current-name)
                   ""))
               mode-line-format))

;; Utility
(defun venv-executable-find (exec)
  "Get current path for exec based on venv."
  (interactive)
  (let ((path (if venv-current-dir
                  (concat (file-name-as-directory venv-current-dir)
                          (file-name-as-directory "bin")
                          exec)
                (executable-find exec))))
    (if (and path (file-exists-p path))
        path
      (executable-find exec))))

;; Install useful packages
(defcustom useful-python-packages '("flake8" "pylint" "jedi" "epc") "Useful python packages")

(defun venv-install-packages (first &rest rest)
  "Install package to venv."
  (when (not (venv-executable-find "pip"))
    (error "pip not found."))
  (let ((command (append (list (venv-executable-find "pip")
                               "install")
                         (cons first rest))))
    (apply 'start-process
           (append
            '("pip" "*pip installation*")
            command)))
  (display-buffer "*pip installation*"))

(defun venv-install-requirements (path)
  "Install package in requirements.txt"
  (interactive (list (read-file-name "Path for requirement.txt: ")))
  (venv-install-packages "-r" path))

(defun venv-install-useful-packages ()
  "Install useful pacakges to venv."
  (interactive)
  (apply 'venv-install-packages useful-python-packages))

;; flycheck
(defun get-current-flake8 ()
  "Get current path for flake8 based on venv."
  (interactive)
  (venv-executable-find "flake8"))
(defun get-current-pylint ()
  "Get current path for pylint based on venv."
  (interactive)
  (venv-executable-find "pylint"))


(defun set-flychecker-executables ()
  "Configure virtualenv for flake8 and lint."
  (if (get-current-flake8)
      (progn (setq flycheck-disabled-checkers
                   (remove 'python-flake8 flycheck-disabled-checkers))
             (flycheck-set-checker-executable 'python-flake8
                                              (get-current-flake8)))
    (flycheck-disable-checker 'python-flake8))
  (if (get-current-pylint)
      (progn (setq flycheck-disabled-checkers
                   (remove 'python-pylint flycheck-disabled-checkers))
             (flycheck-set-checker-executable 'python-pylint
                                              (get-current-pylint)))

    (flycheck-disable-checker 'python-pylint)))
(add-hook 'flycheck-before-syntax-check-hook
          #'set-flychecker-executables 'local)

;;; Jedi
(require-package 'jedi)
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook 'jedi:setup)

;; Additional keys
(setq jedi:key-goto-definition (kbd "M-."))
(setq jedi:key-goto-definition-pop-marker (kbd "M-,"))
(require 'jedi)

;;; RST for docstring
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
