;;; init-python-mode --- Customizations for python-mode
;;; Commentary:
;;; Code:
(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
		("SConscript\\'" . python-mode))
              auto-mode-alist))

;;; 79 columns
(require 'fill-column-indicator)

(defun python-fci-hook ()
  (setq-local fci-rule-column 79)
  (fci-mode))
(add-hook 'python-mode-hook 'python-fci-hook)

;;; Virtualenv setup
(require 'virtualenvwrapper)
(after-load 'eshell
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

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
  "Get current path for EXEC based on venv."
  (interactive)
  (let ((path (if venv-current-dir
                  (concat (file-name-as-directory venv-current-dir)
                          (file-name-as-directory "bin")
                          exec)
                (executable-find exec))))
    (if (and path (file-exists-p path))
        path
      nil)))

;; Install useful packages
(defcustom useful-python-packages '("flake8" "pylint" "jedi" "epc") "Useful python packages.")

(defun venv-install-packages (first &rest rest)
  "Install packages FIRST and REST to venv."
  (when (not (venv-executable-find "pip"))
    (error "Cannot find pip"))
  (let ((command (append (list (venv-executable-find "pip")
                               "install")
                         (cons first rest))))
    (apply 'start-process
           (append
            '("pip" "*pip installation*")
            command)))
  (display-buffer "*pip installation*"))

(defun venv-install-requirements (path)
  "Install package in PATH of requirements.txt."
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

(require 'flycheck)

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
(setq jedi:setup-keys t)
(setq jedi:complete-on-dot t)

(add-hook 'python-mode-hook 'jedi:setup)

;; Additional keys
(defun geonu/add-python-keys ()
  "Add custom keys for python-mode."
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker))
(add-hook 'python-mode-hook 'geonu/add-python-keys)

;; Done
(require 'jedi)

;;; RST for docstring
(require 'python-docstring)
(python-docstring-install)

;;; Disable prettify-symbol-mode
(add-hook 'python-mode-hook (lambda () (prettify-symbols-mode 0)))

(provide 'init-python-mode)
;;; init-python-mode ends here
