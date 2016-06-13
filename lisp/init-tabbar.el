(require-package 'tabbar)
(require 'tabbar)

(tabbar-mode)
(global-set-key [C-left] 'tabbar-backward-tab)
(global-set-key [C-right] 'tabbar-forward-tab)
(global-set-key (kbd "C-x <left>") 'tabbar-backward-tab)
(global-set-key (kbd "C-x <right>") 'tabbar-forward-tab)

(require-package 'tabbar-ruler)
(require 'tabbar-ruler)
(setq tabbar-ruler-global-tabbar t)
(setq tabbar-ruler-popup-menu t)
(setq tabbar-ruler-popup-toolbar t)
(setq tabbar-ruler-popup-scrollbar t)
(tabbar-ruler-group-buffer-groups)
(tabbar-ruler-group-by-projectile-project)

;; Tab navigation
(require-package 'dash)
(require 'dash)

(defun geonu/select-tab-at (index &optional tabset)
  "Select tab of TABSET at INDEX."
  (interactive "NIndex: ")
  (interactive "P")
  (let* ((tabset (if tabset tabset (tabbar-current-tabset)))
         (tabs (tabbar-tabs tabset))
         (tab (nth index tabs)))
    (if tab (progn
              (tabbar-select-tab tab tabset)
              (when tabbar-select-tab-function
                (funcall tabbar-select-tab-function (tabbar-make-mouse-event nil) tab)))
      (error "Invalid tab index: %d" index))))

(defun geonu/select-last-tab (&optional tabset)
  "Select last tab of TABSET."
  (interactive "P")
  (let* ((tabset (if tabset tabset (tabbar-current-tabset)))
         (tabs (tabbar-tabs tabset))
         (tab (-> tabs last car)))
    (tabbar-select-tab tab tabset)
    (when tabbar-select-tab-function
      (funcall tabbar-select-tab-function (tabbar-make-mouse-event nil) tab))))

(if *is-a-mac*
    (progn
      (global-set-key (kbd "s-1") (lambda () (interactive) (geonu/select-tab-at 0)))
      (global-set-key (kbd "s-2") (lambda () (interactive) (geonu/select-tab-at 1)))
      (global-set-key (kbd "s-3") (lambda () (interactive) (geonu/select-tab-at 2)))
      (global-set-key (kbd "s-4") (lambda () (interactive) (geonu/select-tab-at 3)))
      (global-set-key (kbd "s-5") (lambda () (interactive) (geonu/select-tab-at 4)))
      (global-set-key (kbd "s-6") (lambda () (interactive) (geonu/select-tab-at 5)))
      (global-set-key (kbd "s-7") (lambda () (interactive) (geonu/select-tab-at 6)))
      (global-set-key (kbd "s-8") (lambda () (interactive) (geonu/select-tab-at 7)))
      (global-set-key (kbd "s-9") 'geonu/select-last-tab))
  (progn
    (global-set-key (kbd "C-1") (lambda () (interactive) (geonu/select-tab-at 0)))
    (global-set-key (kbd "C-2") (lambda () (interactive) (geonu/select-tab-at 1)))
    (global-set-key (kbd "C-3") (lambda () (interactive) (geonu/select-tab-at 2)))
    (global-set-key (kbd "C-4") (lambda () (interactive) (geonu/select-tab-at 3)))
    (global-set-key (kbd "C-5") (lambda () (interactive) (geonu/select-tab-at 4)))
    (global-set-key (kbd "C-6") (lambda () (interactive) (geonu/select-tab-at 5)))
    (global-set-key (kbd "C-7") (lambda () (interactive) (geonu/select-tab-at 6)))
    (global-set-key (kbd "C-8") (lambda () (interactive) (geonu/select-tab-at 7)))
    (global-set-key (kbd "C-9") 'geonu/select-last-tab)))

(provide 'init-tabbar)
