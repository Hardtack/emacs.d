;;; init-fonts --- Customizations for fonts
;;; Commentary:
;;; Code:

;; Character sets
(require 'dash)


;;; Changing font sizes

(set-face-attribute 'default nil :height 140)

(defcustom universal-font nil
  "Font for universal characters.")
(defcustom roman-font nil
  "Font for roman chracters.")
(defcustom korean-font nil
  "Font for korean characters.")
(defcustom universal-font-candidates '()
  "Font candidates for universal-characters.")
(defcustom roman-font-candidates '()
  "Font candidates for roman chracters.")
(defcustom korean-font-candidates '()
  "Font candidates for korean characters.")

(defun geonu/font-exists-p (font)
  "Check the existence of FONT in this machine."
  (if window-system
      (-> (x-list-fonts font) not not)
    nil))

(defun geonu/select-first-existing-font (candidates)
  "Select first installed font from CANDIDATES."
  (-first #'geonu/font-exists-p candidates))

(defun geonu/update-fonts ()
  "Update fonts from customs."
  (interactive)
  (if universal-font
      (progn (set-face-attribute 'default nil :family universal-font)
             (set-fontset-font t 'hangul (font-spec :name universal-font)))
    (progn
      (when roman-font (set-face-attribute 'default nil :family roman-font))
      (when korean-font (set-fontset-font t 'hangul (font-spec :name korean-font))))))

(defun geonu/update-fonts-from-global-candidates ()
  "Update fonts from universal-font-candidates, roman-font-candidates, korean-font-candidates."
  (interactive)
  (setq universal-font (geonu/select-first-existing-font universal-font-candidates))
  (setq roman-font (geonu/select-first-existing-font roman-font-candidates))
  (setq korean-font (geonu/select-first-existing-font korean-font-candidates))
  (geonu/update-fonts))

(defun use-ugly-fixed-universal-font-off ()
  "Don't use ugly fixed-size universal font."
  (interactive)
  (setq universal-font-candidates '())
  (geonu/update-fonts-from-global-candidates))
(defun use-ugly-fixed-universal-font ()
  "Use ugly fixed-size universal font."
  (interactive)
  (when (not (geonu/select-first-existing-font ugly-universal-font-candidates))
    (error "No fixed-sized universal fonts available."))
  (setq universal-font-candidates ugly-universal-font-candidates)
  (geonu/update-fonts-from-global-candidates))
(setq ugly-universal-font-candidates
      '("D2Coding" "NanumGothicCoding"))
(setq universal-font-candidates
      '())
(setq roman-font-candidates
      '("Menlo" "Monaco" "Consolas"))
(setq korean-font-candidates
      '("D2Coding" "NanumGothicCoding" "Apple SD Gothic Neo"))

(geonu/update-fonts-from-global-candidates)
(when window-system (use-ugly-fixed-universal-font))

;;; Emoji support
(setq emojify-emoji-styles 'unicode)
(require 'emojify)
(add-hook 'after-init-hook #'global-emojify-mode)

(provide 'init-fonts)
;;; init-fonts ends here
