;;; package --- appearance.el
;;;
;;; Commentary:
;;;   Henning Jansen 2025.
;;;
;;; Code:

;; Mute visible-bell, blink the modeline on errors.
(setq visible-bell nil)

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; --- Pre-set theme styling ---------------------------------------------------
;;
(defun use-default-theme ()
  (interactive)
  (load-theme 'deeper-blue)
  (when (boundp 'jansenh/default-font)
    (set-face-attribute 'default nil :font jansenh/default-font))) ;; Default theme

(defun use-presentation-theme ()
  (interactive)
  (when (boundp 'jansenh/presentation-font)
    (set-face-attribute 'default nil :font jansenh/presentation-font)))

(defun toggle-presentation-mode ()
  (interactive)
  (if (string= (frame-parameter nil 'font) jansenh/default-font)
      (use-presentation-theme)
    (use-default-theme)))

(global-set-key (kbd "C-<f9>") 'toggle-presentation-mode)

(use-default-theme)

;; Font for now, Emacs 29++/Ubuntu (used temporarily)
(set-frame-font "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 120) ;; TODO make jansenh/**-font

;; --- Miscellaneous settings --------------------------------------------------
;;
;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Highlight current line -- off
(global-hl-line-mode -1)


;; Don't beep. Don't visible-bell (fails on el capitan). Just blink the modeline on errors.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Unclutter the modeline
(require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "tagedit" '(diminish 'tagedit-mode))
(eval-after-load "elisp-slime-nav" '(diminish 'elisp-slime-nav-mode))
(eval-after-load "skewer-mode" '(diminish 'skewer-mode))
(eval-after-load "skewer-css" '(diminish 'skewer-css-mode))
(eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "whitespace-cleanup-mode" '(diminish 'whitespace-cleanup-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;;(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")
(rename-modeline "python-mode" clojure-mode "Py")
(rename-modeline "org-mode" clojure-mode "Org")
(rename-modeline "lisp-mode" elisp-mode "ELisp")

(provide 'appearance)
;;; appearance.el ends here
