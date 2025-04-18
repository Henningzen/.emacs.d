;;; package --- apperance.el
;;;
;;; Commentary:
;;;   Henning Jansen 2025.
;;;   My keybindings, mostly copied from Christian Johansen and Magnar Sveen.
;;;
;;; Code:

(setq font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Don't beep. Don't visible-bell (fails on el capitan). Just blink the modeline on errors.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.05 nil 'invert-face 'mode-line)))

;; Highlight current line
(global-hl-line-mode -1)

(setq css-fontify-colors nil)

;; Set custom theme path
(setq custom-theme-directory (concat user-emacs-directory "themes"))

(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; Default theme                                                    ;; TODO make jansenh/**-font
(defun use-presentation-theme ()
  (interactive)
  (when (boundp 'jansenh/presentation-font)
    (set-face-attribute 'default nil :font jansenh/presentation-font)))

(defun use-default-theme ()
  (interactive)
  (load-theme 'deeper-blue)
  (when (boundp 'jansenh/default-font)
    (set-face-attribute 'default nil :font jansenh/default-font)))

(defun toggle-presentation-mode ()
  (interactive)
  (if (string= (frame-parameter nil 'font) jansenh/default-font)
      (use-presentation-theme)
    (use-default-theme)))

(global-set-key (kbd "C-<f9>") 'toggle-presentation-mode)

(use-default-theme)

;; Font for in Emacs 29/Ubuntu
(set-frame-font "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 120)

;;(set-frame-font "Source Code Pro Medium")                    ;; TODO: install font

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; org-mode colors
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("IMPEDED" . (:foreground "red" :weight bold))
        ))

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Make zooming affect frame instead of buffers
;;(require 'zoom-frm)                                               ;; TODO! Has been installed in site-lisp...

;; Sweet window-splits
(defadvice split-window-right (after balance activate) (balance-windows))
(defadvice delete-window (after balance activate) (balance-windows))
(defadvice split-window-below (after balance activate) (balance-windows))

(defun enable-zoom-one-shot-keybindings ()
  (set-transient-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "+") 'zoom-frm-in)
     (define-key map (kbd "-") 'zoom-frm-out)
     (define-key map (kbd "0") 'zoom-frm-unzoom)
     map) t))

(defun zoom-frame-in ()
  (interactive)
  (zoom-frm-in)
  (enable-zoom-one-shot-keybindings))

(defun zoom-frame-out ()
  (interactive)
  (zoom-frm-out)
  (enable-zoom-one-shot-keybindings))

(global-set-key (kbd "C-x +") 'zoom-frame-in)
(global-set-key (kbd "C-x -") 'zoom-frame-out)
(global-set-key (kbd "C-x C-0") 'zoom-frm-unzoom)

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

(provide 'appearance)
