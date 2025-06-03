;;; package --- init.el
;;;
;;; Commentary:
;;;   Henning Jansen 2025.
;;;   Developed on GNU Emacs 30.1 (GNU Emacs 30.1 (build 2,
;;;   x86_64-pc-linux-gnu, GTK+ Version 3.24.38, cairo version 1.16.0)
;;;   of 2025-04-16
;;;
;;;   Emacs init.el is and subseqent *.el files are based on Christian
;;;   Johansen and Magnar Sveen emacs.d settings.
;;;
;;; Code:

;; Menubar, toolbar and scrollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))


;; Remove security vulnerability
(eval-after-load "enriched"
  '(defun enriched-decode-display-prop (start end &optional param)
     (list start end)))

;; Optional splashscreen
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set patch to settings
(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Write all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; We don't need lock-files, no one else is here
(setq create-lockfiles nil)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Technomancy's Better Defaults
;;https://git.sr.ht/~technomancy/better-defaults

(add-to-list 'load-path "/home/jansenh/.emacs.d/better-defaults")
(require 'better-defaults)

;; Set up appearance early
(require 'appearance)

;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     beginend
     browse-kill-ring
     change-inner
     cider
     clj-refactor
     clojure-mode
     clojure-mode-extra-font-locking
     company
     consult
     css-eldoc
     dash
     deadgrep
     diff-hl
     diminish
     dockerfile-mode
     editorconfig
     edn
     elisp-slime-nav
     eproject
     ;; exec-path-from-shell
     expand-region
     f
     fill-column-indicator
     find-file-in-project
     flx
     flx-ido
     flycheck
     flycheck-clj-kondo
     flycheck-joker
     flycheck-pos-tip
     forge
     gptel
     gist
     highlight-escape-sequences
     html-to-hiccup
     htmlize
     hydra
     ido-at-point
     ido-completing-read+
     ido-vertical-mode
     inflections
     ;; jet
;;   js2-mode
     ;;   js2-refactor
     jump-char
     ;; kaocha-runner
     ;; less-css-mode
     ;; lorem-ipsum
     magit
     marginalia
     markdown-mode
     ;; minions
     move-text
     multifiles
     nov
;;   nodejs-repl
     orderless
     paredit
     perspective
     prodigy
     projectile
     queue
     ;; request
     ;; restclient
     ripgrep
     ;; simple-httpd
     smartparens
     smart-forward
     smex
     smooth-scrolling
     spinner
     sqlite3
     string-edit
     string-edit-at-point
     ;; systemd
     ;; terraform-mode
     ;; textile-mode
     undo-tree
     use-package
     vertico

     visual-regexp
     wgrep
     whitespace-cleanup-mode
     yasnippet
     zprint-mode
     )))

(require 'use-package)

(use-package vertico
  :init
  (vertico-mode)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (setq enable-recursive-minibuffers t)

  (require 'vertico-directory)
  (keymap-set vertico-map "RET" #'vertico-directory-enter)
  (keymap-set vertico-map "DEL" #'vertico-directory-delete-char)
  (keymap-set vertico-map "M-DEL" #'vertico-directory-delete-word)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        orderless-matching-styles '(orderless-initialism
                                    orderless-literal
                                    orderless-regexp)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode)
  (keymap-set minibuffer-local-map "M-A" #'marginalia-cycle))

(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))


(require 'sane-defaults)

;; Setup extensions
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'magit '(require 'setup-magit))

(require 'setup-gptel)
(require 'setup-perspective)

;; Map files to modes --------------------------------
(require 'mode-mappings)

;; Highlight escape sequences
(require 'highlight-escape-sequences)
(hes-mode)
(put 'font-lock-regexp-grouping-backslash 'face-alias 'font-lock-builtin-face)

;; Visual regexp
(require 'visual-regexp)
(define-key global-map (kbd "M-&") 'vr/query-replace)
(define-key global-map (kbd "M-/") 'vr/replace)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'expand-region)
(require 'delsel)
(require 'jump-char)
(require 'eproject)
(require 'smart-forward)
(require 'change-inner)
(require 'multifiles)

;; Don't use expand-region fast keys
(setq expand-region-fast-keys-enabled nil)

;; Show expand-region command used
(setq er--show-expansion-message t)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

;; Elisp go-to-definition with M-. and back again with M-,
(autoload 'elisp-slime-nav-mode "elisp-slime-nav")
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t) (eldoc-mode 1)))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Diminish modeline
(require 'diminish)
(diminish 'yas-minor-mode)

;; TODO: Novel mode for ePub, PDF et al
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
