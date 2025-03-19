;;; ----------------------------------------------------------------------------
;;; Henning Jansen 2025
;;;   Developed on GNU Emacs 29.4 (build 2, x86_64-pc-linux-gnu,
;;;   GTK+ Version 3.24.41, cairo version 1.18.0) of 2024-12-29
;;; ----------------------------------------------------------------------------


;; Technomancy's Better Defaults
;; https://git.sr.ht/~technomancy/better-defaults

(add-to-list 'load-path "./better-defaults")
(require 'better-defaults)


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

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Set up appearance early
(require 'appearance)

;; Settings for currently logged in user
(setq user-settings-dir
      (concat user-emacs-directory "users/" user-login-name))
(add-to-list 'load-path user-settings-dir)

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


;; Setup packages
(require 'setup-package)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(
     ansible
     beginend
     cider
     clj-refactor
     clojure-mode
     clojure-mode-extra-font-locking
     company
     consult
     css-eldoc
     deadgrep
     diff-hl
     diminish
     dockerfile-mode
     editorconfig
     edn
     elisp-slime-nav
     elm-mode
     eproject
     exec-path-from-shell
     f
     fill-column-indicator
     flx
     flx-ido
     flycheck
     flycheck-clj-kondo
     flycheck-joker
     flycheck-pos-tip
     forge
     gist
     go-mode
     groovy-mode
     highlight-escape-sequences
     html-to-hiccup
     htmlize
     hydra
     ido-at-point
     ido-completing-read+
     ido-vertical-mode
     inflections
     jet
     js2-mode
     js2-refactor
     kaocha-runner
     less-css-mode
     lorem-ipsum
     magit
     magit
     marginalia
     markdown-mode
     markdown-mode
     minions
     move-text
     nodejs-repl
     orderless
     paredit
     perspective
     prodigy
     projectile
     queue
     request
     restclient
     ripgrep
     scala-mode
     simple-httpd
     smartparens
     spinner
     sqlite3
     string-edit
     string-edit-at-point
     systemd
     terraform-mode
     textile-mode
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
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
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

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Setup environment variables from the user's shell.
(when is-mac
  (require-package 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

