;;; package --- init.el
;;;
;;; Commentary:
;;;   Henning Jansen 2025.
;;;   Developed on GNU Emacs 29.4 (build 2, x86_64-pc-linux-gnu,
;;;   GTK+ Version 3.24.41, cairo version 1.18.0) of 2024-12-29
;;;
;;;   Emacs init.el mostly copied from Christian Johansen and Magnar Sveen.
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
     highlight-escape-sequences
     html-to-hiccup
     htmlize
     hydra
     ido-at-point
     ido-completing-read+
     ido-vertical-mode
     inflections
     jet
;;   js2-mode
;;   js2-refactor
     kaocha-runner
     less-css-mode
     lorem-ipsum
     magit
     marginalia
     markdown-mode
     minions
     move-text
;;   nodejs-repl
     orderless
     paredit
     perspective
     prodigy
     projectile
     queue
     request
     restclient
     ripgrep
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

;; Setup extensions
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-rgrep)
(require 'setup-hippie)
(require 'setup-yasnippet)
(require 'setup-perspective)
(require 'setup-ffip)
(require 'setup-html-mode)
(require 'setup-paredit)
(require 'setup-editorconfig)
(require 'setup-css-mode)
(require 'jet-custom)

(global-set-key (kbd "C-c j e j") 'copy-edn-as-json)
(global-set-key (kbd "C-c j j e") 'copy-json-as-edn)

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'prodigy)
(global-set-key (kbd "C-x M-m") 'prodigy)

;; Font lock dash.el
(eval-after-load "dash" '(dash-enable-font-lock))

;; Default setup of smartparens
(require 'smartparens-config)
(setq sp-autoescape-string-quote nil)
(--each '(css-mode-hook
          restclient-mode-hook
          java-mode
          markdown-mode)
  (add-hook it 'turn-on-smartparens-mode))

;; Language specific setup files
(eval-after-load 'clojure-mode '(require 'setup-clojure-mode))
(eval-after-load 'markdown-mode '(require 'setup-markdown-mode))
(eval-after-load 'elm-mode '(require 'setup-elm-mode))

;; Load stuff on demand
(autoload 'skewer-start "setup-skewer" nil t)
(autoload 'skewer-demo "setup-skewer" nil t)
(autoload 'auto-complete-mode "auto-complete" nil t)

;; Map files to modes
(require 'mode-mappings)


;; Calendar stuff
(require 'setup-calendar)

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

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Diminish modeline clutter
(require 'diminish)
(diminish 'yas-minor-mode)

;; ;; Conclude init by setting up specifics for the current user
;; (when (file-exists-p user-settings-dir)
;;   (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))

;; ;; Slides
;; (require 'slides)
;; (put 'narrow-to-page 'disabled nil)
(provide 'init)
;;; init.el ends here
