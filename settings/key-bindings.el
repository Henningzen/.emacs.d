;;; package --- key-bindings.el
;;;
;;; Commentary:
;;;   Henning Jansen 2025.
;;;   My keybindings.
;;;
;;; Code:

;; C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)
(global-set-key (kbd "C-<tab>") 'completion-at-point)

(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Find file in project
(global-set-key (kbd "C-x o") 'find-file-in-project)

;; Open recent files
(global-set-key (kbd "C-x f") 'recentf-open)

;; Killing text
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

;; Use M-w for copy-line if no active region
(global-set-key (kbd "M-w") 'save-region-or-current-line)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "s-b") 'quick-switch-buffer)

;; Bury current buffer
(global-set-key (kbd "s-y") 'bury-buffer)

;; Window switching
(windmove-default-keybindings) ;; Shift + direction/arrow key

;; Windows rotation and swapping
(global-set-key (kbd "C-x -") 'toggle-window-split)
(global-set-key (kbd "C-x C--") 'rotate-windows)
(global-unset-key (kbd "C-x C-+")) ;; Unset default emacs zoom

;; Magit
(global-set-key (kbd "C-x m") 'magit)
(autoload 'magit "magit")

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")

;; Easy-mode fullscreen rgrep
;; (global-set-key (kbd "M-s s") 'git-grep-fullscreen)  ;; TODO
;; (global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Multi-occur
;; (global-set-key (kbd "M-s m") 'multi-occur)
;; (global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers) ; TODO

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; Find files by name and display results in dired
(global-set-key (kbd "M-s f") 'find-name-dired)

;; Duplicate current line
(global-set-key (kbd "C-c d") 'duplicate-line)

(provide 'key-bindings)
;;; key-bindings.el ends here
