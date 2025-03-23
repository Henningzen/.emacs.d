;;; package --- key-bindings.el
;;;
;;; Commentary:
;;;   Henning Jansen 2025.
;;;   My keybindings, mostly copied from Christian Johansen and Magnar Sveen.
;;;
;;; Code:

;; C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)
(global-set-key (kbd "C-,") 'completion-at-point)

(global-set-key (kbd "C-<f10>") 'menu-bar-mode)

;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Find file in project
(global-set-key (kbd "C-x o") 'find-file-in-project)

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
(windmove-default-keybindings) ;; Shift + direction
(global-set-key (kbd "C-x -") 'toggle-window-split)  ;; TODO
(global-set-key (kbd "C-x C--") 'rotate-windows)     ;; TODO
(global-unset-key (kbd "C-x C-+")) ;; don't zoom like this

(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit) ;; TODO

;; Move windows, even in org-mode
;; (global-set-key (kbd "<s-right>") 'windmove-right)
;; (global-set-key (kbd "<s-left>") 'windmove-left)
;; (global-set-key (kbd "<s-up>") 'windmove-up)
;; (global-set-key (kbd "<s-down>") 'windmove-down)

;; Add region to *multifile*
(global-set-key (kbd "C-!") 'mf/mirror-region-in-multifile)

;; Help should search more than just commands
(global-set-key (kbd "<f1> a") 'apropos)

;; Navigation bindings
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "<prior>") 'beginning-of-buffer)
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<next>") 'end-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "M-<up>") 'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "H-n") 'goto-next-line-with-same-indentation)
(global-set-key (kbd "H-p") 'goto-prev-line-with-same-indentation)

;; Tags
(global-set-key (kbd "M-?") 'tags-search)
(global-set-key (kbd "M-B") 'goto-last-modification)

;; Completion at point
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Search with deadgrep/ripgrep
(require 'deadgrep)
(global-set-key (kbd "<f6>") #'deadgrep)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)

;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Eval buffer
(global-set-key (kbd "C-c C-k") 'eval-buffer)

(global-set-key (kbd "C-x m") 'magit-status-fullscreen)
(autoload 'magit-status-fullscreen "magit")

;; Line movement
(global-set-key (kbd "<C-S-down>") 'move-text-down)
(global-set-key (kbd "<C-S-up>") 'move-text-up)

;; Toggle quotes
(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; Find files by name and display results in dired
(global-set-key (kbd "M-s f") 'find-name-dired)

;; Duplicate current line
(global-set-key (kbd "C-c d") 'duplicate-line)

(provide 'key-bindings)
;;; key-bindings.el ends here

