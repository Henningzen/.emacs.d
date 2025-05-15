;;; package --- magit.el
;;;
;;; Commentary:
;;;   Henning Jansen 2025.

;;;
;;; Code:

;; full screen magit-status
(defun magit-quit (&optional kill-buffer)
  "Like magit-mode-bury-buffer, but also restores the window
configuration stored by magit-status-fullscreen"
  (interactive "P")
  ;; Kill all associated Magit buffers when a double prefix arg is given.
  (when (>= (prefix-numeric-value kill-buffer) 16)
    (let ((current (current-buffer)))
      (dolist (buf (magit-mode-get-buffers))
        (unless (eq buf current)
          (kill-buffer buf)))))
  (funcall magit-bury-buffer-function kill-buffer)
  (jump-to-register :magit-fullscreen))

(defun magit-status-fullscreen (prefix)
  (interactive "P")
  (window-configuration-to-register :magit-fullscreen)
  (magit-status)
  (unless prefix
    (delete-other-windows)))

; move cursor into position when entering commit message
(defun my/magit-cursor-fix ()
  (beginning-of-buffer)
  (when (looking-at "#")
    (forward-line 1)))

(add-hook 'git-commit-mode-hook 'my/magit-cursor-fix)

;; expand sections by default
(setq magit-section-initial-visibility-alist
      '((untracked . show)
        (unstaged . show)
        (unpushed . show)
        (unpulled . show)
        (stashes . show)))

(define-key magit-status-mode-map (kbd "q") 'magit-quit)

(set-default 'magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

(set-default 'magit-diff-refine-hunk t)

;; update diff-hl
(global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; use forge
(with-eval-after-load 'magit
(require 'forge))

(provide 'setup-magit)
;;; setup-magit.el ends here
