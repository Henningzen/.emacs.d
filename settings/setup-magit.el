;;; package --- magit.el
;;;
;;; Commentary:
;;;   Henning Jansen 2025.

;;;
;;; Code:


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
