;;; package --- setup-package.el
;;;
;;; Commentary:
;;;   Henning Jansen 2025.
;;;
;;; Code:

(require 'package)
(require 'dash)

(setq package-pinned-packages '())

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")      ; always good
        ("melpa"        . "https://melpa.org/packages/")         ; rolling / dev
        ("melpa-stable" . "https://stable.melpa.org/packages/"))) ; conservative default

;; Everything keeps coming from melpa-stable, EXCEPT gptel:
(setq package-pinned-packages
      '((gptel . "melpa")))

(package-initialize)

(unless (file-exists-p "/home/jansenh/.emacs.d/elpa/archives/melpa")
  (package-refresh-contents))

(defun packages-install (packages)
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

;;; On-demand installation of packages

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(provide 'setup-package)
