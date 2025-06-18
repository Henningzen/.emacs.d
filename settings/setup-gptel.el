;;; package --- setup-gptel
;;;
;;; Commentary:
;;;   Henning Jansen 2025.
;;;
;;;
;;; Code:

;; --- Authinfo secret management
(require 'auth-source)
(require 'epa-file)

;; Enable EasyPG, which is used for handling encryption and decryption.
(epa-file-enable)
(setq auth-sources '("~/.authinfo.gpg"))

(defun my/get-secret (host login)
  "Return the secret (password) for HOST and LOGIN via auth-source."
  (let ((match (car (auth-source-search :host host :user login :max 1))))
    (when match
      (let ((secret (plist-get match :secret)))
        (if (functionp secret) (funcall secret) secret)))))

;; Define your custom system prompt
(defcustom gptel-claude-normal-system-prompt
  "You are a large language model living in Emacs and a helpful assistant pro-coder. Respond concisely. You will write output in formal Org mode, with line-width max 80 characters."
  "System prompt for normal LLM mode."
  :type 'string
  :group 'gptel)

;; Define LLM backends
(setq gptel-claude-normal
      (gptel-make-anthropic "Claude"
                 :stream t 
                 :key (my/get-secret "anthropic" "apikey")
                 :models '(claude-opus-4-20250514)))

(setq gptel-claude-thinking
      (gptel-make-anthropic "Claude-thinking"
        :key (my/get-secret "anthropic" "apikey")
        :stream t
        :models '(claude-opus-4-20250514)
        :header (lambda () 
                  (when-let* ((key (gptel--get-api-key)))
                    `(("x-api-key" . ,key)
                      ("anthropic-version" . "2023-06-01")
                      ("anthropic-beta" . "pdfs-2024-09-25")
                      ("anthropic-beta" . "output-128k-2025-02-19")
                      ("anthropic-beta" . "prompt-caching-2024-07-31"))))
        :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
                          :max_tokens 4096)))

;; ;; Set as default model
;; (setq gptel-model "claude-3-7-sonnet-20250219")

;; ;; Set default backend
;; (setq gptel-backend gptel-claude-normal
;;       gptel-model 'claude-3-7-sonnet-20250219)

;; ;; Set the default system prompt
;; (setq gptel-system-prompt gptel-claude-normal-system-prompt)

;; ;; Create functions to switch between backends
;; (defun gptel-use-claude-normal ()
;;   (interactive)
;;   (setq gptel-backend gptel-claude-normal)
;;   (message "Switched to normal Claude Sonnet"))

;; (defun gptel-use-claude-thinking ()
;;   (interactive)
;;   (setq gptel-backend gptel-claude-thinking)
;;   (message "Switched to Claude Sonnet with thinking enabled"))


;; =================================================================
;; GPTEL Configuration for Mistral
;; =================================================================
(use-package gptel
  :ensure t
  :config
  ;; Define the list of models you want to use
  (defvar my/gptel-mistral-models
    '("mistral-large-latest"
      "codestral-2501")
    "A list of available Mistral models.")

  ;; Set up Mistral as a backend
  (setq gptel-backend (gptel-make-openai "Mistral"
                        :host "api.mistral.ai"
                        :endpoint "/v1/chat/completions"
                        :stream t
                        ;; Securely get key from ~/.authinfo.gpg
                        :key (auth-source-pick-first-password :host "api.mistral.ai")
                        :models my/gptel-mistral-models))

  ;; Set Mistral as the default backend and model
  (setq gptel-default-backend gptel-backend)
  (setq gptel-model 'mistral-large-latest) ; Set a sensible default

  ;; --- Logging Functionality ---
  (defun my/gptel-log-request-details (request)
    "Log the model and prompt before sending a gptel request."
    (let ((model (plist-get request :model)))
      (message "GPTEL REQUEST -> Using model: %s" model)))

  ;; Add the logging function to the official gptel hook
  (add-hook 'gptel-before-request-hook #'my/gptel-log-request-details))

;; --- Interactive Model Selector ---
(defun gptel-select-model ()
  "Select a Mistral model and set it as the current gptel-model."
  (interactive)
  (let ((model (completing-read "Select model: " my/gptel-mistral-models nil t)))
    (setq gptel-model model)
    (message "Switched to model: %s" model)))

;; Optional: Bind a key to your model selector
(global-set-key (kbd "C-c g m") 'gptel-select-model)

(provide 'setup-gptel)


