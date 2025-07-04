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
(defcustom gptel-normal-system-prompt
  "You are a large language model living in Emacs and a helpful assistant pro-coder. Respond concisely. You will write output in formal Org mode, with line-width max 80 characters."
  "System prompt for normal LLM mode."
  :type 'string
  :group 'gptel)

;; Define LLM backends
(setq gptel-mistral (gptel-make-openai "Mistral"
                      :host "api.mistral.ai"
                      :endpoint "/v1/chat/completions"
                      :stream t
                      :key (my/get-secret "api.mistral.ai" "apikey")
                      :models '(mistral-medium-2505
                                codestral-2501
                                magistral-medium-2505)))

(setq gptel-claude (gptel-make-anthropic "Claude"
                     :stream t 
                     :key (my/get-secret "api.anthropic.ai" "apikey")
                     :models '(claude-opus-4-20250514)))

(setq gptel-claude-thinking (gptel-make-anthropic "Claude-thinking"
                              :key (my/get-secret "api.anthropic.ai" "apikey")
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

;; Set as default model
(setq gptel-model "mistral-medium-2025-05")

;; Set default backend
(setq gptel-backend gptel-mistral
      gptel-model 'mistral-medium-2505)

;; Set the default system prompt
(setq gptel-system-prompt gptel-normal-system-prompt)

;; Create a generic function to switch backends and models
(defun gptel-switch-backend (backend model)
  "Switch to the specified BACKEND and MODEL."
  (interactive)
  (setq gptel-backend backend
        gptel-model model)
  (message "Switched to %s with model %s" (gptel-backend-name backend) model))

;; Create functions to switch between backends
(defun gptel-use-mistral ()
  (interactive)
  (gptel-switch-backend gptel-mistral 'mistral-medium-2505))

(defun gptel-use-codestral ()
  (interactive)
  (gptel-switch-backend gptel-mistral 'codestral-2501))

(defun gptel-use-magistral ()
  (interactive)
  (gptel-switch-backend gptel-mistral 'magistral-medium-2506))

(defun gptel-use-claude ()
  (interactive)
  (gptel-switch-backend gptel-claude 'claude-opus-4-20250514))

(defun gptel-use-claude-thinking ()
  (interactive)
  (gptel-switch-backend gptel-claude-thinking 'claude-opus-4-20250514))

(setq gptel-default-mode 'org-mode)
(setq gptel-setting '(system-prompt . "You are a large language model living in Emacs and a helpful pro-coder assistant. Respond concisely."))

(provide 'setup-gptel)


