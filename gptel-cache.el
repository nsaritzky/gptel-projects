;;; gptel-cache.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Saritzky
;;
;; Author: Saritzky <e5746916@ML9LV724T9>
;; Maintainer: Saritzky <e5746916@ML9LV724T9>
;; Created: January 10, 2025
;; Modified: January 10, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nsaritzky-atelio/gptel-cache
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'gptel)
(require 'gptel-anthropic)

(dolist (model '(claude-3-opus-20240229
                 claude-3-5-sonnet-20241022))
  (cl-pushnew 'cache (get model :capabilities)))

(defcustom gptel-cache-strategy 'ephemeral
  "Strategy for caching promps in Anthropic models.
Presently, the only valid value is \=ephemeral\="
  :type '(choice (const :tag "Cache for approximately 5 minutes" ephemeral))
  :group 'gptel-cache)

(defcustom gptel-cache-enabled t
  "Whether to enable caching for LLMs that support it."
  :type 'boolean
  :group 'gptel-cache)

(cl-defstruct gptel-cache-message
  "A GPTel message with cache control."
  role content cache)

(cl-defmethod gptel--request-data ((backend gptel-anthropic) messages)
  "Format MESSAGES for the Anthropic backend with cache control.

MESSAGES should be a list of plists containing message data.
Messages with cache control are moved to the beginning of the
array for more effective caching."
  (let (cached-messages regular-messages)
    ;; Split messages into cached and regular
    (dolist (msg messages)
      (let* ((content (plist-get msg :content))
             (msg-plist
              `(:role ,(plist-get msg :role)
                :content ,content)))
        ;; Add cache control
        (if (and gptel-cache-enabled
                 (plist-get msg :cache)
                 (gptel--model-capable-p 'cache))
            (progn
              (setq msg-plist
                    (plist-put msg-plist :cache_control
                               `(:type ,(symbol-name gptel-cache-strategy))))
              (push msg-plist cached-messages))
          (push msg-plist regular-messages))))

    ;; Combine messages with cached ones first
    (let ((request-plist
           `(:model ,(gptel--model-name gptel-model)
             :stream ,(or (and gptel-stream gptel-use-curl
                               (gptel-backend-stream backend))
                          :json-false)
             :messages ,(vconcat (nreverse cached-messages)
                                 (nreverse regular-messages)))))
      ;; Add standard request parameters
      (when gptel-temperature
        (plist-put request-plist :temperature gptel-temperature))
      (when gptel-max-tokens
        (plist-put request-plist :max_tokens gptel-max-tokens))
      (gptel--merge-plists
       request-plist
       (gptel-backend-request-params backend)
       (gptel--model-request-params gptel-model)))))

(cl-defmethod gptel--wrap-user-prompt ((backend gptel-anthropic) prompts
                                       &optional inject-media)
  "Wrap the user prompt in PROMPTS with the context.
if INJECT-MEDIA is non-nil, wrap it with the media files instead.
Context is wrapped into separate messages at the start of the
conversation to facilitate caching."
  (let ((context nil)
        (can-cache (and gptel-cache-enabled
                        (gptel-backend-stream backend)
                        (gptel--model-capable-p 'cache))))
    (if inject-media
        (when-let ((media-list (gptel-context--collect-media)))
          (setq context
                (vconcat
                 (gptel--anthropic-parse-multipart media-list))))
      (when-let ((wrapped-context (gptel-context--wrap nil)))
        (setq context
              (vector `(:type "text" :text ,wrapped-context)))))

    (if (null context)
        prompts
      (let ((context-msg
             `(:role "user"
               :content ,context
               ,@(when can-cache '(:cache t)))))
        (if (plist-get (car (last prompts)) :cache)
            (append (list context-msg) (butlast prompts))
          (append (list context-msg) prompts))))))

(defvar gptel-cache--hits 0
  "Number of cache hits in the current Emacs session.")

(defvar gptel-cache--misses 0
  "Number of cache misses in the current Emacs session.")

(cl-defmethod gptel--parse-response :around ((backend gptel-anthropic) response info)
  "Track cache hits/misses in responses."
  (when-let ((cache-status (plist-get response :cache_status)))
    (pcase cache-status
      ("hit" (cl-incf gptel-cache--hits))
      ("miss" (cl-incf gptel-cache--misses))))
  (cl-call-next-method))

(defun gptel-cache-statistics ()
  "Display statistics about cache effectiveness."
  (interactive)
  (let* ((hits gptel-cache--hits)
         (misses gptel-cache--misses)
         (total (+ hits misses))
         (hit-rate (if (> total 0)
                       (* 100.0 (/ hits (float total)))
                     0.0)))
    (message "Cache statistics: %d hits, %d misses (%.1f%% hit rate)"
             hits misses hit-rate)))

(defun gptel-cache-clear-statistics ()
  "Reset cache hit/miss counters."
  (interactive)
  (setq gptel-cache--hits 0
        gptel-cache--misses 0)
  (message "Cache statistics cleared"))

(declare-function transient-append-suffix "transient")
(declare-function transient-define-infix "transient")

;;;###autoload
(defun gptel-cache-setup-menu ()
  "Set up the cache control menu items for gptel."
  (require 'transient)
  (transient-define-infix gptel-cache--infix-cache-enabled ()
    :class 'gptel--switches
    :variable 'gptel-cache-enabled
    :argument "cache"
    :description "Enable response caching"
    :key "C")

  (transient-append-suffix 'gptel-menu '(0 -1)
    '["Cache"
      :if (lambda ()
            (and (gptel--model-capable-p 'cache)
                 gptel-expert-commands))
      ("C" "Enable caching" gptel-cache--infix-cache-enabled)
      ("H" "Show statistics" gptel-cache-statistics)]))

(provide 'gptel-cache)
;;; gptel-cache.el ends here
