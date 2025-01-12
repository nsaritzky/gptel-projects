;;; gptel-projects.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Saritzky
;;
;; Author: Saritzky <e5746916@ML9LV724T9>
;; Maintainer: Saritzky <e5746916@ML9LV724T9>
;; Created: January 09, 2025
;; Modified: January 09, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/nsaritzky-atelio/gptel-projects
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'gptel)
(require 'gptel-context)
(require 'gptel-anthropic)
(require 'gptel-projects-workspace)
(require 'projectile)
(require 'request)
(require 'json)

(defgroup gptel-projects nil
  "Project-specific context management for gptel."
  :group 'gptel)

(defcustom gptel-projects-filename ".gptel-context"
  "Name of the file storing project-specific context lists.
This file will be stored in the project root directory."
  :type 'string
  :group 'gptel-projects)

(defcustom gptel-projects-relative-paths t
  "Whether to store file paths relative to project root."
  :type 'boolean
  :group 'gptel-projects)

(defcustom gptel-projects-token-limits
  '((gpt-3.5-turbo . 16000)
    (gpt-4 . 8000)
    (gpt-4-32k . 32000)
    (gpt-4-turbo . 128000)
    (claude-3-5-sonnet-20241022 . 200000)
    (claude-3-5-haiku-20241022 . 200000)
    (claude-3-opus-20240229 . 200000))
  "Alist mapping models to their maximum token limits.
When non-nil, warn if context exceeds this limit.
Each entry should be (model-name . limit).
Set specific model limits to nil to disable checking."
  :type '(alist :key-type symbol
          :value-type (choice (integer :tag "Token limit")
                              (const :tag "No limit" nil)))
  :group 'gptel-projects)

(defcustom gptel-projects-token-limit-action 'warn
  "Action to take when context exceeds token limit.
- warn: Display a warning message
- ask: Ask user whether to continue
- block: Don't add files that would exceed the limit"
  :type '(choice (const :tag "Warn only" warn)
          (const :tag "Ask to continue" ask)
          (const :tag "Block addition" block))
  :group 'gptel-projects)

(defcustom gptel-projects-chat-dir ".gptel"
  "Directory name for storing project-specific chat files.
This directory is created in the project root."
  :type 'string
  :group 'gptel-projects)

(defcustom gptel-projects-default-chat-name "chat.gptel"
  "Default name for new chat files."
  :type 'string
  :group 'gptel-projects)

(defcustom gptel-projects-save-chats t
  "Whether to save project chat buffers automatically.
When non-nil, chat buffers are saved when Emacs is closed or when
the project is switched."
  :type 'boolean
  :group 'gptel-projects)

(defcustom gptel-projects-after-response-functions nil
  "Hooks run after each LLM response in a project chat buffer.
Each function is called with the response start and end positions
as arguments."
  :type 'hook
  :group 'gptel-projects)

(defface gptel-projects-header-face
  '((((class color) (min-colors 88) (background light))
     :foreground "RoyalBlue4" :extend t)
    (((class color) (min-colors 88) (background dark))
     :foreground "LightSkyBlue" :extend t))
  "Face for project chat buffer headers."
  :group 'gptel-projects)

(defvar gptel-projects--chat-buffers (make-hash-table :test 'equal)
  "Hash table mapping project roots to their chat buffers.")

(defvar gptel-projects--chat-history nil
  "History of chat names for the current project.")

(defvar-local gptel-projects--root nil
  "Project root directory for this chat buffer.")

(defvar gptel-projects--applying-context nil
  "Internal flag to prevent recursive context application.")

(defun gptel-projects--get-chat-dir (root)
  "Get the chat directory for project at ROOT."
  (let ((dir (expand-file-name gptel-projects-chat-dir root)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun gptel-projects--buffer-name (project-name- chat-name)
  "Generate a buffer name for PROJECT-NAME- and CHAT-NAME."
  (format "*gptel: %s/%s*" project-name- chat-name))

(defun gptel-projects--list-project-chats (root)
  "List all chat files in project ROOT."
  (let* ((chat-dir (gptel-projects--get-chat-dir root))
         (files (and (file-exists-p chat-dir)
                     (directory-files chat-dir nil "\\.gptel$"))))
    (or files
        (list gptel-projects-default-chat-name))))

;;;###autoload
(defun gptel-projects-new (chat-name)
  "Create a new chat buffer for the current project.
CHAT-NAME is the name for this chat buffer. If called
interactively, prompt for the name."
  (interactive
   (list
    (let* ((root (projectile-project-root))
           (chats (gptel-projects--list-project-chats root))
           (default-name
            (let ((base gptel-projects-default-chat-name)
                  (n 0))
              (while (member
                      (if (= n 0) base
                        (format "%s<%d>"
                                (file-name-sans-extension base) n))
                      chats)
                (cl-incf n))
              (if (= n 0) base
                (format "%s<%d>"
                        (file-name-sans-extension base) n)))))
      (read-string
       (format "Chat name (default '%s'): " default-name)
       nil 'gptel-projects--chat-history default-name))))
  (unless (projectile-project-root)
    (user-error "Not in a project"))

  (let* ((root (projectile-project-root))
         (existing-bufs (gethash root gptel-projects--chat-buffers))
         (chat-file
          (expand-file-name chat-name (gptel-projects--get-chat-dir root)))
         (buf-name (gptel-projects--buffer-name
                    (projectile-project-name) chat-name))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (funcall gptel-default-mode)
      (gptel-mode)
      (setq-local gptel-projects--root root)
      (puthash root
               (cons buf (remq buf existing-bufs))
               gptel-projects--chat-buffers)
      (add-hook 'gptel-post-response-functions
                #'gptel-projects--after-response nil t)
      (add-hook 'kill-buffer-hook
                #'gptel-projects--on-kill-buffer nil t)
      (unless (file-exists-p chat-file)
        (goto-char (point-min))
        (insert
         (propertize
          (format "Chat: %s\nProject: %s\nCreated: %s\n\n"
                  chat-name (projectile-project-name)
                  (format-time-string "%Y-%m-%d %T"))
          'face 'gptel-projects-header-face)))
      (setq buffer-file-name chat-file)
      (when (file-exists-p chat-file)
        (insert-file-contents chat-file))
      (goto-char (point-max))
      (hack-local-variables))
    (pop-to-buffer buf)
    buf))

;;;###autoload
(defun gptel-projects-list ()
  "List and switch to a project chat buffer."
  (interactive)
  (unless (projectile-project-root)
    (user-error "Not in a project"))

  (let* ((root (projectile-project-root))
         (chat-dir (gptel-projects--get-chat-dir root))
         (project-name- (projectile-project-name))
         (existing-bufs (gethash root gptel-projects--chat-buffers))
         (choices
          (delete-dups
           (append
            (mapcar
             (lambda (buf)
               (cons (string-remove-prefix
                      "*gptel: " (string-remove-suffix "*"
                                                       (buffer-name buf)))
                     buf))
             existing-bufs)
            (mapcar
             (lambda (file)
               (cons (format "%s/%s"
                             project-name- file)
                     (expand-file-name file chat-dir)))
             (directory-files chat-dir nil "\\.gptel$"))))))
    (if (null choices)
        (gptel-projects-new gptel-projects-default-chat-name)
      (let* ((choice
              (completing-read
               "Select chat: "
               (lambda (str pred action)
                 (if (eq action 'metadata)
                     `(metadata
                       (annotation-function
                        . ,(lambda (key)
                             (if-let ((buf (cdr (assoc key choices))))
                                 (if (buffer-live-p buf)
                                     "  [Open]"
                                   "")
                               ""))))
                   (complete-with-action
                    action choices str pred)))))
             (target (cdr (assoc choice choices))))
        (cond
         ((and (bufferp target) (buffer-live-p target))
          (pop-to-buffer target))
         ((stringp target)
          (let ((buf-name (gptel-projects--buffer-name
                           project-name-
                           (file-name-nondirectory target))))
            (if-let ((buf (get-buffer buf-name)))
                (pop-to-buffer buf)
              (gptel-projects-new (file-name-nondirectory target))))))))))

(defun gptel-projects--save-chats (&optional root)
  "Save all chat buffers for project ROOT.
If ROOT is nil, save chats for the current project."
  (let ((roots (if root (list root)
                 (hash-table-keys gptel-projects--chat-buffers))))
    (dolist (project-root roots)
      (dolist (buf (gethash project-root gptel-projects--chat-buffers))
        (when (and (buffer-live-p buf)
                   (buffer-modified-p buf))
          (with-current-buffer buf
            (save-buffer)))))))

(defun gptel-projects--after-response (beg end)
  "Function called after gptel response in project chat buffers.
Runs `gptel-projects-after-response-functions' with the response
positisions BEG and END."
  (run-hook-with-args 'gptel-projects-after-response-functions beg end))

(defun gptel-projects--on-kill-buffer ()
  "Clean up when a project chat buffer is killed."
  (when (and gptel-projects-save-chats
             (buffer-modified-p))
    (save-buffer))
  (when gptel-projects--root
    (let ((bufs (gethash gptel-projects--root
                         gptel-projects--chat-buffers)))
      (puthash gptel-projects--root
               (remq (current-buffer) bufs)
               gptel-projects--chat-buffers))))

(defvar gptel-projects-table (make-hash-table :test 'equal)
  "Hash table mapping project roots to their context file lists.")

(defun gptel-projects--get-list ()
  "Get the context list for current project."
  (when-let ((root (projectile-project-root)))
    (gethash root gptel-projects-table)))

(defun gptel-projects--set-list (list)
  "Set the context LIST for current project."
  (when-let ((root (projectile-project-root)))
    (puthash root list gptel-projects-table)))

(defun gptel-projects--context-file ()
  "Get the context file path for the current project."
  (when-let ((project-root (projectile-project-root)))
    (let ((file (expand-file-name gptel-projects-filename project-root)))
      (message "Context file path: %s" file) ; Debug logging
      file)))

(defun gptel-projects--relative-to-root (file)
  "Convert FILE path to be relative to project root if needed."
  (if (and gptel-projects-relative-paths
           (projectile-project-root))
      (file-relative-name file (projectile-project-root))
    file))

(defun gptel-projects--absolute-from-root (file)
  "Convert FILE path to absolute from project root if needed."
  (if (and gptel-projects-relative-paths
           (projectile-project-root)
           (not (file-name-absolute-p file)))
      (expand-file-name file (projectile-project-root))
    file))

(defun gptel-projects-save ()
  "Save the current context list to project's context file."
  (when-let ((context-file (gptel-projects--context-file)))
    (let ((project-list (gptel-projects--get-list)))
      (message "Saving context list: %S" project-list) ; Debug logging
      (make-directory (file-name-directory context-file) t)
      (with-temp-file context-file
        (let ((print-length nil)
              (print-level nil))
          (prin1 project-list (current-buffer))))
      (message "Saved context to %s" context-file))))

(defun gptel-projects-load ()
  "Load the context list from project's context file."
  (when-let* ((context-file (gptel-projects--context-file))
              ((file-exists-p context-file)))
    (message "Loading context from: %s" context-file) ; Debug logging
    ;; Clear existing context first
    (gptel-context-remove-all)
    ;; Load and apply new context
    (with-temp-buffer
      (insert-file-contents context-file)
      (goto-char (point-min))
      (gptel-projects--set-list (read (current-buffer)))
      (message "Loaded context list: %S" (gptel-projects--get-list))
      )))

(defun gptel-projects--add-and-save (rel-file abs-file)
  "Add REL-FILE to context list and save after token check passes.
ABS-FILE is the absolute path to the file."
  (message "Adding file to context: %s" rel-file) ; Debug logging
  (let ((project-list (or (gptel-projects--get-list) nil)))
    (unless (member rel-file project-list)
      (gptel-projects--set-list (cons rel-file project-list))
      (gptel-projects-save)
      (gptel-context-add-file abs-file)
      (message "Added and saved %s to project context" rel-file))))

(defun gptel-projects-add-file (file)
  "Add FILE to the project's context list."
  (interactive
   (list (read-file-name "Add file to project context: "
                         (projectile-project-root))))
  (unless (projectile-project-root)
    (user-error "Not in a projectile project"))

  (let* ((rel-file (gptel-projects--relative-to-root file))
         (abs-file (gptel-projects--absolute-from-root rel-file)))
    (unless (member rel-file (gptel-projects--get-list))
      ;; Check if adding would exceed token limit
      (when (file-readable-p abs-file)
        (let ((new-context
               (concat
                (or (gptel-context--string (gptel-context--collect)) "")
                "\n\n"
                (with-temp-buffer
                  (insert-file-contents abs-file)
                  (buffer-string)))))
          (gptel-projects--check-token-limit
           new-context
           (lambda (within-limit)
             (if within-limit
                 (gptel-projects--add-and-save rel-file abs-file)
               (message "File not added - would exceed token limit")))))))))

(defun gptel-projects-remove-file (file)
  "Remove FILE from the project's context list."
  (interactive
   (list (completing-read "Remove file from project context: "
                          (or (gptel-projects--get-list) '()) nil t)))
  (unless (projectile-project-root)
    (user-error "Not in a projectile project"))

  (let ((abs-file (gptel-projects--absolute-from-root file)))
    (gptel-projects--set-list
     (delete file (gptel-projects--get-list)))
    (gptel-projects-save)
    (gptel-context-remove abs-file)
    (message "Removed %s from project context" file)))

(define-derived-mode gptel-projects-list-mode special-mode "GPTel Projects"
  "Major mode for displaying GPTel project context lists."
  (setq-local revert-buffer-function #'gptel-projects--refresh-list-buffer))

(defun gptel-projects--refresh-list-buffer (&optional _ignore-auto _noconfirm)
  "Refresh the GPTel projects list buffer."
  (let* ((inhibit-read-only t)
         (project-root default-directory)
         (project-list (gptel-projects--get-list)))
    (erase-buffer)
    (insert (format "GPTel Context Files for Project: %s\n\n"
                    (let ((default-directory project-root))
                      (projectile-project-name))))
    (if project-list
        (dolist (file project-list)
          (let ((abs-file (gptel-projects--absolute-from-root file )))
            (insert (format "  %s%s\n" file
                            (if (file-exists-p abs-file)
                                ""
                              " [missing]")))))
      (insert "  No files in project context.\n"))
    (goto-char (point-min))))

(defun gptel-projects-list-files ()
  "Display the list of files in the project's context."
  (interactive)
  (unless (projectile-project-root)
    (user-error "Not in a projectile project"))

  (let ((buf (get-buffer-create "*GPTel Project Context*"))
        (project-dir default-directory))
    (with-current-buffer buf
      (gptel-projects-list-mode)
      (setq default-directory project-dir)
      (gptel-projects--refresh-list-buffer))
    (display-buffer buf)))

(defun gptel-projects-apply ()
  "Apply all files from the project's context list to the current gptel context."
  (interactive)
  (unless gptel-projects--applying-context
    (let ((gptel-projects--applying-context t))
      (message "gptel-projects-apply called")
      (gptel-context-remove-all)
      (when-let ((project-list (gptel-projects--get-list)))
        (dolist (file project-list)
          (let ((abs-file (gptel-projects--absolute-from-root file)))
            (when (file-exists-p abs-file)
              (gptel-context-add-file abs-file))))
        (gptel-projects-workspace--add-code-info-to-context)))))

(defun gptel-projects-clear ()
  "Clear all context for the current project."
  (interactive)
  (unless (projectile-project-root)
    (user-error "Not in a projectile project"))
  (gptel-projects--set-list nil)
  (gptel-projects-save)
  (gptel-context-remove-all)
  (message "Cleared project context"))

(defun gptel-projects--switch-project ()
  "Handle switching project contexts.
This should be called when switching projects or initializing a project."
  (gptel-context-remove-all)
  (gptel-projects--set-list nil)
  (gptel-projects-apply))

;;;###autoload
(define-minor-mode gptel-projects-mode
  "Automatically apply project-specific context in gptel buffers."
  :global t
  :group 'gptel-projects
  (if gptel-projects-mode
      (progn
        (when (projectile-project-root)
          (gptel-projects-load))
        (add-hook 'projectile-after-switch-project-hook
                  #'gptel-projects-load)
        (add-hook 'projectile-before-switch-project-hook
                  #'gptel-projects--save-chats)
        (add-hook 'gptel-mode-hook #'gptel-projects-apply)
        (add-hook 'kill-emacs-hook
                  (lambda ()
                    (when gptel-projects-save-chats
                      (maphash (lambda (root _)
                                 (gptel-projects--save-chats root))
                               gptel-projects--chat-buffers)))))
    (remove-hook 'projectile-after-switch-project-hook
                 #'gptel-projects-load)
    (remove-hook 'projectile-before-switch-project-hook
                 #'gptel-projects--save-chats)
    (remove-hook 'gptel-mode-hook #'gptel-projects-apply)
    (remove-hook 'kill-emacs-hook #'gptel-projects--save-chats)
    (gptel-context-remove-all)
    (gptel-projects--set-list nil)))

(defun gptel-projects-add-from-projectile ()
  "Add files to context using projectile's file selection interface."
  (interactive)
  (unless (projectile-project-root)
    (user-error "Not in a projectile project"))

  (let* ((file-candidates (projectile-project-files (projectile-project-root)))
         (files (completing-read-multiple
                 "Add files to context: "
                 (mapcar (lambda (f) (concat f "|")) file-candidates)))
         (count 0))
    (dolist (file (mapcar (lambda (f) (string-remove-suffix "|" f)) files))
      (unless (member file (gptel-projects--get-list))
        (cl-incf count)
        (let ((abs-file (gptel-projects--absolute-from-root file)))
          (gptel-projects--add-and-save file abs-file))))
    (when (> count 0)
      (gptel-projects-save)
      (message "Added %d file%s to project context"
               count (if (= count 1) "" "s")))))

(setq request-log-level 'debug)

(defun gptel-projects--count-tokens (text callback &optional model)
  "Count tokens in TEXT using Anthropic's API and MODEL.
Calls CALLBACK with the token count and model as arguments, or nil if counting fails.
MODEL defaults to the current gptel-model.

CALLBACK should be a function taking three arguments: (count model error-message).
If successful, error-message will be nil."
  (require 'request)
  (let* ((model-name (gptel--model-name (or model gptel-model)))
         (json-data (json-encode
                     `((model . ,model-name)
                       (messages . [((role . "user")
                                     (content . ,text))])))))
    (request "https://api.anthropic.com/v1/messages/count_tokens"
      :type "POST"
      :headers `(("Content-Type" . "application/json")
                 ("anthropic-version" . "2023-06-01")
                 ("x-api-key" . ,(gptel--get-api-key)))
      :data json-data
      :parser 'json-read
      :error-parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback (cdr (assq 'input_tokens data)) model-name nil)))
      :error (cl-function
              (lambda (&key error-thrown response &allow-other-keys)
                (let* ((response-status (request-response-status-code response))
                       (error-msg
                        (or (when-let ((err-json (request-response-data response)))
                              (plist-get err-json :error))
                            error-thrown)))
                  (funcall callback nil model-name
                           (format "Token count failed: HTTP %s - %S"
                                   response-status error-msg))))))))

(defun gptel-projects-show-token-count ()
  "Display token count for current project context."
  (interactive)
  (unless (projectile-project-root)
    (user-error "Not in a projectile project"))

  (if (null (gptel-projects--get-list))
      (message "Project context is empty - 0 tokens")
    (let ((context (gptel-context--string
                    (gptel-context--collect))))
      (if (null context)
          (message "Project context is empty - 0 tokens")
        (gptel-projects--count-tokens
         context
         (lambda (token-count model error-message)
           (if token-count
               (let ((limit (alist-get (gptel--intern model)
                                       gptel-projects-token-limits)))
                 (message "Project context uses approximately %d tokens%s"
                          token-count
                          (if (and limit (> token-count limit))
                              (format " (EXCEEDS %s's %d token LIMIT!)"
                                      model limit)
                            (format " (%s's limit: %s)"
                                    model (if limit
                                              (format "%d tokens" limit)
                                            "none")))))
             (message "Failed to count tokens: %s" error-message))))))))

(cl-defun gptel-projects--check-token-limit (text callback)
  "Check if TEXT would exceed token limit.
Calls CALLBACK with t if within limit or no limit set, nil otherwise.
Prompts user depending on gptel-projects-token-limit-action."
  (gptel-projects--count-tokens
   text
   (lambda (token-count model error)
     (let ((limit (and model (alist-get (gptel--intern model)
                                        gptel-projects-token-limits))))
       (if (or error (null limit))
           (funcall callback t)         ; Count failed or no limit, assume ok
         (if (<= token-count limit)
             (funcall callback t)
           (pcase gptel-projects-token-limit-action
             ('warn
              (message "Warning: Context exceeds %s's token limit (%d > %d)"
                       model token-count limit)
              (funcall callback t))
             ('ask
              (if (y-or-n-p
                   (format "Context exceeds %s's token limit (%d > %d). Continue? "
                           model token-count limit))
                  (funcall callback t)
                (funcall callback nil)))
             ('block (funcall callback nil)))))))))

(provide 'gptel-projects)
;;; gptel-projects.el ends here
