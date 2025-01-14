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

(require 'treesit)
(require 'projectile)

(defcustom gptel-projects-workspace-collect-symbols t
  "Wheter to collect symbols from project files using tree-sitter.
When non-nil add names of functions, classes, and other symbols
to the context when analyzing project files."
  :type 'boolean
  :group 'gptel-projects)

(defcustom gptel-projects-workspace-code-file-extensions
  '(".el" ".py" ".js" ".jsx" ".ts" ".tsx" ".java" ".c" ".cpp" ".h" ".hpp" ".rs" ".go")
  "List of file extensions to consider as code files for symbol extraction."
  :type '(repeat string)
  :group 'gptel-projects)

(defun gptel-projects-workspace--get-treesit-language (filename)
  "Determine the tree-sitter language for FILENAME based on extension."
  (when-let* ((ext (file-name-extension filename))
              (lang-name
               (cond
                ((member (concat "." ext) '(".c" ".h")) "c")
                ((member (concat "." ext) '(".cpp" ".hpp")) "cpp")
                ((equal ext "py") "python")
                ((equal ext "js") "javascript")
                ((equal ext "jsx") "jsx")
                ((equal ext "ts") "typescript")
                ((equal ext "tsx") "tsx")
                ((equal ext "java") "java")
                ((equal ext "go") "go")
                ((equal ext "rs") "rust")
                ((equal ext "el") "elisp"))))
    (intern lang-name)))

(defun gptel-projects-workspace--get-parameters (node lang)
  "Extract parameter list from NODE based on LANG."
  (pcase lang
    ('python
     (when-let ((params (treesit-node-child-by-field-name node "parameters")))
       (mapcar #'treeset-node-text
               (treesit-filter-child
                params (lambda (n) (equal (treesit-node-type n) "identifier"))))))
    ((or 'javascript 'typescript 'tsx 'jsx)
     (when-let ((params (treesit-node-child-by-field-name node "parameters")))
       (let (param-names)
         (treesit-search-subtree
          params (lambda (n)
                   (when (member (treesit-node-type n)
                                 '("identifier" "pattern_identifier"))
                     (push (treesit-node-text n) param-names))))
         (nreverse param-names))))
    ('java
     (when-let ((params (treesit-node-child-by-field-name node "parameters")))
       (let (param-names)
         (treesit-search-subtree
          params (lambda (n)
                   (when-let ((id-node
                               (treesit-node-child-by-field-name n "name")))
                     (push (treesit-node-text id-node) param-names))))
         (nreverse param-names))))
    ('go
     (when-let ((params (treesit-node-child-by-field-name node "parameters")))
       (let (param-names)
         (treesit-search-subtree
          params (lambda (n)
                   (when (equal (treesit-node-type n) "parameter-declaration")
                     (push (treesit-node-text
                            (treesit-node-child-by-field-name n "name"))
                           param-names))))
         (nreverse param-names))))
    ('rust
     (when-let ((params (treesit-node-child-by-field-name node "parameters")))
       (let ((param-names nil))
         (treesit-search-subtree
          params (lambda (n)
                   (when (equal (treesit-node-type n) "identifier")
                     (progn (push (treesit-node-text n) param-names)
                            nil))
                   nil))
         (nreverse param-names))))
    ('elisp
     (when-let ((params (treesit-node-child-by-field-name node "parameters")))
       (mapcar #'treesit-node-text
               (treesit-filter-child
                params (lambda (n) (equal (treesit-node-type n) "symbol"))))))))

(defun gptel-projects-workspace--format-symbol (node-text params)
  "Format NODE-TEXT with PARAMS as a symbol definition."
  (if params
      (format "%s(%s)" node-text (string-join params ", "))
    (format "%s()" node-text)))

(defun gptel-projects-workspace--extract-symbols (filename)
  "Extract symbols from FILENAME using tree-sitter if available."
  (when-let* ((lang (gptel-projects-workspace--get-treesit-language filename))
              ((fboundp 'treesit-ready-p))
              ((treesit-ready-p lang)))
    (with-temp-buffer
      (insert-file-contents (gptel-projects--absolute-from-root filename))
      (let* ((parser (treesit-parser-create lang))
             (root-node (treesit-parser-root-node parser))
             (symbols nil)
             (patterns
              (pcase lang
                ('python
                 '((class_definition name: (identifier) @class)
                   (function_definition
                    name: (identifier)
                    parameters: (parameters)) @function))
                ((or 'javascript 'jsx)
                 '(((class_declaration name: (identifier) @class)
                    (function_declaration
                     name: (identifier)
                     parameters: (formal_parameters)) @function
                    (method_definition
                     name: (property_identifier) @method
                     parameters: (formal_parameters) @params))))
                ((or 'typescript 'tsx)
                 '(((class_declaration name: (type_identifier) @class)
                    (function_declaration
                     name: (identifier)
                     parameters: (formal_parameters)) @function
                    (method_definition
                     name: (property_identifier)
                     parameters: (formal_parameters)) @method)))
                ('java
                 '(((class_declaration name: (identifier) @class)
                    (method_declaration
                     name: (identifier)
                     parameters: (formal_parameters)) @method)))
                ('go
                 '(((type_declaration (type_spec name: (type-identifier) @type))
                    (function_declaration
                     name: (identifier)
                     parameters: (parameter_list)) @function)))
                ('rust
                 '(((function_item
                     name: (identifier)) @function
                     (struct_item name: (type_identifier) @struct))))
                ('elisp
                 '(((function_definition name: (symbol)
                     parameters: (_)) @function))))))
        (when patterns
          (dolist (pattern patterns)
            (dolist (match (treesit-query-capture root-node pattern))
              (pcase-let ((`(,name . ,node) match))
                (if (and (memq (intern (symbol-name name))
                               '(function method definition.function)))
                    (push (cons (intern (symbol-name name))
                                (gptel-projects-workspace--format-symbol
                                 (treesit-node-text (treesit-node-child-by-field-name node "name"))
                                 (gptel-projects-workspace--get-parameters node lang)))
                          symbols)
                  (push (cons (intern (symbol-name name))
                              (treesit-node-text node))
                        symbols)))))
          (treesit-parser-delete parser)
          (nreverse symbols))))))

(defun gptel-projects-workspace-get-symbol-definition (filename symbol)
  "Find definition of SYMBOL in FILENAME using tree-sitter."
  (let ((abs-file (gptel-projects--absolute-from-root filename)))
    (unless (file-exists-p abs-file)
      (error "File does not exist: %s" abs-file))
    (with-current-buffer (find-file-noselect abs-file)
      (unless (treesit-parser-list)
        (treesit-parser-create (gptel-projects-workspace--get-treesit-language abs-file)))
      (let* ((parser (car (treesit-parser-list)))
             (root-node (treesit-parser-root-node parser))
             (language (treesit-parser-language parser))
             (query-patterns
              (pcase language
                ('elisp
                 `((function_definition
                    name: (symbol) @name
                    (:equal @name ,symbol)) @function))
                ('java
                 `((method_declaration
                    name: (identifier) @name
                    (:equal @name ,symbol)) @method
                    (class_declaration
                     name: (identifier) @name
                     (:equal @name ,symbol)) @class
                    (field_declaration
                     declarator: (variable_declarator
                                  name: (identifier) @name
                                  (:equal @name ,symbol)) @field)))
                ((or 'javascript 'typescript 'tsx)
                 `((function_declaration
                    name: (identifier) @name
                    (:equal @name ,symbol)) @function
                    (class_declaration
                     name: (identifier) @name
                     (:equal @name ,symbol)) @class
                    (method_definition
                     name: (property_identifier) @name
                     (:equal @name ,symbol)) @method
                    (variable_declaration
                     declarator: (variable_declarator
                                  name: (identifier) @name
                                  (:equal @name ,symbol))) @var
                    (arrow_function
                     name: (identifier) @name
                     (:equal @name ,symbol)) @arrow))
                ('jsx
                 `((function_declaration
                    name: (identifier) @name
                    (:equal @name ,symbol)) @function
                    (class_declaration
                     name: (identifier) @name
                     (:equal @name ,symbol)) @class
                    (method_definition
                     name: (property_identifier) @name
                     (:equal @name ,symbol)) @method
                    (variable_declaration
                     declarator: (variable_declarator
                                  name: (identifier) @name
                                  (:equal @name ,symbol))) @var
                    (arrow_function
                     name: (identifier) @name
                     (:equal @name ,symbol)) @arrow))
                ('go
                 `((function_declaration
                    name: (identifier) @name
                    (:equal @name ,symbol)) @function
                    (method_declaration
                     name: (field_identifier) @name
                     (:equal @name ,symbol)) @method
                    (const_declaration
                     (const_spec
                      name: (identifier) @name
                      (:equal @name ,symbol))) @const
                    (var_declaration
                     (var_spec
                      name: (identifier) @name
                      (:equal @name ,symbol))) @var))
                ('python
                 `((function_definition
                    name: (identifier) @name
                    (:equal @name ,symbol)) @function
                    (class_definition
                     name: (identifier) @name
                     (:equal @name ,symbol)) @class
                    (assignment
                     left: (identifier) @name
                     (:equal @name ,symbol)) @assignment))
                ('rust
                 `((function_item
                    name: (identifier) @name
                    (:equal @name ,symbol)) @function
                    (struct_item
                     name: (type_identifier) @name
                     (:equal @name ,symbol)) @struct
                    (impl_item
                     type: (type_identifier) @name
                     (:equal @name ,symbol)) @impl
                    (const_item
                     name: (identifier) @name
                     (:equal @name ,symbol)) @const
                    (let_declaration
                     pattern: (identifier) @name
                     (:equal @name ,symbol)) @let))))
             (query (treesit-query-compile language query-patterns))
             (node (cdar (treesit-query-capture root-node query))))
        (when node
          (buffer-substring-no-properties
           (treesit-node-start node)
           (treesit-node-end node)))))))

(defun gptel-projects-workspace--symbols-to-context-string (file-symbols-alist)
  "Convert FILE-SYMBOLS-ALIST to a context string.
FILE-SYMBOLS-ALIST is an alist of the form (filename . symbols-alist)."
  (with-temp-buffer
    (dolist (file-entry file-symbols-alist)
      (let ((file (car file-entry))
            (symbols (cdr file-entry)))
        (insert (format "\nFile: %s\n" file))
        (insert "Symbols:\n")
        (dolist (type '(class struct type impl function method var))
          (let ((type-symbols
                 (mapcar #'cdr
                         (cl-remove-if-not
                          (lambda (s) (eq (car s) type))
                          symbols))))
            (when type-symbols
              (insert (format "  %s: %s\n"
                              (capitalize (symbol-name type))
                              (string-join type-symbols ",  "))))))))
    (buffer-string)))

(defun gptel-projects-workspace-collect-code-info ()
  "Collect information about code files in the current project.
Returns a context string containing file paths and symbol information."
  (when (projectile-project-root)
    (let* ((all-files (projectile-project-files (projectile-project-root)))
           (code-files
            (seq-filter
             (lambda (f)
               (seq-some
                (lambda (ext) (string-suffix-p ext f))
                gptel-projects-workspace-code-file-extensions))
             all-files))
           (file-symbols nil))
      (message "Collecting symbols for files: %S" code-files)
      (when gptel-projects-workspace-collect-symbols
        (dolist (file code-files)
          (when-let ((symbols
                      (gptel-projects-workspace--extract-symbols
                       (expand-file-name file (projectile-project-root)))))
            (message "Extracted symbols from %s: %S" file symbols)
            (push (cons file symbols) file-symbols)))
        (when file-symbols
          (let ((context-str
                 (concat "\n=== Symbol Information ===\n"
                         (gptel-projects-workspace--symbols-to-context-string
                          file-symbols))))
            (message "Context string: %s" context-str)
            context-str))))))

(defun gptel-projects-workspace--add-code-info-to-context ()
  "Add code file and symbol information to the current gptel context."
  (message "Adding workspace context...")
  (message "Current gptel-context--alist: %S" gptel-context--alist)
  (when-let ((context-string (gptel-projects-workspace-collect-code-info)))
    (message "Context buffer exists: %s" (get-buffer " *gptel-workspace-context*"))
    (let ((buf (get-buffer-create " *gptel-workspace-context*")))
      (message "Before buffer mod - gptel-context--alist: %S" gptel-context--alist)
      (with-current-buffer buf
        (erase-buffer)
        (insert context-string)
        (setq buffer-undo-list t)
        (setq gptel-context--alist
              (assq-delete-all buf gptel-context--alist))
        (gptel-context--make-overlay (point-min) (point-max))))))

(defvar gptel-projects-workspace--watched-files nil
  "List of files being monitored for updates in the current project.
Each element is of the form (project-root . files) where files is
a list of absolute file paths being monitored for context updates.")

(defun gptel-projects-workspace--watch-file (file)
  "Set up save hook for FILE to update context."
  (when-let ((buf (find-buffer-visiting file)))
    (with-current-buffer buf
      (add-hook 'after-save-hook #'gptel-projects-workspace--after-save-hook nil t))))

(defun gptel-projects-workspace--after-save-hook ()
  "Hook run after saving a file that's part of the gptel context."
  (when (and (buffer-file-name) (member (buffer-file-name) gptel-projects-workspace--watched-files))
    (gptel-projects-workspace-update-code-info)))

(defun gptel-projects-workspace--start-watching-files ()
  "Start monitoring project files for changes."
  (when (projectile-project-root)
    (let* ((all-files (projectile-project-files (projectile-project-root)))
           (code-files
            (seq-filter
             (lambda (f)
               (seq-some
                (lambda (ext) (string-suffix-p ext f))
                gptel-projects-workspace-code-file-extensions))
             all-files)))
      (setq gptel-projects-workspace--watched-files
            (mapcar (lambda (f)
                      (expand-file-name f (projectile-project-root)))
                    code-files))
      (mapc #'gptel-projects-workspace--watch-file gptel-projects-workspace--watched-files))))

(defun gptel-projects-workspace--stop-watching-files ()
  "Stop monitoring project files for changes."
  (when gptel-projects-workspace--watched-files
    (dolist (file gptel-projects-workspace--watched-files)
      (when-let ((buf (find-buffer-visiting file)))
        (with-current-buffer buf
          (remove-hook 'after-save-hook
                       #'gptel-projects-workspace--after-save-hook t))))
    (setq gptel-projects-workspace--watched-files nil)))

(defun gptel-projects-workspace-update-code-info ()
  "Update the code information in the current gptel context."
  (interactive)
  (save-excursion
    ;; Only remove our workspace entries from gptel-context--alist
    (setq gptel-context--alist
          (cl-remove-if
           (lambda (entry)
             (let ((buf (car entry)))
               (and (buffer-live-p buf)
                    (string= " *gptel-workspace-context*"
                             (buffer-name buf)))))
           gptel-context--alist))

    ;; Kill old workspace buffer if it exists
    (when-let ((old-buf (get-buffer " *gptel-workspace-context*")))
      (kill-buffer old-buf))

    ;; Add new context
    (gptel-projects-workspace--add-code-info-to-context)
    (gptel-projects-workspace--start-watching-files)))

(defun gptel-projects-workspace--cleanup()
  "Clean up workspace resources."
  (when-let ((buf (get-buffer " *gptel-workspace-context")))
    (setq gptel-context--alist
          (assq-delete-all buf gptel-context--alist))
    (kill-buffer buf))
  (gptel-projects-workspace--stop-watching-files))

(defun gptel-projects-workspace--stop-watching-project ()
  "Stop monitoring files for current project if no more gptel buffers exist."
  (when-let* ((root (projectile-project-root))
              (project-buffers
               (cl-loop for buf in (buffer-list)
                        when (and (buffer-local-value 'gptel-projects-mode buf)
                                  (equal (with-current-buffer buf
                                           (projectile-project-root))
                                         root))
                        collect buf))
              ((= (length project-buffers) 1))
              ((eq (car project-buffers) (current-buffer))))
    (gptel-projects-workspace--stop-watching-files)))

(add-hook 'gptel-mode-hook
          (lambda ()
            (gptel-projects-workspace-update-code-info)))
(add-hook 'gptel-projects-mode-hook (lambda ()
                                      (if gptel-projects-mode
                                          (gptel-projects-workspace--start-watching-files)
                                        (gptel-projects-workspace--cleanup))))
(add-hook 'kill-buffer-hook #'gptel-projects-workspace--stop-watching-project)

(provide 'gptel-projects-workspace)
;;; gptel-projects-workspace.el ends here
