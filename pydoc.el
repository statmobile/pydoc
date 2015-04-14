;;; pydoc.el --- functional, syntax highlighted pydoc navigation

;; Copyright (C) 2015 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Maintainer: Brian J. Lopes <statmobile@gmail.com>
;; Created: 8 Mar 2015
;; Version: 0.1
;; Keywords: pydoc, python
;; Homepage: https://github.com/statmobile/pydoc

;;; Commentary:

;; This module runs pydoc on an argument, inserts the output into a
;; buffer, and then linkifies and colorizes the buffer. For example,
;; some things are linked to open the source code, or to run pydoc on
;; them. Some things are colorized for readability, e.g. environment
;; variables and strings, function names and arguments.
;;
;; https://github.com/statmobile/pydoc
;;
;; There is one command. M-x pydoc

;;; Changelog:
;;
;; Updated license and headers for release.


;;; Code:

(require 'cl-lib)
(require 'goto-addr)
(require 'help-mode)
;; we use org-mode for python fontification
(require 'org)


;;; Options

(defgroup pydoc nil
  "Help buffer for pydoc."
  :prefix "pydoc"
  :group 'external
  :group 'help)

(defcustom pydoc-command "python -m pydoc"
  "The command to use to run pydoc."
  :type 'string
  :group 'pydoc)

(defcustom pydoc-make-method-buttons t
  "If non-nil, create buttons for methods."
  :type 'boolean
  :group 'pydoc)

(defcustom pydoc-after-finish-hook nil
  "Hook run by after pydoc buffer is prepared."
  :type 'hook
  :group 'pydoc)

(defface pydoc-example-leader-face
  '((t (:inherit font-lock-doc-face)))
  "Face used to highlight code example leader (e.g., \">>>\").")


;;; Buttons

(define-button-type 'pydoc-source
  :supertype 'help-xref
  'help-function 'find-file-other-window
  'help-echo (purecopy "mouse-2, RET: visit file"))

(define-button-type 'pydoc-source-search
  :supertype 'help-xref
  'help-function (lambda (file search)
                   (find-file-other-window file)
                   (goto-char (point-min))
                   (re-search-forward search nil t)
                   (beginning-of-line))
  'help-echo (purecopy "mouse-2, RET: view in source"))

(define-button-type 'pydoc-help
  :supertype 'help-xref
  'help-function (lambda (pkg) (pydoc pkg))
  'help-echo (purecopy "mouse-2, RET: view pydoc help"))


;;; Buffer information

(defconst pydoc-sections-re
  (rx line-start
      (group
       (one-or-more (any upper))
       (zero-or-more (and (any space) (one-or-more (any upper)))))
      line-end)
  "Regular expression matching top-level pydoc sections.")

(defvar pydoc-file nil
  "File associated with the current pydoc buffer.
The help for modules and packages have a \"FILE\" section (unless
they are built-in module like `sys`).")
(put 'pydoc-file 'permanent-local t)
(make-variable-buffer-local 'pydoc-file)

(defvar pydoc-info nil
  "Plist with information about the current pydoc buffer.

Keys include

  type
    The type of object.  This will always be non-nil.  Possible values
    are

      py-package
      py-module

      py-function
      py-class

      py-topic-list   (from \"pydoc topics\")
      py-keyword-list (from \"pydoc keywords\")
      py-module-list  (from \"pydoc modules\")

      py-topic
      py-keyword

      not-found
      unknown

  name
    The name of the object for the current help buffer.  This will be
    nil for help on topics as well as topic, keyword, and modules
    lists.

  in
    The name object, if any, that conains the object display in the
    current help buffer.

  sections
    An alist of section names and positions (if the object has
    sections).")
(put 'pydoc-info'permanent-local t)
(make-variable-buffer-local 'pydoc-info)

(defun pydoc-set-info ()
  "Set up `pydoc-info'for the current pydoc buffer."
  (setq pydoc-info (pydoc-get-info))
  (setq pydoc-info
        (plist-put pydoc-info
                   :sections (pydoc-get-sections))))

(defun pydoc-get-info ()
  "Return help name and type for the current pydoc buffer.

Return a plist with the keywords :name, :type, and :in.  All
return values will have a :type property.

See `pydoc-info' for more details on the keys."
  (save-excursion
    (goto-char (point-min))
    (cond
     ((looking-at "Help on package \\(.+\\) in \\(.+\\):")
      (list :name (match-string-no-properties 1) :type 'py-package
            :in (match-string-no-properties 2)))
     ((looking-at "Help on package \\(.+\\):")
      (list :name (match-string-no-properties 1) :type 'py-package))
     ((looking-at "Help on \\(?:built-in \\)?module \\(.+\\) in \\(.+\\):")
      (list :name (match-string-no-properties 1) :type 'py-module
            :in (match-string-no-properties 2)))
     ((looking-at "Help on \\(?:built-in \\)?module \\(.+\\):")
      (list :name (match-string-no-properties 1) :type 'py-module))
     ((looking-at (concat "Help on \\(?:built-in \\)?function \\(.+\\)"
                          " in \\(?:module \\)?\\(.+\\):"))
      (list :name (match-string-no-properties 1) :type 'py-function
            :in (match-string-no-properties 2)))
     ((looking-at "Help on class \\(.+\\) in \\(.+\\):")
      (list :name (match-string-no-properties 1) :type 'py-class
            :in (match-string-no-properties 2)))
     ((looking-at "The \"\\(.+\\)\" statement")
      (list :name (match-string-no-properties 1) :type 'py-keyword))
     ((looking-at "no Python documentation found for")
      (list :type 'not-found))
     ((looking-at "\\w+.*\n\\*+")
      (list :type 'py-topic))
     ((looking-at (concat "\nHere is a list of available topics."
                          "  Enter any topic name to get more help.$"))
      (list :type 'py-topic-list :start (match-end 0)))
     ((looking-at (concat "\nHere is a list of the Python keywords."
                          "  Enter any keyword to get more help.$"))
      (list :type 'py-keyword-list :start (match-end 0)))
     ;; This should be the last branch before t because it doesn't
     ;; restore point back to the beginning of the buffer.
     ((re-search-forward
       "Please wait a moment while I gather a list of all available modules...$"
       nil t)
      ;; ^ This may not be at a predictable line due to import
      ;; messages, so search for it.
      (let ((start (point)))
        (re-search-forward "Enter any module name to get more help.")
        (list :type 'py-module-list :start start :end (match-beginning 0))))
     (t
      (list :type 'unknown)))))

(defun pydoc-get-sections ()
  "Return sections of the current pydoc buffer.
An alist of (section . position) cells is returned, where
\"section\" the lower case version of the section title."
  (save-excursion
    (goto-char (point-min))
    (let (case-fold-search
          sections name start next-start)
      (while (re-search-forward pydoc-sections-re nil t)
        (setq next-start (match-beginning 0))
        (when name
          (push (cons name (cons start (1- next-start)))
                sections))
        (setq name (downcase (match-string-no-properties 1))
              start next-start))
      (when name
        (push (cons name (cons start (point-max)))
              sections))
      sections)))

(defun pydoc--section-start (section)
  "Return start position for SECTION.
Value is obtained from buffer-local `pydoc-info'."
  (car (cdr (assoc section (plist-get pydoc-info :sections)))))

(defmacro pydoc--with-section (section regexp &rest body)
  "Perform REGEXP search within SECTION.
Execute BODY for each sucessful search."
  (declare (indent 2))
  `(let* ((section-pos (cdr (assoc ,section (plist-get pydoc-info :sections))))
          (case-fold-search nil))
     (when section-pos
       (save-excursion
         (goto-char (car section-pos))
         (while (re-search-forward ,regexp (cdr section-pos) t)
           ,@body)))))

;; This is the pydoc version of `help-make-xrefs'.
(defun pydoc-make-xrefs (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (let ((old-modified (buffer-modified-p))
            (case-fold-search nil)
            (inhibit-read-only t))
        (cl-case (plist-get pydoc-info :type)
          ((not-found py-function py-class))
          ((py-topic py-keyword)
           (pydoc--buttonize-related-topics))
          ((py-keyword-list py-topic-list py-module-list)
           (goto-char (plist-get pydoc-info :start))
           (pydoc--buttonize-help-list (plist-get pydoc-info :end)))
          (py-module
           (pydoc--buttonize-file)
           (when pydoc-file
             (pydoc--buttonize-functions pydoc-file)
             (pydoc--buttonize-classes pydoc-file)
             (when pydoc-make-method-buttons
               (pydoc--buttonize-methods pydoc-file))
             (pydoc--buttonize-data pydoc-file)))
          (py-package
           (pydoc--buttonize-package-contents (plist-get pydoc-info :name))
           (pydoc--buttonize-file)
           (when pydoc-file
             (pydoc--buttonize-functions pydoc-file)
             (pydoc--buttonize-classes pydoc-file)
             (when pydoc-make-method-buttons
               (pydoc--buttonize-methods pydoc-file))
             (pydoc--buttonize-data pydoc-file))))
        (pydoc--buttonize-urls)
        (pydoc--buttonize-sphinx)
        ;; Delete extraneous newlines at the end of the docstring
        (goto-char (point-max))
        (while (and (not (bobp)) (bolp))
          (delete-char -1))
        (insert "\n")
        (pydoc--insert-navigation-links)
        (set-buffer-modified-p old-modified)))))

(defun pydoc--buttonize-help-list (&optional limit)
  (save-excursion
    (while (re-search-forward "\\b\\w+\\b" limit t)
      (help-xref-button 0 'pydoc-help (match-string 0)))))

(defun pydoc--buttonize-related-topics ()
  (save-excursion
    (when (re-search-forward "^Related help topics: \\(\\w+\\)"
                             nil t)
      (help-xref-button 1 'pydoc-help (match-string 1))
      (let ((line-end (point-at-eol)))
        (while (re-search-forward ",\\s-*\\(\\w+\\)" line-end t)
          (help-xref-button 1 'pydoc-help (match-string 1)))))))

;; This is taken from `help-make-xrefs'.
(defun pydoc--insert-navigation-links ()
  (when (or help-xref-stack help-xref-forward-stack)
    (insert "\n"))
  ;; Make a back-reference in this buffer if appropriate.
  (when help-xref-stack
    (help-insert-xref-button help-back-label 'help-back
                             (current-buffer)))
  ;; Make a forward-reference in this buffer if appropriate.
  (when help-xref-forward-stack
    (when help-xref-stack
      (insert "\t"))
    (help-insert-xref-button help-forward-label 'help-forward
                             (current-buffer)))
  (when (or help-xref-stack help-xref-forward-stack)
    (insert "\n")))

(defun pydoc--buttonize-file ()
  (let ((file-pos (pydoc--section-start "file")))
    (when file-pos
      (save-excursion
        (goto-char file-pos)
        (looking-at "^FILE\n    \\(.+\\)$")
        (let ((file (match-string-no-properties 1)))
          (when (file-exists-p file)    ; This may be "(built-in)".
            (setq pydoc-file file)
            (help-xref-button 1 'pydoc-source pydoc-file)))))))

(defun pydoc--buttonize-urls ()
  (save-excursion
    (while (re-search-forward goto-address-url-regexp nil t)
      (help-xref-button 0 'help-url (match-string 0)))))

(defun pydoc--buttonize-functions (file)
  (pydoc--with-section "functions"
      "^\\s-+\\([_a-zA-z0-9]+\\)("
    (let* ((func (match-string 1))
           (search (format "def %s(" func)))
      (help-xref-button 1 'pydoc-source-search file search))))

(defun pydoc--buttonize-classes (file)
  (pydoc--with-section "classes"
      "    class \\([_A-z0-9]+\\)\\(?:(\\(.*\\))\\)*"
    (let* ((class (match-string 1))
           (search (format "^class %s\\b" class))
           ;; TODO Sometimes this doesn't have full path.
           (superclass (match-string 2)))
      (help-xref-button 1 'pydoc-source-search file search)
      (when superclass
        (help-xref-button 2 'pydoc-help superclass)))))

(defun pydoc--buttonize-methods (file)
  (pydoc--with-section "classes"
      "^     |  \\([a-zA-Z0-9_]*\\)(\\(.*\\))$"
    ;; TODO This is not specific for the class it is under.
    (let* ((meth (match-string 1))
           (search (format "def %s(" meth)))
      (help-xref-button 1 'pydoc-source-search file search))))

(defun pydoc--buttonize-data (file)
  (pydoc--with-section "data"
      "^    \\([_A-Za-z0-9]+\\) ="
    (help-xref-button 1 'pydoc-source-search file (match-string 1))))

(defun pydoc--buttonize-sphinx ()
  (save-excursion
    ;; TODO Add method?
    (while (re-search-forward ":\\(class\\|func\\|mod\\):`~?\\([^`]*\\)`" nil t)
      (let ((name (match-string 2)))
        (help-xref-button 2 'pydoc-help name)))))

(defun pydoc--buttonize-package-contents (pkg-name)
  (pydoc--with-section "package contents"
      "^    \\([a-zA-Z0-9_-]*\\)[ ]?\\((package)\\)?$"
    (let ((package (concat pkg-name "." (match-string 1))))
      (help-xref-button 1 'pydoc-help package))))


;;; Mode

(defconst pydoc-example-code-leader-re
  (rx line-start
      (zero-or-one " |")                ; Within a class
      (zero-or-more space)
      (group (or ">>>" "..."
                 (and "In [" (one-or-more digit) "]:")))
      " "
      (group (one-or-more not-newline))
      line-end)
  "Regular expression matching leader for Python code snippet.
This will be use to highlight line with Python syntax
highlightling.")

(defun pydoc-fontify-inline-code (limit)
  "Fontify example blocks up to LIMIT.
These are lines marked by `pydoc-example-code-leader-re'."
  (when (re-search-forward pydoc-example-code-leader-re limit t)
    (set-text-properties (match-beginning 1) (match-end 1)
                         '(font-lock-face pydoc-example-leader-face))
    (org-src-font-lock-fontify-block
     "python" (match-beginning 2) (match-end 2))
    t))

(defvar pydoc-font-lock-keywords
  `((pydoc-fontify-inline-code)
    (,pydoc-sections-re 0 'bold)
    ("\\$[A-z0-9_]+" 0 font-lock-builtin-face)
    ("``.+?``" 0 font-lock-builtin-face)
    ("`.+?`" 0 font-lock-builtin-face)
    ("\".+?\"" 0 font-lock-string-face)
    ("'.+?'" 0 font-lock-string-face)
    (,(regexp-opt (list "True" "False" "None") 'words)
     1 font-lock-constant-face)))

(defvar pydoc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'next-line)
    (define-key map "N" 'forward-page)
    (define-key map "p" 'previous-line)
    (define-key map "P" 'backward-page)
    (define-key map "f" 'forward-char)
    (define-key map "b" 'backward-char)
    (define-key map "F" 'forward-word)
    (define-key map "B" 'backward-word)
    (define-key map "o" 'occur)
    (define-key map "s" 'isearch-forward)
    map)
  "Keymap for Pydoc mode.")

;;;###autoload
(define-derived-mode pydoc-mode help-mode "Pydoc"
  "Major mode for viewing pydoc output.
Commands:
\\{pydoc-mode-map}"
  (set (make-local-variable 'font-lock-defaults)
       '((pydoc-font-lock-keywords) t nil))
  :keymap pydoc-mode-map)

(defun pydoc-mode-setup ()
  (pydoc-mode)
  (setq buffer-read-only nil))

(defun pydoc-mode-finish ()
  (when (derived-mode-p 'pydoc-mode)
    (setq buffer-read-only t)
    (pydoc-set-info)
    (pydoc-make-xrefs (current-buffer))
    (run-hooks 'pydoc-after-finish-hook)))

(defmacro pydoc-with-help-window (buffer-name &rest body)
  "Display buffer named BUFFER-NAME in a pydoc help window.
This is the same as `with-help-window', except `pydoc-mode-setup'
and `pydoc-mode-finish' are used instead of `help-mode-setup' and
`help-mode-finish'."
  (declare (indent 1) (debug t))
  `(progn
     ;; Make `help-window-point-marker' point nowhere.  The only place
     ;; where this should be set to a buffer position is within BODY.
     (set-marker help-window-point-marker nil)
     (let ((temp-buffer-window-setup-hook
            (cons 'pydoc-mode-setup temp-buffer-window-setup-hook))
           (temp-buffer-window-show-hook
            (cons 'pydoc-mode-finish temp-buffer-window-show-hook)))
       (with-temp-buffer-window
        ,buffer-name nil 'help-window-setup (progn ,@body)))))

(defun pydoc-buffer ()
  "Like `help-buffer', but for pydoc help buffers."
  (buffer-name
   (if (not help-xref-following)
       (get-buffer-create "*pydoc*")
     (unless (derived-mode-p 'help-mode)
       (error "Current buffer is not in Pydoc mode"))
     (current-buffer))))

(defun pydoc-setup-xref (item interactive-p)
  "Like `help-setup-xref', but for pydoc help buffers."
  (with-current-buffer (pydoc-buffer)
    (when help-xref-stack-item
      (push (cons (point) help-xref-stack-item) help-xref-stack)
      (setq help-xref-forward-stack nil))
    (when interactive-p
      (let ((tail (nthcdr 10 help-xref-stack)))
        ;; Truncate the stack.
        (if tail (setcdr tail nil))))
    (setq help-xref-stack-item item)))

;;;###autoload
(defun pydoc (name)
  "Display pydoc information for NAME in `pydoc-buffer'."
  (interactive "sName of function or module: ")
  (pydoc-setup-xref (list #'pydoc name)
                    (called-interactively-p 'interactive))
  (pydoc-with-help-window (pydoc-buffer)
    (call-process-shell-command (concat pydoc-command " " name)
                                nil standard-output)))

(provide 'pydoc)

;;; pydoc.el ends here
