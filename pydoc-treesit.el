;;; pydoc-treesit.el --- Pydoc with Tree-sitter  -*- lexical-binding: t -*-

;; Copyright (C) 2015 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; Maintainer: Brian J. Lopes <statmobile@gmail.com>
;; Contributions from Kyle Meyer and Taro Sato <okomestudio@gmail.com>
;; Created: 30 July 2025
;; Version: 0.3
;; Keywords: pydoc, python
;; Homepage: https://github.com/statmobile/pydoc

;;; Commentary:

;; This module extends `pydoc' with `treesit' for Python object lookup.
;;
;; The `pydoc-treesit' module provides the following function:
;;
;; - `pydoc-treesit-at-point': Run this in a Python buffer to look up the object at point using `treesit'

;;; Changelog:

;; - July 2025: Create pydoc-treesit.el

;;; Code:

(require 'pydoc)

(defun pydoc-treesit--path-to-identifier (node)
  "Get the path to identifier at treesit NODE as a list.
The path is split by '.' as a delimiter."
  (let* ((p (treesit-node-parent node))
         (obj (treesit-node-child-by-field-name p "object"))
         (attr (treesit-node-child-by-field-name p "attribute"))
         (ident (treesit-node-text node)))
    (if (and p (equal (treesit-node-text attr) ident))
        (append (split-string (treesit-node-text obj) "\\.") (list ident))
      (list ident))))

(defun pydoc-treesit--package-root (&optional path)
  "Search upward along filesystem PATH for the Python package root directory.
When not given, PATH defaults to the value of function `buffer-file-name'."
  (let* ((dir (or path (file-name-directory (buffer-file-name))))
         (init-file (expand-file-name "__init__.py" dir)))
    (cond
     ((and (file-exists-p init-file)
           (not (let ((p (file-name-directory (directory-file-name dir))))
                  (and (not (equal p dir))
                       (file-exists-p (expand-file-name "__init__.py" p))))))
      ;; `dir' is at the top-level package.
      (directory-file-name dir))
     (t
      (let ((p (file-name-directory (directory-file-name dir))))
        (when (not (equal p dir))
          (pydoc-treesit--package-root p)))))))

(defun pydoc-treesit--this-module ()
  "Get the (unqualified) name of the Python module for the current buffer."
  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))

(defun pydoc-treesit--this-directory ()
  "Get the directory path containing the file visited by the current buffer."
  (directory-file-name (file-name-directory (buffer-file-name))))

(defun pydoc-treesit--fully-qualify-path (path)
  "Fully resolve the Python module PATH if given relatively.
If PATH is not relative, it is simply returned."
  (if-let* ((num-dots (if (string-match "\\`\\.+\\b" path)
                          (length (match-string 0 path))))
            (package-root (pydoc-treesit--package-root))
            (module-path-comps
             (seq-remove
              (lambda (it) (string= it ""))
              `(,(file-name-nondirectory (directory-file-name package-root))
                ,@(string-split
                   (string-remove-prefix package-root
                                         (pydoc-treesit--this-directory))
                   (regexp-opt (list "/" "\\"))))))
            (module-path (string-join (butlast module-path-comps (1- num-dots))
                                      ".")))
      (if (length= path num-dots)
          module-path
        (concat module-path "." (substring path num-dots)))
    path))

(defvar pydoc-treesit--import-query
  '((import_statement
     name: [(dotted_name (identifier) @name-1)
            (aliased_import
             name: (dotted_name (identifier) @name-1-with-alias)
             alias: (identifier) @alias-1)])
    (import_from_statement
     module_name: [(relative_import (_) @module-relative-2)
                   (dotted_name (_) @module-2)]
     name: [(dotted_name (identifier) @name-2)
            (aliased_import
             name: (dotted_name (identifier) @name-2-with-alias)
             alias: (identifier) @alias-2)]))
  "The `treesit' queries for Python import statements.")

(defun pydoc-treesit--partial-match-import-path (identifier &optional node)
  "Given an IDENTIFIER in NODE, return a matching import path.
IDENTIFIER may be a qualified name (i.e., dotted name). In that case, prefix
matching is performed against imported names. When match is found, the function
always returns the fully-qualified name to IDENTIFIER."
  (let* ((capture (cl-find-if
                   (lambda (c)
                     (and (equal (treesit-node-text (cdr c)) identifier)
                          (cdr c)))
                   (treesit-query-capture (treesit-buffer-root-node)
                                          pydoc-treesit--import-query)))
         (c (cdr capture)))
    (pcase (car capture)
      ('name-1
       (when-let* ((p (treesit-node-parent c)))
         (list (treesit-node-text p))))
      ('alias-1
       (when-let*
           ((p (treesit-node-parent c))
            (n (treesit-node-child-by-field-name p "name")))
         (list (treesit-node-text n))))
      ('name-1-with-alias
       (when-let*
           ((p (treesit-node-parent c))
            (q (treesit-node-parent p))
            (r (treesit-node-parent q))
            (on-import (equal "import_statement" (treesit-node-type r)))
            (l (string-split (treesit-node-text p) "\\.")))
         (take (1+ (cl-position (treesit-node-text c) l :test #'equal)) l)))
      ('name-2
       (when-let*
           ((p (treesit-node-parent c))
            (q (treesit-node-parent p))
            (m (treesit-node-child-by-field-name q "module_name")))
         (list (pydoc-treesit--fully-qualify-path (treesit-node-text m))
               (treesit-node-text c))))
      ('alias-2
       (when-let*
           ((p (treesit-node-parent c))
            (q (treesit-node-parent p))
            (n (treesit-node-child-by-field-name p "name"))
            (m (treesit-node-child-by-field-name q "module_name")))
         (list (pydoc-treesit--fully-qualify-path (treesit-node-text m))
               (treesit-node-text n))))
      ('name-2-with-alias
       (when-let*
           ((p (treesit-node-parent c))
            (q (treesit-node-parent p))
            (r (treesit-node-parent q))
            (on-import (equal "import_from_statement" (treesit-node-type r)))
            (n (treesit-node-child-by-field-name q "name"))
            (m (treesit-node-child-by-field-name r "module_name")))
         (list (pydoc-treesit--fully-qualify-path (treesit-node-text m))
               (treesit-node-text n))))
      ('module-2
       (when-let*
           ((p (treesit-node-parent c))
            (q (treesit-node-parent p))
            (on-import (equal "import_from_statement" (treesit-node-type q)))
            (l (string-split (treesit-node-text p) "\\.")))
         (take (1+ (cl-position (treesit-node-text c) l :test #'equal)) l)))
      ('module-relative-2
       (when-let*
           ((p (treesit-node-parent c))
            (on-import (equal "relative_import" (treesit-node-type p)))
            (l (string-split
                (pydoc-treesit--fully-qualify-path (treesit-node-text p))
                "\\.")))
         (take (1+ (cl-position (treesit-node-text c) l :test #'equal)) l))))))

(defun pydoc-treesit--resolve-to-full-path (node)
  "Resolve treesit NODE to its full path that `pydoc' accepts as input."
  (cond
   ((equal (treesit-node-type node) "identifier")
    (let* ((parts (pydoc-treesit--path-to-identifier node))
           (num-parts (length parts))
           (len num-parts)
           matched)
      (while (not (or matched (eq len 0)))
        (setq matched (pydoc-treesit--partial-match-import-path
                       (string-join (take len parts) ".")
                       node))
        (when matched
          (setq matched (append matched (last parts (- num-parts len)))))
        (setq len (1- len)))
      (string-join (cond
                    ((not (null matched))
                     matched)
                    ((and (eq (length parts) 1)
                          (member (car parts) (pydoc-builtin-objects)))
                     parts)
                    (t
                     (append (list (pydoc-treesit--fully-qualify-path ".")
                                   (pydoc-treesit--this-module))
                             parts)))
                   ".")))

   ((and (equal (treesit-node-type node) ".")
         (equal "import_prefix" (treesit-node-type (treesit-node-parent node))))
    (pydoc-treesit--fully-qualify-path (treesit-node-text (treesit-node-parent node))))

   ;; the non-identifier node is likely a keyword here
   (t (treesit-node-text node))))

;;;###autoload
(defun pydoc-treesit-at-point ()
  "Get help for a Python object at point using `treesit'."
  (interactive)
	(if-let* ((node (treesit-node-at (point)))
            (path (pydoc-treesit--resolve-to-full-path node)))
      (pydoc (shell-quote-argument path))
    (message "Object not found at point")))

(provide 'pydoc-treesit)
;;; pydoc-treesit.el ends here
