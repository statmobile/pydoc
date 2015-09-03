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

(require 'info-look)

(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_.]+"
 :doc-spec
 '(("(python)Index" nil "")))

;;; Code:
;; we use org-mode for python fontification
(require 'org)

(defvar *pydoc-current* nil
 "Stores current pydoc command.")


(defvar *pydoc-last* nil
 "Stores the last pydoc command.")


(defvar *pydoc-history* '()
  "History for pydoc commands.")

(defvar *pydoc-index* 0
  "Current index in the history.")

(defvar pydoc-file nil)
(defvar pydoc-name nil)

;;; faces

(defface pydoc-source-file-link-face
  '((t  (:inherit link)))
  "Link to a file."
  :group 'pydoc-faces)

(defface pydoc-package-link-face
  '((t  (:inherit link)))
  "Link to a package."
  :group 'pydoc-faces)

(defface pydoc-mouse-face
  '((t (:inherit highlight)))
  "Mouse over face."
  :group 'pydoc-faces)

(defface pydoc-class-name-link-face
  '((t (:inherit link)))
  "Class name"
  :group 'pydoc-faces)

(defface pydoc-superclass-name-link-face
  '((t (:inherit link)))
  "Superclass name"
  :group 'pydoc-faces)

(defface pydoc-callable-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Method or function face."
  :group 'pydoc-faces)

(defface pydoc-callable-param-face
  '((t (:foreground "red")))
  "Method or function arguments face. Also used in sphinx
documentation."
  :group 'pydoc-faces)

(defface pydoc-envvars-face
  '((t (:inherit font-lock-variable-name-face)))
  "Environment variables face."
  :group 'pydoc-faces)

(defface pydoc-data-face
  '((t (:inherit font-lock-variable-name-face)))
  "DATA face."
  :group 'pydoc-faces)

(defface pydoc-string-face
  '((t (:inherit font-lock-string-face)))
  "String face."
  :group 'pydoc-faces)

(defface pydoc-button-face
  '((t (:inherit button)))
  "String face."
  :group 'pydoc-faces)

(defface pydoc-sphinx-directive-face
  '((t (:foreground "SteelBlue4" :underline t)))
  "Sphinx directive face."
  :group 'pydoc-faces)

(defface pydoc-sphinx-param-name-face
  '((t (:foreground "red")))
  "Sphinx callable parameter name face."
  :group 'pydoc-faces)

(defface pydoc-sphinx-param-type-face
  '((t (:foreground "DeepSkyBlue3")))
  "Sphinx callable parameter type face."
  :group 'pydoc-faces)

;;; en faces

(defun pydoc-get-name ()
  "Get NAME and store locally."
  (goto-char (point-min))
  ;; the name sometimes is just a word, sometimes there is a - with a string
  ;; after it.
  (when (re-search-forward "^NAME
    \\([a-zA-Z0-9_]*\\(\\..*\\)?\\)\\( -\\)?"
			   nil t)
    (setq pydoc-name (match-string 1))))


(defun pydoc-make-file-link ()
  "Find FILE in a pydoc buffer and make it a clickable link that
opens the file."
  (goto-char (point-min))
  (when (re-search-forward "^FILE
    \\(.*\\)$" nil t)

    (setq pydoc-file (match-string 1))

    (let ((map (make-sparse-keymap))
	  (start (match-beginning 1))
	  (end (match-end 1)))

      ;; set file to be clickable to open the source
      (define-key map [mouse-1]
	`(lambda ()
	  (interactive)
	  (find-file ,pydoc-file)
	  (goto-char (point-min))))

      (set-text-properties
       start end
       `(local-map, map
		   face pydoc-source-file-link-face
		   mouse-face pydoc-mouse-face
		   help-echo "mouse-1: click to open")))))


(defun pydoc-make-package-links ()
  "Make links in PACKAGE CONTENTS."
  (goto-char (point-min))
  (when (re-search-forward "^PACKAGE CONTENTS" nil t)
    (forward-line)

    (while (string-match
	    "^    \\([a-zA-Z0-9_-]*\\)[ ]?\\((package)\\)?"
	    (buffer-substring
	     (line-beginning-position)
	     (line-end-position)))

      (let ((map (make-sparse-keymap))
	    (start (match-beginning 1))
	    (end (match-end 1))
	    (package (concat
		      pydoc-name "."
		      (match-string 1
				    (buffer-substring
				     (line-beginning-position)
				     (line-end-position))))))

	(define-key map [mouse-1]
	  `(lambda ()
	    (interactive)
	    (pydoc ,package)))

	(set-text-properties
	 (+ (line-beginning-position) start)
	 (+ (line-beginning-position) end)
	 `(local-map, map
		      face pydoc-package-link-face
		      mouse-face pydoc-mouse-face
		      help-echo (format "mouse-1: click to open %s" ,package))))
      (forward-line))))


(defun pydoc-colorize-class-methods ()
  "Colorize and linkify class methods.
These tend to be something like:

   | function_name(args)"
  (goto-char (point-min))
  ;; group1 is the method, group2 is the args
  (while (re-search-forward "^\\s-+|  \\([a-zA-Z0-9_]*\\)(\\(.*\\))" nil t)

    (let ((map (make-sparse-keymap))
	  (start (match-beginning 1))
	  (end (match-end 1))
	  (function (match-string 1)))

      (define-key map [mouse-1]
	`(lambda ()
	   (interactive)
	   (find-file ,pydoc-file)
	   (goto-char (point-min))
	   (re-search-forward
	    ;; fragile if spacing is not right
	    (format "def %s(" ,function nil t))))

      (set-text-properties
       start end
       `(local-map, map
		    face pydoc-callable-name-face
		    mouse-face pydoc-mouse-face
		    help-echo (format "mouse-1: click to open %s" ,function)))

      (set-text-properties
       (match-beginning 2)
       (match-end 2)
       '(face pydoc-callable-param-face)))))


(defun pydoc-colorize-functions ()
  "Change color of function names and args.
Also, make function names clickable so they open the source file
at the function definition.

These are in a special section called Functions."
  (goto-char (point-min))
  (when (re-search-forward "^Functions" nil t)
    ;; we use this regexp to find functions "    name(args)"
    (while (re-search-forward "    \\([a-zA-z0-9-]+\\)(\\([^)]*\\))" nil t)

      (let ((map (make-sparse-keymap))
	    (start (match-beginning 1))
	    (end (match-end 1))
	    (function (match-string 1)))

	(define-key map [mouse-1]
	  `(lambda ()
	     (interactive)
	     (find-file ,pydoc-file)
	     (goto-char (point-min))
	     (re-search-forward
	      (format "def %s(" ,function nil t))))

	(set-text-properties
	 start end
	 `(local-map, map
		     face pydoc-callable-name-face
		     mouse-face pydoc-mouse-face
		     help-echo (format "mouse-1: click to open %s" ,function)))

	(set-text-properties
	 (match-beginning 2)
	 (match-end 2)
	 '(face pydoc-callable-param-face))))))


(defun pydoc-colorize-envvars ()
  "Makes environment variables a green color."
  (goto-char (point-min))
  (while (re-search-forward "\\$[a-zA-Z_]*\\>" nil t)
    (set-text-properties
     (match-beginning 0)
     (match-end 0)
     '(face pydoc-envvars-face))))


(defun pydoc-colorize-strings ()
  "Make strings in single ' or \" a green color.
This is not very robust, e.g. it fails if quotes cross lines, or if they are used in mathematics."
  (goto-char (point-min))
  (while (re-search-forward
	  "\\('\\|\\\"\\)[^'\"|
]*\\('\\|\\\"\\)"
		  nil t)
    (set-text-properties
     (match-beginning 0)
     (match-end 0)
     '(face pydoc-string-face))))


(defun pydoc-linkify-sphinx-directives ()
  "Make sphinx directives into clickable links.

class, func and mod directive links will run pydoc on the link contents.

we just colorize parameters in red."

  (goto-char (point-min))
  (while (re-search-forward ":\\(class\\|func\\|mod\\):`\\([^`]*\\)`" nil t)
    (let ((map (make-sparse-keymap)))
      ;; we run pydoc on the func
      (define-key map [mouse-1]
	`(lambda ()
	  (interactive)
	  (pydoc ,(match-string 2))))

      (set-text-properties
       (match-beginning 2)
       (match-end 2)
       `(local-map, map
		    face pydoc-sphinx-directive-face
		    mouse-face pydoc-mouse-face
		    help-echo
		    (format "mouse-1: pydoc %s" ,(match-string 1))))))

  (goto-char (point-min))
  ;; param, parameter, arg, argument, key, keyword
  (while (re-search-forward
	  (concat
	   ":\\(param\\|parameter\\|arg\\|argument\\|key\\|keyword\\):"
	   "`\\([^`]*\\)`")
	  nil t)
    (set-text-properties
     (match-beginning 2)
     (match-end 2)
     '(face pydoc-sphinx-directive-face)))

  ;; :param type name:
  (goto-char (point-min))
  (while (re-search-forward
	  ":param\\s-*\\([^: ]*\\)\\s-*\\([^:]*\\):"
	  nil t)

    (cond
     ;; neither present
     ((and (string= "" (match-string 1))
	   (string= "" (match-string 2)))
      ;; pass
      )
     ;; no type and one arg.
     ((and (not (string= "" (match-string 1)))
	   (string= "" (match-string 2)))
      (set-text-properties
       (match-beginning 1)
       (match-end 1)
       '(face pydoc-sphinx-param-name-face)))
     ;; both type and arg
     (t
      ;; optional type
      (set-text-properties
       (match-beginning 1)
       (match-end 1)
       '(face pydoc-sphinx-param-type-face))

      ;; arg
      (set-text-properties
       (match-beginning 2)
       (match-end 2)
       '(face pydoc-sphinx-param-name-face))))))


(defun pydoc-fontify-inline-code ()
  "fontify lines with >>> in them, which are inline python."
  (goto-char (point-min))
  (while (re-search-forward "\\(\\.\\.\\.\\|>>>\\)" nil t)
    (org-src-font-lock-fontify-block
     "python"
     (line-beginning-position)
     (line-end-position))))


(defun pydoc-linkify-classes ()
  "Find class lines, and colorize and linkify them."
  (goto-char (point-min))
  ;; first match is class name, second match is optional super class
  (while (re-search-forward "^\\s-+class \\(.*\\)(?\\(.*\\)?)?" nil t)
    ;; colorize the class
    (let ((map (make-sparse-keymap)))

      ;; set file to be clickable to open the source
      (define-key map [mouse-1]
	`(lambda ()
	   (interactive)
	   (find-file ,pydoc-file)
	   (goto-char (point-min))
	   ;; this might be fragile if people put other spaces in
	   (re-search-forward (format "^class %s\\b"  ,(match-string 1)))))

      (set-text-properties
       (match-beginning 1)
       (match-end 1)
       `(local-map, map
		    face pydoc-class-name-link-face
		    mouse-face pydoc-mouse-face
		    help-echo "mouse-1: click to open")))

    ;; colorize and link superclass
    (let ((map (make-sparse-keymap)))

      ;; we run pydoc on the superclass
      (define-key map [mouse-1]
	`(lambda ()
	  (interactive)
	  (pydoc ,(match-string 2))))

      (set-text-properties
       (match-beginning 2)
       (match-end 2)
       `(local-map, map
		    face pydoc-superclass-name-link-face
		    mouse-face pydoc-mouse-face
		    help-echo
		    (format "mouse-1: pydoc %s" ,(match-string 2)))))))


(defun pydoc-linkify-data ()
  "Find DATA block and then make links to entries.
This is not perfect, as the data entries are not always in the file defined, e.g. when it is an __init__ file that imports *."
  (goto-char (point-min))
  (when (re-search-forward "^DATA" nil t)
    (while (re-search-forward "\\([_A-Za-z0-9]*\\) =" nil t)
      (let ((map (make-sparse-keymap))
	    (start (match-beginning 1))
	    (end (match-end 1))
	    (token (match-string 1)))

	(define-key map [mouse-1]
	  `(lambda ()
	     (interactive)
	     (find-file ,pydoc-file)
	     (goto-char (point-min))
	     (re-search-forward
	      (format "^%s" ,token nil t))))

	(set-text-properties
	 start end
	 `(local-map, map
		      face pydoc-data-face
		      mouse-face pydoc-mouse-face
		      help-echo (format "mouse-1: click to go to %s" ,token)))))))


(defun pydoc-insert-back-link ()
  "Insert link to next and previous pydoc buffers."
  (goto-char (point-max))
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      (lambda ()
	(interactive)
	(setq *pydoc-index* (mod (- *pydoc-index* 1) (length *pydoc-history*)))
	(pydoc (elt *pydoc-history* *pydoc-index*))))
    (insert
     (propertize "[Back]"
		 'local-map map
		 'face 'pydoc-button-face
		 'mouse-face 'pydoc-mouse-face
		 'help-echo "mouse-1: click to return")))

  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      (lambda ()
	(interactive)
	(setq *pydoc-index* (mod (+ *pydoc-index* 1) (length *pydoc-history*)))
	(pydoc (elt *pydoc-history* *pydoc-index*))))
    (insert
     (concat
      "  "
      (propertize "[Forward]"
		  'local-map map
		  'face 'pydoc-button-face
		  'mouse-face 'pydoc-mouse-face
		  'help-echo "mouse-1: click to return")))))



;;;###autoload
(defun pydoc ()
  "Display pydoc information for NAME in a buffer named *pydoc*."
  (interactive)
  (let* ((info-lookup-mode 'python-mode)
         (default (info-lookup-guess-default 'symbol info-lookup-mode))
         (name (read-string "Name of function or module: " default 'python-lookup-history 0 nil)))
    (switch-to-buffer-other-window "*pydoc*")
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (shell-command-to-string (format "python -m pydoc %s" name)))
    (goto-char (point-min))
  
    ;; store name at end of history if it is not in the history
    ;; already. This isn't exactly a real history this way, since it
    ;; won't add multiple instances, and revisiting a NAME will move you
    ;; around in the history.
    (add-to-list '*pydoc-history* name t)
  
  
    ;; save
    (when *pydoc-current*
        (setq *pydoc-last* *pydoc-current*))
    (setq *pydoc-current* name)
  
    (make-local-variable 'pydoc-file)
    (make-local-variable 'pydoc-name)
  
    (save-excursion
      (pydoc-get-name)
      (goto-address-mode)
      (pydoc-make-file-link)
      (pydoc-make-package-links)
      (pydoc-linkify-classes)
      (pydoc-colorize-functions)
      (pydoc-colorize-class-methods)
      (pydoc-colorize-envvars)
      (pydoc-colorize-strings)
      (pydoc-linkify-sphinx-directives)
      (pydoc-fontify-inline-code)
      (pydoc-linkify-data)
      (pydoc-insert-back-link))
  
    ;; make read-only and press q to quit. add some navigation keys
    (setq buffer-read-only t)
    (use-local-map (copy-keymap text-mode-map))
    (local-set-key "q" #'(lambda () (interactive) (quit-window t)))
    (local-set-key "n" #'next-line)
    (local-set-key "N" #'forward-page)
    (local-set-key "p" #'previous-line)
    (local-set-key "P" #'backward-page)
    (local-set-key "f" #'forward-char)
    (local-set-key "b" #'backward-char)
    (local-set-key "F" #'forward-word)
    (local-set-key "B" #'backward-word)
    (local-set-key "o" #'(lambda () (interactive) (call-interactively 'occur)))
    (local-set-key "s" #'isearch-forward)
    (font-lock-mode)))

(provide 'pydoc)

;;; pydoc.el ends here
