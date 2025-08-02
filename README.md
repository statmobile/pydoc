# Pydoc: Emacs utility for navigating Python documentation through pydoc
[![MELPA](http://melpa.org/packages/pydoc-badge.svg)](http://melpa.org/#/pydoc)

## Installation

To install `pydoc` via Melpa, have the following lines in Emacs's `init.el`:

``` emacs-lisp
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package pydoc)
```

## Documentation

If you want the documentation for a Python object, simply type:

```
M-x pydoc <RET> <python object> <RET>
```

For example, to view the documentation for `datetime.datetime.now`:

```
M-x pydoc <RET> datetime.datetime.now <RET>
```

Getting the Python object under the cursor (at point) is supported with `pydoc-at-point`.
For example, move the cursor to an object in a Python buffer and type:

```
M-x pydoc-at-point <RET>
```

You may want to bind `pydoc-at-point` to a keyboard shortcut, e.g., "C-h .":

``` emacs-lisp
(define-key python-mode-map (kbd "C-h .") #'pydoc-at-point)
(define-key python-ts-mode-map (kbd "C-h .") #'pydoc-at-point)
```

In `python-ts-mode`, `pydoc-at-point` uses Tree-sitter for object inspection. In
`python-mode`, it uses Jedi. To avoid the Jedi dependency, use `pydoc-at-point-no-jedi` in
place of `pydoc-at-point`.

To start a pydoc server and browse documentation with a web browser, type:

```
M-x pydoc-browse <RET>
```

## License

This project is free software: You can redistribute it and/or modify
it under the terms of the [GNU General Public
License](https://github.com/statmobile/pydoc/blob/master/gpl.txt),
either version 3 of the License, or (at your option) any later
version.
