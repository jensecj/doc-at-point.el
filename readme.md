**Library for showing documentation for the symbol-at-point.**

<p align="center">
  <img src="screencast.gif?raw=true">
</p>

# Usage

`doc-at-point-register` registers a new backend.

`doc-at-point` runs through available backends for the current mode and tries to
find a suitable one to use, if one is found it's used to collect the
documentation for the thing-at-point, the documentation is then presented using
`doc-at-point-display-fn.`

Currently there are 2 native display functions
`doc-at-point--display-with-message`, which uses the minibuffer, and
`doc-at-point--display-with-buffer`, which uses a view-buffer in another window.
There are also two extra functions that are used if their libraries are loaded,
*viz.* `popup.el` or `posframe.el` (from the screenshot).

To register a new backend, you need to provide:
- a string id for the backend
- a `major-mode` where it should be active
- a function for finding the symbol-at-point
- a function for getting the documentation from that symbol
- a predicate which decides if the backend should be used
- a number which denotes the priority of the backend (default 1)

```lisp
(doc-at-point-register
  :id "my backend"
  :mode 'some-mode
  :symbol-fn #'thing-at-point
  :doc-fn #'doc-for-symobl
  :should-run t)
```

You can also register a backend for multiple modes at once, `should-run` can
be a predicate function, and you can set a custom ordering for the priority of the backend.
```lisp
(doc-at-point-register
  :id "default elisp backend"
  :mode '(python-mode another-python-mode)
  :symbol-fn #'elpy-doc--symbol-at-point
  :doc-fn #'elpy-rpc-get-pydoc-documentation
  :should-run #'(lambda () (bound-and-true-p elpy-mode))
  :order 2)
```
