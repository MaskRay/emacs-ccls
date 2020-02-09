;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani
;; Copyright (C) 2018 Fangrui Song

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'cc-mode)
(require 'lsp)
(require 'lsp-treemacs)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'dash)

(defgroup ccls nil
  "Customization options for the ccls client"
  :group 'tools)

(defcustom ccls-tree-initial-levels 2
  "Number of tree levels to fetch in custom $ccls methods."
  :type 'integer
  :group 'ccls)

(defun ccls-common-treemacs-icon-from-kind (kind)
  "Return a valid Treemacs icon from a ccls KIND."
  (cond
   ;; Invalid kind will be represented with a class icon.
   ((zerop kind) 'class)
   ((= 1 kind) 'file)
   ((= 2 kind) 'class)
   ((= 3 kind) 'function)
   ((= 4 kind) 'localvariable)))

(defun ccls-common-treemacs-return-action (filename location)
  "Action that will occur when a node is clicked.
FILENAME is the file that will be opened.
LOCATION is the place in FILENAME that contains the symbol reference."
  (lsp-treemacs--open-file-in-mru filename)
  (goto-char (lsp--position-to-point
              (gethash "start" (gethash "range" location))))
  (recenter)
  (run-hooks 'xref-after-jump-hook))

(provide 'ccls-common)
