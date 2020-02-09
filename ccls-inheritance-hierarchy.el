;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani
;; Copyright (C) 2018 Fangrui Song
;; Copyright (C) 2020 Daniel Martín

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

(require 'ccls-common)
(require 'lsp-treemacs)

(defcustom ccls-inheritance-hierarchy-qualified t
  "Use qualified name for types in inheritance hierarchy."
  :group 'ccls
  :type 'boolean)

(defcustom ccls-inheritance-hierarchy-side-window 'bottom
  "Position where the inheritance hierarchy window will be shown."
  :group 'ccls
  :type '(choice (const :tag "Bottom of the selected frame" bottom)
		 (const :tag "Left of the selected frame" left)
		 (const :tag "Right of the selected frame" right)
                 (const :tag "Top of the selected frame" top)))

(cl-defstruct ccls-inheritance-hierarchy-node
  ;; Unique identifier for this node.
  id
  ;; Kind of programming structure that this node represents.
  kind
  ;; Label that will appear in Treemacs.
  name
  ;; Number of children nodes of this particular node.
  num-children
  ;; Source code location that this node represents.
  location
  ;; File name where the structure represented by this node is.
  filename)

(defun ccls-inheritance-hierarchy--node-label (derived name filename num-children)
  "Return a label for a DERIVED or not inheritance hierarchy node.
NAME is the name of the symbol.  FILENAME is the file where the
symbol resides.  NUM-CHILDREN is the number of children of the
node and will determine if singular or plural is used."
  (let ((template (if (zerop num-children)
                      (format "%s"
                              (propertize (f-filename filename) 'face 'lsp-lens-face))
                    (format
                     (if (< 1 num-children) "%s (%s %s classes)"
                       "%s (%s %s class)")
                     (f-filename filename)
                     num-children
                     (if (eq derived :json-false) "base" "derived")))))
    (format "%s %s" name (propertize template 'face 'lsp-lens-face))))

(defun ccls-inheritance-hierarchy--create-treemacs-node (node derived)
  "Create a Treemacs NODE for class hierarchy, given DERIVED."
  (let ((id (ccls-inheritance-hierarchy-node-id node))
        (kind (ccls-inheritance-hierarchy-node-kind node))
        (name (ccls-inheritance-hierarchy-node-name node))
        (num-children (ccls-inheritance-hierarchy-node-num-children node))
        (location (ccls-inheritance-hierarchy-node-location node))
        (filename (ccls-inheritance-hierarchy-node-filename node)))
    (if (< 0 num-children)
      (list :key id
            :label (ccls-inheritance-hierarchy--node-label
                    derived name filename num-children)
            :children (lambda (_child)
                        (--map (car (ccls-inheritance-hierarchy--handle-references
                                     it derived))
                               (gethash "children"
                                        (with-current-buffer ccls--source-code-buffer
                                          (lsp-request
                                           "$ccls/inheritance"
                                           (ccls-inheritance-hierarchy--parameters
                                            nil id kind derived))))))
            :icon (ccls-common-treemacs-icon-from-kind kind)
            :ret-action (lambda (&rest _)
                        (interactive)
                        (ccls-common-treemacs-return-action filename location)))
    (list :key id
          :label (ccls-inheritance-hierarchy--node-label
                  derived name filename num-children)
          :icon (ccls-common-treemacs-icon-from-kind kind)
          :ret-action (lambda (&rest _)
                        (interactive)
                        (ccls-common-treemacs-return-action filename location))))))

(defun ccls-inheritance-hierarchy--handle-references (refs derived)
  "Create Treemacs nodes from REFS, given DERIVED."
  (->> (list refs)
       (-map (lambda (ref)
               (-let* ((location (gethash "location" ref '(nil . nil)))
                       (filename (lsp--uri-to-path (gethash "uri" location)))
                       ((&hash "id" "kind" "name" "children") ref)
                       (node
                        (make-ccls-inheritance-hierarchy-node
                         :id id :kind kind :name name
                         :num-children (length children)
                         :location location :filename filename)))
                 (ccls-inheritance-hierarchy--create-treemacs-node node derived))))))

(defun ccls-inheritance-hierarchy--search (method params title expand derived)
  "Search and prepare the buffer for the inheritance hierarchy call.
- METHOD is the ccls method that will answer with inheritance
hierarchy information.
- PARAMS are the parameters for METHOD.
- TITLE is the name that the treemacs buffer will have.
- EXPAND tells the Treemacs frontend if the nodes should appear expanded.
- DERIVED controls if the API will return derived class or base
class information."
  (let ((search-buffer (get-buffer-create "*LSP Lookup*")))
    (setq ccls--source-code-buffer (current-buffer))
    (display-buffer-in-side-window search-buffer
                                   `((side . ,ccls-inheritance-hierarchy-side-window)))
    (lsp-request-async
     method
     params
     (lambda (refs)
       (lsp-treemacs--set-mode-line-format search-buffer " Rendering results... ")
       (lsp-with-cached-filetrue-name
        (let ((lsp-file-truename-cache (ht)))
          (lsp-treemacs-render
           (ccls-inheritance-hierarchy--handle-references refs derived)
           title
           expand)))
       (lsp--info "Refresh completed!"))
     :mode 'detached
     :cancel-token :treemacs-lookup)

    (with-current-buffer search-buffer
      (lsp-treemacs-initialize)
      (lsp-treemacs--set-mode-line-format search-buffer " Loading... ")
      (setq-local lsp-treemacs-tree nil)
      (lsp-treemacs-generic-refresh))))

(defun ccls-inheritance-hierarchy--parameters (initial-call id kind derived)
  "Return the parameters for the INITIAL-CALL or subsequent invocations.
ID is the identifier for the inheritance hierarchy call.
KIND is the kind of programming structure that a parent node represents.
DERIVED controls if the call will return derived classes or base classes."
  (if initial-call
      `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
                      :position ,(lsp--cur-position)
                      :derived ,derived
                      :qualified
                      ,(if ccls-inheritance-hierarchy-qualified t :json-false)
                      :levels 1
                      :hierarchy t)
    `(:id ,id
          :kind ,kind
          :derived ,derived
          :qualified ,(if ccls-inheritance-hierarchy-qualified t :json-false)
          :levels ,ccls-tree-initial-levels
          :hierarchy t)))

(defun ccls-inheritance-hierarchy (derived)
  "Show the base class inheritance hierarchy tree using ccls.
With a prefix argument DERIVED, show derived classes instead."
  (interactive "P")
  (let ((json-derived (if derived t :json-false)))
    (ccls-inheritance-hierarchy--search "$ccls/inheritance"
                                        (ccls-inheritance-hierarchy--parameters
                                         t nil nil json-derived)
                                        " Inheritance Viewer "
                                        t
                                        json-derived)))

(provide 'ccls-inheritance-hierarchy)
;;; ccls-inheritance-hierarchy.el ends here
