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

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defcustom ccls-member-hierarchy-qualified nil
  "Show a detailed name for member hierarchy."
  :group 'ccls
  :type 'boolean)

(defcustom ccls-member-hierarchy-side-window 'bottom
  "Position where the member hierarchy window will be shown."
  :group 'ccls
  :type '(choice (const :tag "Bottom of the selected frame" bottom)
		 (const :tag "Left of the selected frame" left)
		 (const :tag "Right of the selected frame" right)
                 (const :tag "Top of the selected frame" top)))

;; ---------------------------------------------------------------------
;;   Data Structures
;; ---------------------------------------------------------------------

(cl-defstruct ccls-member-hierarchy-node
  ;; User-facing name of this node.
  name
  ;; User-facing field name of this node.
  field-name
  ;; Unique identifier for this node.
  id
  ;; Number of children nodes of this particular node.
  num-children
  ;; Source code location that this node represents.
  location
  ;; File name where the structure represented by this node is.
  filename)

(defun ccls-member-hierarchy--create-treemacs-node (node parent)
  "Create a Treemacs NODE to show member hierarchy, given a PARENT and some information."
  (let ((id (ccls-member-hierarchy-node-id node))
        (name (ccls-member-hierarchy-node-name node))
        (field-name (ccls-member-hierarchy-node-field-name node))
        (num-children (ccls-member-hierarchy-node-num-children node))
        (filename (ccls-member-hierarchy-node-filename node))
        (location (ccls-member-hierarchy-node-location node)))
    (if (not (or (>= 0 num-children)
                 (and parent
                      (equal id (ccls-member-hierarchy-node-id parent)))))
        (list :key id
              :label (if (null parent)
                         name
                       field-name)
              :children (lambda (_child)
                          (--map (car (ccls-member-hierarchy--handle-references
                                       it node))
                                 (gethash "children"
                                          (with-current-buffer ccls--source-code-buffer
                                            (lsp-request
                                             "$ccls/member"
                                             (ccls-member-hierarchy--parameters
                                              nil id))))))
              :icon 'localvariable
              :ret-action (lambda (&rest _)
                            (interactive)
                            (ccls-common-treemacs-return-action filename location)))
      (list :key id
            :label (if (null parent)
                         name
                       field-name)
            :icon 'localvariable
            :ret-action (lambda (&rest _)
                          (interactive)
                          (ccls-common-treemacs-return-action filename location))))))

(defun ccls-member-hierarchy--handle-references (refs parent)
  "Create Treemacs nodes from REFS and a PARENT."
  (->> (list refs)
       (-map (lambda (ref)
               (-let* ((location (gethash "location" ref '(nil . nil)))
                       (filename (lsp--uri-to-path (gethash "uri" location)))
                       ((&hash "location" location "numChildren" num-children "name" name "fieldName" field-name "id" id) ref)
                       (node
                        (make-ccls-member-hierarchy-node
                         :name name :field-name field-name :id id :num-children num-children :location location :filename filename)))
                 (ccls-member-hierarchy--create-treemacs-node node parent))))))

(defun ccls-member-hierarchy--search (method params title expand)
  "Search and prepare the buffer for the member hierarchy call.
- METHOD is the ccls method that will answer with member
  hierarchy information.
- PARAMS are the parameters for METHOD.
- TITLE is the name that the Treemacs buffer will have.
- EXPAND tells the Treemacs frontend if the nodes should appear expanded."
  (let ((search-buffer (get-buffer-create "*LSP Lookup*")))
    (setq ccls--source-code-buffer (current-buffer))
    (display-buffer-in-side-window search-buffer
                                   `((side . ,ccls-member-hierarchy-side-window)))
    (lsp-request-async
     method
     params
     (lambda (refs)
       (lsp-treemacs--set-mode-line-format search-buffer " Rendering results... ")
       (lsp-with-cached-filetrue-name
        (let ((lsp-file-truename-cache (ht)))
          (lsp-treemacs-render
           (ccls-member-hierarchy--handle-references refs nil)
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

(defun ccls-member-hierarchy--parameters (initial-call id)
  "Return the parameters for the INITIAL-CALL or a subsequent call given an ID."
  (if initial-call
      `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
                      :position ,(lsp--cur-position)
                      :levels 1
                      :qualified ,(if ccls-member-hierarchy-qualified t :json-false)
                      :hierarchy t)
    `(:id ,id
          :levels 2
          :qualified ,(if ccls-member-hierarchy-qualified t :json-false)
          :hierarchy t)))

(defun ccls-member-hierarchy ()
  "Show the hierarchy of members of the class/struct at point."
  (interactive)
  (ccls-member-hierarchy--search "$ccls/member"
                                 (ccls-member-hierarchy--parameters
                                  t nil)
                                 " Member Hierarchy "
                                 nil))

(provide 'ccls-member-hierarchy)
;;; ccls-member-hierarchy.el ends here
