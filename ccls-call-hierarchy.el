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

(defface ccls-call-hierarchy-node-normal-face
  nil
  "Regular face to show in call hierarchy labels."
  :group 'ccls)

(defface ccls-call-hierarchy-node-base-face
  '((t (:foreground "orange red")))
  "Face to show in call hierarchy labels when a symbol comes from the base class."
  :group 'ccls)

(defface ccls-call-hierarchy-node-derived-face
  '((t (:foreground "orange")))
  "Face to show in call hierarchy labels when a symbol comes from the derived class."
  :group 'ccls)

(defcustom ccls-call-hierarchy-qualified t
  "Use qualified name for call hierarchy."
  :group 'ccls
  :type 'boolean)

(defcustom ccls-call-hierarchy-side-window 'bottom
  "Position where the call hierarchy window will be shown."
  :group 'ccls
  :type '(choice (const :tag "Bottom of the selected frame" bottom)
		 (const :tag "Left of the selected frame" left)
		 (const :tag "Right of the selected frame" right)
                 (const :tag "Top of the selected frame" top)))

(cl-defstruct ccls-call-hierarchy-node
  ;; Unique identifier for this node.
  id
  ;; Kind of call site that this node represents.
  call-type
  ;; Label that will appear in Treemacs.
  name
  ;; Number of children nodes of this particular node.
  num-children
  ;; Source code location that this node represents.
  location
  ;; File name where the structure represented by this node is.
  filename)

(defun ccls-call-hierarchy--node-label (callee name filename num-children call-type)
  "Create a label for a call hierarchy node.
- CALLEE if the label show show callees; callers otherwise.
- NAME is the symbol name that appears as title.
- FILENAME is the file name that should appear in the label.
- NUM-CHILDREN specifies if singular or plural should be used.
- CALL-TYPE changes the face of the label depending on the type of caller/callee."
  (let ((template (if (zerop num-children)
                      (format "%s"
                              (propertize (f-filename filename) 'face
                                          (pcase call-type
                                            ('0 'ccls-call-hierarchy-node-normal-face)
                                            ('1 'ccls-call-hierarchy-node-base-face)
                                            ('2 'ccls-call-hierarchy-node-derived-face))))
                    (format
                     "%s (%s %s)"
                     (f-filename filename)
                     num-children
                     (let ((label (cond ((eq callee :json-false) "caller")
                                        (t "callee"))))
                       (if (< 1 num-children)
                           (concat label "s")
                         label))))))
    (format "%s %s" name (propertize template 'face 'lsp-lens-face))))

(defun ccls-call-hierarchy--create-treemacs-node (node callee)
  "Create a Treemacs NODE for call hierarchy given CALLEE and some information."
  (let ((id (ccls-call-hierarchy-node-id node))
        (call-type (ccls-call-hierarchy-node-call-type node))
        (name (ccls-call-hierarchy-node-name node))
        (num-children (ccls-call-hierarchy-node-num-children node))
        (location (ccls-call-hierarchy-node-location node))
        (filename (ccls-call-hierarchy-node-filename node)))
    (if (< 0 num-children)
      (list :key id
            :label (ccls-call-hierarchy--node-label
                    callee name filename num-children call-type)
            :children (lambda (_child)
                        (--map (car (ccls-call-hierarchy--handle-references
                                     it callee))
                               (gethash "children"
                                        (with-current-buffer ccls--source-code-buffer
                                          (lsp-request
                                           "$ccls/call"
                                           (ccls-call-hierarchy--parameters
                                            nil id callee))))))
            :icon 'method
            :ret-action (lambda (&rest _)
                        (interactive)
                        (ccls-common-treemacs-return-action filename location)))
    (list :key id
          :label (ccls-call-hierarchy--node-label
                  callee name filename num-children call-type)
          :icon 'method
          :ret-action (lambda (&rest _)
                        (interactive)
                        (ccls-common-treemacs-return-action filename location))))))

(defun ccls-call-hierarchy--handle-references (refs callee)
  "Create Treemacs nodes from REFS, given CALLEE."
  (->> (list refs)
       (-map (lambda (ref)
               (-let* ((location (gethash "location" ref '(nil . nil)))
                       (filename (lsp--uri-to-path (gethash "uri" location)))
                       ((&hash "id" "callType" "name" "children") ref)
                       (node
                        (make-ccls-call-hierarchy-node
                         :id id :call-type callType :name name
                         :num-children (length children)
                         :location location :filename filename)))
                 (ccls-call-hierarchy--create-treemacs-node node callee))))))

(defun ccls-call-hierarchy--search (method params title expand callee)
  "Search and prepare the buffer for the call hierarchy call.
- METHOD is the ccls method that will answer with call
hierarchy information.
- PARAMS are the parameters for METHOD.
- TITLE is the name that the treemacs buffer will have.
- EXPAND tells the Treemacs frontend if the nodes should appear expanded.
- CALLEE controls if the API will return callees or callers."
  (let ((search-buffer (get-buffer-create "*LSP Lookup*")))
    (setq ccls--source-code-buffer (current-buffer))
    (display-buffer-in-side-window search-buffer
                                   `((side . ,ccls-call-hierarchy-side-window)))
    (lsp-request-async
     method
     params
     (lambda (refs)
       (print refs)
       (lsp-treemacs--set-mode-line-format search-buffer " Rendering results... ")
       (lsp-with-cached-filetrue-name
        (let ((lsp-file-truename-cache (ht)))
          (lsp-treemacs-render
           (ccls-call-hierarchy--handle-references refs callee)
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

(defun ccls-call-hierarchy--parameters (initial-call id callee)
  "Return the parameters for INITIAL-CALL or subsequent inheritance requests.
ID identifies the object that should be queried and CALLEE
controls if the call will return callees or callers."
  (if initial-call
      `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
                      :position ,(lsp--cur-position)
                      :callee ,callee
                      :callType 3
                      :qualified ,(if ccls-call-hierarchy-qualified t :json-false)
                      :hierarchy t)
    `(:id ,id
          :callee ,callee
          :callType 3
          :levels ,ccls-tree-initial-levels
          :qualified ,(if ccls-call-hierarchy-qualified t :json-false)
          :hierarchy t)))

(defun ccls-call-hierarchy (callee)
  "Show the call hierarchy tree using ccls.
With a prefix argument CALLEE, show the callee information instead."
  (interactive "P")
  (let ((json-callee (if callee t :json-false)))
    (ccls-call-hierarchy--search "$ccls/call"
                                 (ccls-call-hierarchy--parameters
                                  t nil json-callee)
                                 " Call Viewer "
                                 nil
                                 json-callee)))

(provide 'ccls-call-hierarchy)
;;; ccls-call-hierarchy.el ends here
