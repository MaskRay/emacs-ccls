;;; ccls.el --- ccls client for lsp-mode     -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Tobias Pisani
;; Copyright (C) 2018 Fangrui Song

;; Author: Tobias Pisani, Fangrui Song
;; Package-Version: 20180929.1
;; Version: 0.1
;; Homepage: https://github.com/MaskRay/emacs-ccls
;; Package-Requires: ((emacs "25.1") (lsp-mode "4.2") (dash "0.14") (projectile "1.0.0"))
;; Keywords: languages, lsp, c++

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
;;
;;; Commentary:

;;
;; To enable, call (lsp-ccls-enable) in your c++-mode hook.
;; See more at https://github.com/MaskRay/ccls/wiki/Emacs
;;

;;; Code:

(require 'ccls-common)
(require 'ccls-semantic-highlight)
(require 'ccls-code-lens)
(require 'ccls-tree)
(require 'ccls-call-hierarchy)
(require 'ccls-inheritance-hierarchy)
(require 'ccls-member-hierarchy)

(require 'seq)

;; ---------------------------------------------------------------------
;;   Customization
;; ---------------------------------------------------------------------

(defcustom ccls-executable
  "ccls"
  "Path of the ccls executable."
  :type 'file
  :group 'ccls)

(defcustom ccls-args
  nil
  "Additional command line options passed to the ccls executable."
  :type '(repeat string)
  :group 'ccls)

(defcustom ccls-initialization-options
  nil
  "initializationOptions"
  :group 'ccls)

;; ---------------------------------------------------------------------
;;   Other ccls-specific methods
;; ---------------------------------------------------------------------
;;
(defun ccls-info ()
  (lsp-request "$ccls/info" (make-hash-table)))

(defun ccls-file-info ()
  (lsp-request "$ccls/fileInfo"
               `(:textDocument ,(lsp--text-document-identifier))))

(defun ccls-preprocess-file (&optional output-buffer)
  "Preprocess selected buffer."
  (interactive)
  (lsp--cur-workspace-check)
  (-when-let* ((mode major-mode)
               (info (ccls-file-info))
               (args (seq-into (gethash "args" info) 'vector))
               (new-args (let ((i 0) ret)
                           (while (< i (length args))
                             (let ((arg (elt args i)))
                               (cond
                                ((string= arg "-o") (cl-incf i))
                                ((string-match-p "\\`-o.+" arg))
                                (t (push arg ret))))
                             (cl-incf i))
                           (nreverse ret))))
    (with-current-buffer (or output-buffer
                             (get-buffer-create
                              (format "*lsp-ccls preprocess %s*" (buffer-name))))
      (pop-to-buffer (current-buffer))
      (with-silent-modifications
        (erase-buffer)
        (insert (format "// Generated by: %s"
                        (combine-and-quote-strings new-args)))
        (insert (with-output-to-string
                  (with-current-buffer standard-output
                    (apply #'process-file (car new-args) nil t nil "-E" (cdr new-args)))))
        (delay-mode-hooks (funcall mode))
        (setq buffer-read-only t)))))

(defun ccls-reload ()
  "Reset database and reload cached index files."
  (interactive)
  (lsp-notify "$ccls/reload" (list :whitelist [] :blacklist [])))

(defun ccls-navigate (direction)
  "Navigate to a nearby outline symbol.
DIRECTION can be \"D\", \"L\", \"R\" or \"U\"."
  (lsp-find-custom "$ccls/navigate" `(:direction ,direction)))

;; ---------------------------------------------------------------------
;;  Register lsp client
;; ---------------------------------------------------------------------

(defun ccls--suggest-project-root ()
  (and (memq major-mode '(c-mode c++-mode cuda-mode objc-mode))
       (when-let (dir (locate-dominating-file default-directory ".ccls-root"))
         (expand-file-name dir))))

(advice-add 'lsp--suggest-project-root :before-until #'ccls--suggest-project-root)


(defun ccls--get-initialization-options ()
  "Simple function to return initializationOptions for ccls.
User can rewrite this function to provide some project-specific options."
  ccls-initialization-options)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection (lambda () (cons ccls-executable ccls-args)))
  :major-modes '(c-mode c++-mode cuda-mode objc-mode)
  :server-id 'ccls
  :multi-root nil
  :notification-handlers
  (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
          ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
  :initialization-options (lambda () (ccls--get-initialization-options))
  :library-folders-fn nil))

(provide 'ccls)
;;; ccls.el ends here
