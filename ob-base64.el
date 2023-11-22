;;; ob-base64.el --- org-babel for base64 content -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, embedding, orgmode, base64, rendering
;; Version: 1.0.0
;; Package-Requires: ()
;; Homepage: https://github.com/keyweeusr/ob-base64

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package extends org-mode with a way to evaluate base64-encoded content
;; either within Emacs or view it externally via some 3rd-party opener.
;;
;; (add-to-list 'org-babel-load-languages '(base64 . t))

;;; Requirements:

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

(defgroup ob-base64
  nil
  "Customization group for `ob-base64'."
  :group 'org
  :group 'convenience
  :group 'extensions
  :group 'external)

(defcustom ob-base64-default-external
  nil
  "Open images in external viewer or insert to Org buffer."
  :group 'ob-base64
  :type 'boolean)

(defcustom ob-base64-external-viewer-bin
  "/usr/bin/xviewer"
  "Path to external image viewer."
  :group 'ob-base64
  :type 'string)

(defcustom ob-base64-tmp-file-autodelete
  nil
  "Auto-delete temporary file when exiting `with-temp-file', might break."
  :group 'ob-base64
  :type 'boolean)

(defcustom ob-base64-schedule-delete-interval-ms
  100
  "Auto-delete temporary file, might break external viewer."
  :group 'ob-base64
  :type 'boolean)

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:base64 '())

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:base64' function below. Variables get concatenated in the
;; `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:base64 (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-base64 nil t)
  (let ((vars (org-babel--get-vars (or processed-params
                                       (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair)
                (org-babel-base64-var-to-base64 (cdr pair))))
      vars "\n")
     "\n" body "\n")))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
(defun org-babel-execute:base64 (body params)
  "Execute a block of Base64 code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Base64 source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         (result-params (assq :result-params processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (assq :result-type processed-params))
         ;; expand the body with `org-babel-expand-body:base64'
         (full-body (org-babel-expand-body:base64
                     body params processed-params))
         (external (let ((value nil))
                     (dolist (param processed-params)
                       (when (string= ":external" (car param))
                         (setq value (cdr param))))
                     value))
         (type (let ((value nil))
                 (dolist (param processed-params)
                   (when (string= ":type" (car param))
                     (setq value (cdr param))))
                 (if (not value)
                     (error ":type for base64 block must be defined")
                   value))))
    (let* ((tmp-file (make-temp-file "ob-base64-" nil (format ".%s" type)))
           (tmp-name (format "ob-base64-render-%s"
                             (replace-regexp-in-string "/" "" tmp-file))))
      (with-temp-file tmp-file
        (set-buffer-file-coding-system 'no-conversion)
        (insert (base64-decode-string full-body)))
      (if (or ob-base64-default-external
              (string= external "yes"))
          (start-process tmp-name nil ob-base64-external-viewer-bin tmp-file)
        nil)
      (if ob-base64-tmp-file-autodelete
          (delete-file tmp-file)
        (when (or ob-base64-default-external
                  (string= external "yes"))
          (run-at-time
           t (/ ob-base64-schedule-delete-interval-ms 1000.0)
           (lambda (name file)
             (let ((found nil))
               (dolist (proc (process-list))
                 (when (string= name (process-name proc))
                   (setq found t)))
               (unless found
                 (delete-file file)
                 (dolist (timer timer-list)
                   (when (string-match name (format "%s" timer))
                     (cancel-timer timer))))))
           tmp-name tmp-file))))
    nil))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:base64 (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS.")

(defun org-babel-base64-var-to-base64 (var)
  "Convert an elisp var into a string of base64 source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-base64-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string.")

(defun org-babel-base64-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")))

(provide 'ob-base64)
;;; ob-base64.el ends here
