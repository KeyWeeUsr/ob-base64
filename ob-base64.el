;;; ob-base64.el --- Org-babel for base64 content -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Badida

;; Author: Peter Badida <keyweeusr@gmail.com>
;; Keywords: convenience, embedding, orgmode, base64, rendering
;; Version: 1.1.2
;; Package-Requires: ((emacs "26.1"))
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
(require 'image-file)

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

(defcustom ob-base64-schedule-autodelete-interval-ms
  100
  "Auto-delete temporary file after decoding, might break external viewer."
  :group 'ob-base64
  :type 'number)

(defcustom ob-base64-schedule-delete-interval-ms
  1000
  "Delay for deleting temporary file after displaying.

Waits for closing of external process, safer than
`ob-base64-schedule-autodelete-interval-ms'."
  :group 'ob-base64
  :type 'number)

(defcustom ob-base64-embed-image-string
  " "
  "Placeholder for embedded image."
  :group 'ob-base64
  :type 'string)

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:base64
  '((:results . "embed") (:exports . "results"))
  "Default arguments for evaluating a base64 source block.")

(defun org-babel-expand-body:base64 (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body.
Optional argument PROCESSED-PARAMS Coming from org-babel template.

This function expands the body of a source code block by doing
things like prepending argument definitions to the body, it
should be called by the `org-babel-execute:base64' function
below. Variables get concatenated in the `mapconcat' form,
therefore to change the formatting you can edit the `format'
form."
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
(defun org-babel-execute:base64 (body params &optional level)
  "Execute a block of Base64 code with org-babel.
This function is called by `org-babel-execute-src-block'
Argument BODY Coming from org-babel template.
Argument PARAMS Coming from org-babel template.
Optional argument LEVEL of recursive call.

:type | :results | action
raw   | browse   | open in new/temp buffer
raw   | embed    | standard text result of decoded base64
raw   | file     | link to kept file
image | browse   | `image-mode' for file, `delete-file' after func call
image | embed    | to temp file, `create-image', embed, `delete-file'
image | file     | link to kept file
bin   | browse   | `hexl-find-file'
bin   | embed    | `hexlify-buffer'
bin   | file     | link to kept file

:scale is passed to `create-image'."
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
         (embed (let ((value nil))
                  (dolist (param processed-params)
                   (when (string= ":results" (car param))
                     (setq value (split-string (cdr param)))))
                  (when (string= "embed" (car (last value)))
                    t)))
         (file (let ((value nil))
                  (dolist (param processed-params)
                   (when (string= ":results" (car param))
                     (setq value (split-string (cdr param)))))
                  (when (string= "file" (car (last value)))
                    t)))
         (browse (let ((value nil))
                  (dolist (param processed-params)
                   (when (string= ":results" (car param))
                     (setq value (split-string (cdr param)))))
                  (when (string= "browse" (car (last value)))
                    t)))
         (type (let ((value nil))
                 (dolist (param processed-params)
                   (when (string= ":type" (car param))
                     (setq value (cdr param))))
                 (if (not value)
                     (error ":type for base64 block must be defined")
                   value)))
         (scale (let ((value nil))
                 (dolist (param processed-params)
                   (when (string= ":scale" (car param))
                     (setq value (cdr param))))
                 (if (not value)
                     1.0
                   value)))
         (tmp-skip (and embed (not (member type image-file-name-extensions))))
         (tmp-file (unless tmp-skip
                     (org-babel-temp-file "ob-base64-" (format ".%s" type))))
         (tmp-name (unless tmp-skip
                     (format "ob-base64-render-%s"
                             (replace-regexp-in-string "/" "" tmp-file)))))
    (ignore vars result-params result-type)

    (unless (and embed (not (member type image-file-name-extensions)))
      (with-temp-file tmp-file
        (set-buffer-file-coding-system 'no-conversion)
        (insert (base64-decode-string full-body))))

    (when (or ob-base64-default-external
              (string= external "yes"))
      (ob-base64--render-external tmp-name tmp-file))

    (when ob-base64-tmp-file-autodelete
      ;; TODO: try-finally
      (delete-file tmp-file))

    (cond ((or ob-base64-default-external (string= external "yes"))
           (ob-base64--render-external-cleanup tmp-name tmp-file))
          (browse (if (not (string= type "bin"))
                      (ob-base64--render-internal-browse tmp-name tmp-file)
                    (hexl-find-file tmp-file)
                    (delete-file tmp-file)))
          (embed (if (member type image-file-name-extensions)
                     (ob-base64--render-internal-embed
                      body params level tmp-name tmp-file scale)
                   (if (string= type "bin")
                       (with-temp-buffer
                         (let ((y-or-n-p (lambda (&rest _) nil)))
                           (ignore y-or-n-p)
                           (insert (base64-decode-string full-body))
                           (hexlify-buffer)
                           (buffer-string)))
                     (base64-decode-string full-body))))
          (file tmp-file))))

(defun ob-base64--render-external (name file)
  "Render a decoded base64 externally with `ob-base64-external-viewer-bin'.
Argument NAME for external process.
Argument FILE path to the temporary file to render."
  (start-process name nil ob-base64-external-viewer-bin file))

(defun ob-base64--render-external-cleanup (name file)
  "Clean-up FILE after rendering externally.
Argument NAME for external process."
  (run-at-time
   t
   (/ ob-base64-schedule-autodelete-interval-ms 1000.0)
   (lambda (lname lfile)
     (let ((found nil))
       (dolist (proc (process-list))
         (when (string= lname (process-name proc))
           (setq found t)))
       (unless found
         (when (file-exists-p lfile)
           (delete-file lfile))
         (dolist (timer timer-list)
           (when (string-match lname (format "%s" timer))
             (cancel-timer timer))))))
   name file)
  nil)

(defun ob-base64--render-internal-browse (name file)
  "Render an image into a new buffer with `image-mode'.
Argument NAME for timer identification.
Argument FILE path to the temporary file to render."
  (browse-url-emacs file nil)
  (run-at-time
   (/ ob-base64-schedule-delete-interval-ms 1000)
   nil
   (lambda (lname lfile)
     (delete-file lfile)
     (dolist (timer timer-list)
       (when (string-match lname (format "%s" timer))
         (cancel-timer timer))))
   name file))

(defun ob-base64--render-internal-embed (body params level name file scale)
  "Render an image directly into a buffer.

This is done via recursion due to org-babel not being able to
take the result of `create-image' and render it in-place, so the
first iteration makes the `#+RESULT:' section and the following
navigates to it only to insert the image in-place.  The raw
inserted value is ` ' (whitespace).
Argument BODY org-babel param.
Argument PARAMS org-babel param.
Argument LEVEL of recursive call.
Argument NAME for timer identification.
Argument FILE path to the temporary file to render.
Argument SCALE passed as `create-image' :scale property."
  (when (and (not (org-babel-where-is-src-block-result))
             (not level))
    (run-at-time
     0 nil
     (lambda (old-file old-body old-params new-level)
       (delete-file old-file)
       (org-babel-execute:base64 old-body old-params new-level))
     file body params 1))

  (when (or level (org-babel-where-is-src-block-result))
    (goto-char (org-babel-where-is-src-block-result))
    (move-end-of-line nil)
    (forward-char)
    (delete-region (point) (line-end-position))
    (backward-char)
    (delete-char 1)
    (insert "\n")
    (insert-image (create-image file nil nil :scale scale)
                  ob-base64-embed-image-string nil
                  nil)
    (move-beginning-of-line nil)
    (backward-char)
    (move-beginning-of-line nil)
    (backward-char))

  (run-at-time
   (/ ob-base64-schedule-delete-interval-ms 1000)
   nil
   (lambda (lname lfile)
     (delete-file lfile)
     (dolist (timer timer-list)
       (when (string-match lname (format "%s" timer))
         (cancel-timer timer))))
   name file)
  nil)

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:base64 (_session _params) "Noop.")
(defun org-babel-base64-table-or-string (_results) "Noop.")
(defun org-babel-base64-initiate-session (&optional _session) "Noop.")

(defun org-babel-base64-var-to-base64 (var)
  "Convert an elisp VAR into a string of base64 source code specifying a VAR."
  (format "%S" var))

(provide 'ob-base64)
;;; ob-base64.el ends here
