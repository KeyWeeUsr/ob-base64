;;; ob-base64-tests.el -- tests for ob-base64

;;; Code:

(require 'ert)
(require 'ob-base64)


(defun ob-base64-cleanup ()
  (mapc
   (lambda (buff)
     (unless
         (or (string-match-p (regexp-quote "*Messages*") (buffer-name buff))
             (string-match-p (regexp-quote " *temp*") (buffer-name buff))
             (string-match-p (regexp-quote " *code-conversion-work*")
                             (buffer-name buff)))
       (kill-buffer buff)))
   (buffer-list)))

(ert-deftest ob-base64-raw-results-browse-opens-url ()
  (with-temp-buffer
    (ob-base64-cleanup)
    (org-mode)
    (add-to-list 'org-babel-load-languages '(base64 . t))
    (let ((org-confirm-babel-evaluate nil)
          (old-buffs "")
          (type "raw")
          browse-called
          mock-called
          image-mode-activated
          buff-name)
      (insert (format "#+begin_src base64 :type %s :results browse\n" type))
      (goto-char (point-max))
      (insert "b2ItYmFzZTY0\n")
      (goto-char (point-max))
      (insert "#+end_src\n")
      (goto-char (point-max))
      (should (eq (point) (point-max)))

      (setq old-buffs (format "%s" (buffer-list)))
      (should (not browse-called))
      (should (cdr (split-string (buffer-name (current-buffer)) "*temp*")))

      (advice-add 'browse-url-emacs
                  :after
                  (lambda (&rest r) (setq-local browse-called t)))
      (advice-add 'org-babel-insert-result
                  :override
                  (lambda (&rest r) (setq-local mock-called t)))
      (let ((inhibit-message t)
            (message-log-max nil))
        (org-ctrl-c-ctrl-c))
      (sleep-for 2)  ;; wait for delete timer
      (advice-remove 'browse-url-emacs
                     (lambda (&rest r) (setq-local browse-called t)))
      (advice-remove 'org-babel-insert-result
                     (lambda (&rest r) (setq-local mock-called t)))

      (should browse-called)
      (should (not (string= old-buffs (format "%s" (buffer-list)))))
      (setq buff-name (buffer-name (current-buffer)))
      (should (cdr (split-string buff-name "ob-base64-")))
      (should (string= (car (cdr (split-string buff-name "\\."))) type)))))

(ert-deftest ob-base64-raw-results-embed ()
  (with-temp-buffer
    (ob-base64-cleanup)
    (org-mode)
    (add-to-list 'org-babel-load-languages '(base64 . t))
    (let ((org-confirm-babel-evaluate nil)
          (type "raw")
          result)
      (insert (format "#+begin_src base64 :type %s :results embed\n" type))
      (goto-char (point-max))
      (insert "b2ItYmFzZTY0\n")
      (goto-char (point-max))
      (insert "#+end_src\n")
      (goto-char (point-max))
      (should (eq (point) (point-max)))

      (should (not (org-babel-where-is-src-block-result)))
      (let ((inhibit-message t)
            (message-log-max nil))
        (org-ctrl-c-ctrl-c))
      (should (org-babel-where-is-src-block-result))

      (goto-char (point-min))
      (goto-char (org-babel-where-is-src-block-result))
      (end-of-line)
      (forward-char)
      (setq result (buffer-substring (point) (point-at-eol)))
      (should (string= result ": ob-base64")))))

(ert-deftest ob-base64-raw-results-file-inserts-and-keeps ()
  (with-temp-buffer
    (ob-base64-cleanup)
    (org-mode)
    (add-to-list 'org-babel-load-languages '(base64 . t))
    (let ((org-confirm-babel-evaluate nil)
          (type "raw")
          out-file-raw
          out-file)
      (insert (format "#+begin_src base64 :type %s :results file\n" type))
      (goto-char (point-max))
      (insert "b2ItYmFzZTY0\n")
      (goto-char (point-max))
      (insert "#+end_src\n")
      (goto-char (point-max))
      (should (eq (point) (point-max)))

      (should (not (org-babel-where-is-src-block-result)))
      (let ((inhibit-message t)
            (message-log-max nil))
        (org-ctrl-c-ctrl-c))
      (should (org-babel-where-is-src-block-result))

      (goto-char (point-min))
      (goto-char (org-babel-where-is-src-block-result))
      (end-of-line)
      (forward-char)
      (setq out-file-raw (buffer-substring (point) (point-at-eol)))
      (setq out-file (substring out-file-raw 7 (- (length out-file-raw) 2)))
      (should (string-match "\\[\\[file:.*?\\]\\]" out-file-raw))
      (should (file-exists-p out-file))
      (delete-file out-file)
      (should (not (file-exists-p out-file))))))

(ert-deftest ob-base64-image-results-browse-opens-url ()
  (with-temp-buffer
    (ob-base64-cleanup)
    (org-mode)
    (add-to-list 'org-babel-load-languages '(base64 . t))
    (let ((org-confirm-babel-evaluate nil)
          (old-buffs "")
          (type "png")
          browse-called
          mock-called
          image-mode-activated
          buff-name)
      (insert (format "#+begin_src base64 :type %s :results browse\n" type))
      (goto-char (point-max))
      (insert-file-contents "./example.base64")
      (goto-char (point-max))
      (insert "#+end_src\n")
      (goto-char (point-max))
      (should (eq (point) (point-max)))

      (setq old-buffs (format "%s" (buffer-list)))
      (should (not browse-called))
      (should (not image-mode-activated))
      (should (cdr (split-string (buffer-name (current-buffer)) "*temp*")))

      (advice-add 'browse-url-emacs
                  :after
                  (lambda (&rest r) (setq-local browse-called t)))
      (advice-add 'image-mode
                  :override
                  (lambda (&rest r) (setq-local image-mode-activated t)))
      (advice-add 'org-babel-insert-result
                  :override
                  (lambda (&rest r) (setq-local mock-called t)))
      (let ((inhibit-message t)
            (message-log-max nil))
        (org-ctrl-c-ctrl-c))
      (sleep-for 2)  ;; wait for delete timer
      (advice-remove 'browse-url-emacs
                     (lambda (&rest r) (setq-local browse-called t)))
      (advice-remove 'image-mode
                     (lambda (&rest r) (setq-local image-mode-activated t)))
      (advice-remove 'org-babel-insert-result
                     (lambda (&rest r) (setq-local mock-called t)))

      (should browse-called)
      (should image-mode-activated)
      (should (not (string= old-buffs (format "%s" (buffer-list)))))
      (setq buff-name (buffer-name (current-buffer)))
      (should (cdr (split-string buff-name "ob-base64-")))
      (should (string= (car (cdr (split-string buff-name "\\."))) type)))))

(ert-deftest ob-base64-image-results-embed-creates-image ()
  (with-temp-buffer
    (ob-base64-cleanup)
    (org-mode)
    (add-to-list 'org-babel-load-languages '(base64 . t))
    (let ((org-confirm-babel-evaluate nil)
          (old-buffs "")
          (type "png")
          buff-name
          file-deleted
          img-text)
      (insert (format "#+begin_src base64 :type %s :results embed\n" type))
      (goto-char (point-max))
      (insert-file-contents "./example.base64")
      (goto-char (point-max))
      (insert "#+end_src\n")
      (goto-char (point-max))
      (should (eq (point) (point-max)))

      (setq old-buffs (format "%s" (buffer-list)))
      (should (cdr (split-string (buffer-name (current-buffer)) "*temp*")))

      (should (not (org-babel-where-is-src-block-result)))
      (let ((inhibit-message t)
            (message-log-max nil))
        (advice-add 'delete-file
                    :after
                    (lambda (&rest r) (setq file-deleted t)))
        (org-ctrl-c-ctrl-c)
        (sleep-for 2)
        (advice-remove 'delete-file
                       (lambda (&rest r) (setq file-deleted t))))
      (should (org-babel-where-is-src-block-result))

      (goto-char (point-min))
      (goto-char (org-babel-where-is-src-block-result))
      (end-of-line)
      (forward-char)
      (setq img-text (buffer-substring (point) (point-at-eol)))
      (should (string= " " img-text))
      (should (string= old-buffs (format "%s" (buffer-list))))
      (should file-deleted))))

(ert-deftest ob-base64-image-results-file-inserts-and-keeps ()
  (with-temp-buffer
    (ob-base64-cleanup)
    (org-mode)
    (add-to-list 'org-babel-load-languages '(base64 . t))
    (let ((org-confirm-babel-evaluate nil)
          (old-buffs "")
          (type "png")
          image-mode-activated
          buff-name
          out-file-raw
          out-file)
      (insert (format "#+begin_src base64 :type %s :results file\n" type))
      (goto-char (point-max))
      (insert-file-contents "./example.base64")
      (goto-char (point-max))
      (insert "#+end_src\n")
      (goto-char (point-max))
      (should (eq (point) (point-max)))

      (setq old-buffs (format "%s" (buffer-list)))
      (should (cdr (split-string (buffer-name (current-buffer)) "*temp*")))

      (should (not (org-babel-where-is-src-block-result)))
      (let ((inhibit-message t)
            (message-log-max nil))
        (org-ctrl-c-ctrl-c))
      (should (org-babel-where-is-src-block-result))

      (goto-char (point-min))
      (goto-char (org-babel-where-is-src-block-result))
      (end-of-line)
      (forward-char)
      (setq out-file-raw (buffer-substring (point) (point-at-eol)))
      (setq out-file (substring out-file-raw 7 (- (length out-file-raw) 2)))
      (should (string-match "\\[\\[file:.*?\\]\\]" out-file-raw))
      (should (string= old-buffs (format "%s" (buffer-list))))
      (should (file-exists-p out-file))
      (delete-file out-file)
      (should (not (file-exists-p out-file))))))

(ert-deftest ob-base64-bin-results-browse-opens-hexl ()
  (with-temp-buffer
    (ob-base64-cleanup)
    (org-mode)
    (add-to-list 'org-babel-load-languages '(base64 . t))
    (let ((org-confirm-babel-evaluate nil)
          (old-buffs "")
          (type "bin")
          hexl-called
          mock-called
          image-mode-activated
          buff-name)
      (insert (format "#+begin_src base64 :type %s :results browse\n" type))
      (goto-char (point-max))
      (insert "b2ItYmFzZTY0\n")
      (goto-char (point-max))
      (insert "#+end_src\n")
      (goto-char (point-max))
      (should (eq (point) (point-max)))

      (setq old-buffs (format "%s" (buffer-list)))
      (should (not hexl-called))
      (should (cdr (split-string (buffer-name (current-buffer)) "*temp*")))

      (advice-add 'hexl-find-file
                  :after
                  (lambda (&rest r) (setq-local hexl-called t)))
      (advice-add 'org-babel-insert-result
                  :override
                  (lambda (&rest r) (setq-local mock-called t)))
      (let ((inhibit-message t)
            (message-log-max nil))
        (org-ctrl-c-ctrl-c))
      (sleep-for 2)  ;; wait for delete timer
      (advice-remove 'browse-url-emacs
                     (lambda (&rest r) (setq-local hexl-called t)))
      (advice-remove 'org-babel-insert-result
                     (lambda (&rest r) (setq-local mock-called t)))

      (should hexl-called)
      (should (not (string= old-buffs (format "%s" (buffer-list)))))
      (setq buff-name (buffer-name (current-buffer)))
      (should (cdr (split-string buff-name "ob-base64-")))
      (should (string= (car (cdr (split-string buff-name "\\."))) type)))))

(ert-deftest ob-base64-bin-results-file-inserts-and-keeps ()
  (with-temp-buffer
    (ob-base64-cleanup)
    (org-mode)
    (add-to-list 'org-babel-load-languages '(base64 . t))
    (let ((org-confirm-babel-evaluate nil)
          (type "bin")
          out-file-raw
          out-file)
      (insert (format "#+begin_src base64 :type %s :results file\n" type))
      (goto-char (point-max))
      (insert "b2ItYmFzZTY0\n")
      (goto-char (point-max))
      (insert "#+end_src\n")
      (goto-char (point-max))
      (should (eq (point) (point-max)))

      (should (not (org-babel-where-is-src-block-result)))
      (let ((inhibit-message t)
            (message-log-max nil))
        (org-ctrl-c-ctrl-c))
      (should (org-babel-where-is-src-block-result))

      (goto-char (point-min))
      (goto-char (org-babel-where-is-src-block-result))
      (end-of-line)
      (forward-char)
      (setq out-file-raw (buffer-substring (point) (point-at-eol)))
      (setq out-file (substring out-file-raw 7 (- (length out-file-raw) 2)))
      (should (string-match "\\[\\[file:.*?\\]\\]" out-file-raw))
      (should (file-exists-p out-file))
      (delete-file out-file)
      (should (not (file-exists-p out-file))))))

(ert-deftest ob-base64-bin-results-embed ()
  (with-temp-buffer
    (ob-base64-cleanup)
    (org-mode)
    (add-to-list 'org-babel-load-languages '(base64 . t))
    (let ((org-confirm-babel-evaluate nil)
          (type "bin")
          result)
      (insert (format "#+begin_src base64 :type %s :results embed\n" type))
      (goto-char (point-max))
      (insert "b2ItYmFzZTY0\n")
      (goto-char (point-max))
      (insert "#+end_src\n")
      (goto-char (point-max))
      (should (eq (point) (point-max)))

      (should (not (org-babel-where-is-src-block-result)))
      (let ((inhibit-message t)
            (message-log-max nil))
        (org-ctrl-c-ctrl-c))
      (should (org-babel-where-is-src-block-result))

      (goto-char (point-min))
      (goto-char (org-babel-where-is-src-block-result))
      (end-of-line)
      (forward-char)
      (setq result (buffer-substring (point) (point-at-eol)))
      (should
       (string=
        result
        ": 00000000: 6f62 2d62 6173 6536 34                   ob-base64")))))

(provide 'ob-base64-tests)

;;; ob-base64-tests.el ends here
