;;; org-typst-preview-test.el --- Tests for org-typst-preview -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Roi Fe Wu

;;; Code:

(require 'ert)
(require 'org-typst-preview)

(defmacro org-typst-preview-test--with-renderers (&rest body)
  "Run BODY recording renderer requests in `latex-regions' and `typst-fragments'."
  (declare (indent 0) (debug t))
  `(let (latex-regions typst-fragments)
     (cl-letf (((symbol-function 'display-graphic-p)
                (lambda (&optional _display) t))
               ((symbol-function 'org-check-external-command) #'ignore)
               ((symbol-function 'org--latex-preview-region)
                (lambda (begin end)
                  (push (list begin end) latex-regions)))
               ((symbol-function 'org-typst-preview--schedule)
                (lambda (fragment)
                  (push fragment typst-fragments)
                  'scheduled)))
       ,@body)))

(ert-deftest org-typst-preview-parses-boundaries ()
  (with-temp-buffer
    (org-mode)
    (insert "#(x^2#)\n#[\n  sum_(i=1)^n i\n#]")
    (let ((inline (org-typst-preview--fragment-at (point-min))))
      (should (equal (plist-get inline :body) "x^2"))
      (should (equal (plist-get inline :value) "$x^2$"))
      (should-not (plist-get inline :block)))
    (goto-char (point-min))
    (search-forward "#[")
    (let ((block (org-typst-preview--fragment-at (- (point) 2))))
      (should (equal (plist-get block :body) "sum_(i=1)^n i"))
      (should (equal (plist-get block :value) "$ sum_(i=1)^n i $"))
      (should (plist-get block :block)))))

(ert-deftest org-typst-preview-collects-only-typst-fragments ()
  (with-temp-buffer
    (org-mode)
    (insert "Inline #(sum_(i=1)^n i#).\n\n"
            "#[integral_0^1 x^2 dif x = 1/3#]\n"
            "LaTeX remains $x^2$.")
    (let ((fragments
           (org-typst-preview--fragments (point-min) (point-max))))
      (should (= (length fragments) 2))
      (should (equal (plist-get (car fragments) :value)
                     "$sum_(i=1)^n i$"))
      (should (equal (plist-get (cadr fragments) :value)
                     "$ integral_0^1 x^2 dif x = 1/3 $")))))

(ert-deftest org-typst-preview-ignores-literal-and-escaped-boundaries ()
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src typst\n"
            "#[not_a_preview#]\n"
            "#+end_src\n\n"
            "\\#(escaped#) and #(previewed#)")
    (let ((fragments
           (org-typst-preview--fragments (point-min) (point-max))))
      (should (= (length fragments) 1))
      (should (equal (plist-get (car fragments) :body) "previewed")))))

(ert-deftest org-typst-preview-builds-typst-source-with-org-settings ()
  (let ((org-format-latex-options
         '(:foreground "red" :background "Transparent" :scale 2.0))
        (org-typst-preview-font-size 10.0)
        (org-typst-preview-preamble "#let answer = 42"))
    (let ((source (org-typst-preview--source "$answer$" 'default)))
      (should (string-match-p "fill: none" source))
      (should (string-match-p "size: 20\\.000pt" source))
      (should (string-match-p "fill: rgb(255,0,0)" source))
      (should (string-match-p "#let answer = 42" source))
      (should (string-suffix-p "$answer$\n" source)))))

(ert-deftest org-typst-preview-mode-remaps-org-commands ()
  (with-temp-buffer
    (org-mode)
    (org-typst-preview-mode 1)
    (should (eq (command-remapping 'org-latex-preview)
                'org-typst-preview))
    (should-not (command-remapping 'org-clear-latex-preview))
    (should (memq #'org-typst-preview--export-before-parsing
                  org-export-before-parsing-functions))
    (org-typst-preview-mode -1)
    (should (eq (key-binding (kbd "C-c C-x C-l")) 'org-latex-preview))
    (should-not (memq #'org-typst-preview--export-before-parsing
                      org-export-before-parsing-functions))))

(ert-deftest org-typst-preview-key-previews-latex-at-point ()
  (dolist (formula '("$x^2$" "\\(x^2\\)" "\\[x^2\\]"
                     "\\begin{equation}\nx^2\n\\end{equation}"))
    (with-temp-buffer
      (org-mode)
      (org-typst-preview-mode 1)
      (insert "Before #(y#).\n\n" formula "\n\nAfter #(z#).")
      (search-backward formula)
      (forward-char 2)
      (let* ((datum (org-element-context))
             (bounds (list (org-element-begin datum) (org-element-end datum))))
        (org-typst-preview-test--with-renderers
          (call-interactively (key-binding (kbd "C-c C-x C-l")))
          (should (equal latex-regions (list bounds)))
          (should-not typst-fragments))))))

(ert-deftest org-typst-preview-key-previews-typst-at-point ()
  (with-temp-buffer
    (org-mode)
    (org-typst-preview-mode 1)
    (insert "$x$\n\n#(y#)\n\n$z$")
    (search-backward "y")
    (org-typst-preview-test--with-renderers
      (call-interactively (key-binding (kbd "C-c C-x C-l")))
      (should-not latex-regions)
      (should (equal (mapcar (lambda (fragment) (plist-get fragment :body))
                             typst-fragments)
                     '("y"))))))

(ert-deftest org-typst-preview-key-previews-both-in-section ()
  (with-temp-buffer
    (org-mode)
    (org-typst-preview-mode 1)
    (insert "* First\nText.\n$x$\n#(y#)\n* Second\n$z$\n#(w#)")
    (let ((end (save-excursion (search-backward "* Second"))))
      (goto-char (point-min))
      (search-forward "Text.")
      (org-typst-preview-test--with-renderers
        (call-interactively (key-binding (kbd "C-c C-x C-l")))
        (should (equal latex-regions (list (list (point-min) end))))
        (should (equal (mapcar (lambda (fragment) (plist-get fragment :body))
                               typst-fragments)
                       '("y")))))))

(ert-deftest org-typst-preview-key-previews-both-in-region ()
  (with-temp-buffer
    (org-mode)
    (org-typst-preview-mode 1)
    (insert "$before$\n#(before#)\n\n$x$\n#(y#)\n\n$after$\n#(after#)")
    (let* ((begin (save-excursion (search-backward "$x$")))
           (end (save-excursion (search-backward "$after$")))
           (transient-mark-mode t))
      (goto-char end)
      (set-mark begin)
      (setq mark-active t)
      (org-typst-preview-test--with-renderers
        (call-interactively (key-binding (kbd "C-c C-x C-l")))
        (should (equal latex-regions (list (list begin end))))
        (should (equal (mapcar (lambda (fragment) (plist-get fragment :body))
                               typst-fragments)
                       '("y")))))))

(ert-deftest org-typst-preview-key-previews-both-in-buffer ()
  (dolist (active-region '(nil t))
    (with-temp-buffer
      (org-mode)
      (org-typst-preview-mode 1)
      (insert "$x$\n#(y#)\n* Next\n$z$\n#(w#)")
      (goto-char (point-min))
      (let ((transient-mark-mode t)
            (current-prefix-arg '(16)))
        (when active-region
          (set-mark (point))
          (forward-char 3)
          (setq mark-active t))
        (org-typst-preview-test--with-renderers
          (call-interactively (key-binding (kbd "C-c C-x C-l")))
          (should (equal latex-regions
                         (list (list (point-min) (point-max)))))
          (should (equal (mapcar (lambda (fragment) (plist-get fragment :body))
                                 (reverse typst-fragments))
                         '("y" "w"))))))))

(ert-deftest org-typst-preview-key-clears-both-kinds-of-preview ()
  (dolist (scope '(section region buffer))
    (with-temp-buffer
      (org-mode)
      (org-typst-preview-mode 1)
      (insert "* First\n$x$\n#(y#)\n* Second\n$z$\n#(w#)")
      (let* ((fragments (org-typst-preview--fragments (point-min) (point-max)))
             (typst-overlays (mapcar #'org-typst-preview--make-overlay fragments))
             (latex-overlays
              (mapcar (lambda (formula)
                        (goto-char (point-min))
                        (search-forward formula)
                        (let ((overlay (make-overlay (- (point) (length formula))
                                                      (point))))
                          (overlay-put overlay 'org-overlay-type 'org-latex-overlay)
                          overlay))
                      '("$x$" "$z$")))
             (transient-mark-mode t)
             (current-prefix-arg (if (eq scope 'buffer) '(64) '(4))))
        (goto-char (point-min))
        (when (eq scope 'region)
          (set-mark (point))
          (goto-char (plist-get (car fragments) :end))
          (setq mark-active t))
        (org-typst-preview-test--with-renderers
          (call-interactively (key-binding (kbd "C-c C-x C-l")))
          (should-not latex-regions)
          (should-not typst-fragments))
        (should-not (overlay-buffer (car latex-overlays)))
        (should-not (overlay-buffer (car typst-overlays)))
        (if (eq scope 'buffer)
            (progn
              (should-not (overlay-buffer (cadr latex-overlays)))
              (should-not (overlay-buffer (cadr typst-overlays))))
          (should (overlay-buffer (cadr latex-overlays)))
          (should (overlay-buffer (cadr typst-overlays))))))))

(ert-deftest org-typst-preview-expands-typst-export-boundaries ()
  (with-temp-buffer
    (org-mode)
    (insert "Inline #(x^2#).\n\n#[sum_(i=1)^n i#]")
    (org-typst-preview--export-before-parsing 'typst)
    (let ((result (buffer-string)))
      (should (string-match-p
               (regexp-quote "Inline @@typst:$x^2$@@.") result))
      (should (string-match-p
               (regexp-quote
                "#+begin_export typst\n$ sum_(i=1)^n i $\n#+end_export")
               result)))))

(ert-deftest org-typst-preview-compiles-and-caches-svg ()
  :tags '(:integration)
  (skip-unless (executable-find org-typst-preview-executable))
  (let* ((cache-directory (make-temp-file "org-typst-preview-test-" t))
         (org-typst-preview-image-directory cache-directory)
         (org-format-latex-options
          '(:foreground "red" :background "white" :scale 1.0))
         (org-typst-preview--jobs (make-hash-table :test #'equal))
         (org-typst-preview--queue nil)
         (org-typst-preview--running 0)
         (deadline (+ (float-time) 15.0)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#[sum_(i=1)^n i = (n(n+1))/2#]")
          (let ((fragment
                 (car (org-typst-preview--fragments
                       (point-min) (point-max)))))
            (should (eq (org-typst-preview--schedule fragment) 'scheduled))
            (while (and (> (hash-table-count org-typst-preview--jobs) 0)
                        (< (float-time) deadline))
              (accept-process-output nil 0.05))
            (should (= (hash-table-count org-typst-preview--jobs) 0))
            (should (= (length (org-typst-preview--overlays
                                (point-min) (point-max)))
                       1))
            (let ((svg (car (directory-files cache-directory t "\\.svg\\'"))))
              (should svg)
              (with-temp-buffer
                (insert-file-contents svg)
                (should (search-forward "<svg" nil t))))
            (org-typst-preview-clear)
            (should (eq (org-typst-preview--schedule fragment) 'cached))))
      (delete-directory cache-directory t))))

(ert-deftest org-typst-preview-discards-edited-pending-preview ()
  :tags '(:integration)
  (skip-unless (executable-find org-typst-preview-executable))
  (let* ((cache-directory (make-temp-file "org-typst-preview-stale-" t))
         (org-typst-preview-image-directory cache-directory)
         (org-format-latex-options
          '(:foreground "red" :background "Transparent" :scale 1.0))
         (org-typst-preview--jobs (make-hash-table :test #'equal))
         (org-typst-preview--queue nil)
         (org-typst-preview--running 0)
         (deadline (+ (float-time) 15.0)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#(x#)")
          (org-typst-preview--schedule
           (car (org-typst-preview--fragments (point-min) (point-max))))
          (goto-char 3)
          (delete-char 1)
          (insert "y")
          (while (and (> (hash-table-count org-typst-preview--jobs) 0)
                      (< (float-time) deadline))
            (accept-process-output nil 0.05))
          (should-not (org-typst-preview--overlays
                       (point-min) (point-max))))
      (delete-directory cache-directory t))))

(ert-deftest org-typst-preview-keeps-latest-compiler-error ()
  :tags '(:integration)
  (skip-unless (executable-find org-typst-preview-executable))
  (let* ((cache-directory (make-temp-file "org-typst-preview-error-" t))
         (org-typst-preview-image-directory cache-directory)
         (org-typst-preview-preamble "#let invalid = (")
         (org-typst-preview-log-buffer
          (generate-new-buffer-name " *org-typst-preview-error*"))
         (org-format-latex-options
          '(:foreground "red" :background "Transparent" :scale 1.0))
         (org-typst-preview--jobs (make-hash-table :test #'equal))
         (org-typst-preview--queue nil)
         (org-typst-preview--running 0)
         (deadline (+ (float-time) 15.0)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (insert "#(x#)")
          (org-typst-preview--schedule
           (car (org-typst-preview--fragments (point-min) (point-max))))
          (while (and (> (hash-table-count org-typst-preview--jobs) 0)
                      (< (float-time) deadline))
            (accept-process-output nil 0.05))
          (should (get-buffer org-typst-preview-log-buffer))
          (should-not (org-typst-preview--overlays
                       (point-min) (point-max))))
      (when-let* ((buffer (get-buffer org-typst-preview-log-buffer)))
        (kill-buffer buffer))
      (delete-directory cache-directory t))))

(provide 'org-typst-preview-test)

;;; org-typst-preview-test.el ends here
