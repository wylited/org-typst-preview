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
         (deadline (+ (float-time) 120.0)))
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
         (deadline (+ (float-time) 120.0)))
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
         (deadline (+ (float-time) 120.0)))
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

;;; Dollar mode

(ert-deftest org-typst-preview-dollar-parses-inline-and-display ()
  (with-temp-buffer
    (org-mode)
    (insert "A $x$ and $ y^2 $ here.")
    (org-typst-preview-dollar-mode 1)
    (let ((fragments
           (org-typst-preview--fragments (point-min) (point-max))))
      (should (= (length fragments) 2))
      (should (equal (plist-get (car fragments) :value) "$x$"))
      (should-not (plist-get (car fragments) :block))
      (should (plist-get (car fragments) :dollar))
      (should (equal (plist-get (cadr fragments) :value) "$ y^2 $"))
      (should (plist-get (cadr fragments) :block))
      (should (plist-get (cadr fragments) :dollar)))))

(ert-deftest org-typst-preview-dollar-parses-multiline-display ()
  (with-temp-buffer
    (org-mode)
    (insert "$ x +\ny $")
    (org-typst-preview-dollar-mode 1)
    (let ((fragment
           (car (org-typst-preview--fragments (point-min) (point-max)))))
      (should (equal (plist-get fragment :value) "$ x +\ny $"))
      (should (plist-get fragment :block)))))

(ert-deftest org-typst-preview-dollar-ignores-non-fragments ()
  (with-temp-buffer
    (org-mode)
    (insert "Cost \\\\$5 and $10 total.\n\n"
            "#+begin_src typst\n$ x $\n#+end_src\n\n"
            "~$x$~ and =$x$= stay literal.\n\n"
            "Escaped \\$ x $ here.")
    (org-typst-preview-dollar-mode 1)
    (should-not (org-typst-preview--fragments (point-min) (point-max)))))

(ert-deftest org-typst-preview-dollar-coexists-with-boundaries ()
  (with-temp-buffer
    (org-mode)
    (insert "#(a#) and $x$ plus $ b $ end #[c#]")
    (org-typst-preview-dollar-mode 1)
    (let ((fragments
           (org-typst-preview--fragments (point-min) (point-max))))
      (should (equal (mapcar (lambda (fragment)
                               (list (plist-get fragment :value)
                                     (and (plist-get fragment :block) t)
                                     (and (plist-get fragment :dollar) t)))
                             fragments)
                     '(("$a$" nil nil)
                       ("$x$" nil t)
                       ("$ b $" t t)
                       ("$ c $" t nil)))))))

(ert-deftest org-typst-preview-dollar-off-keeps-latex-untouched ()
  (with-temp-buffer
    (org-mode)
    (insert "A $x$ here.")
    (should-not org-typst-preview-dollar-mode)
    (should-not (org-typst-preview--fragments (point-min) (point-max)))))

(ert-deftest org-typst-preview-dollar-previews-and-caches ()
  :tags '(:integration)
  (skip-unless (executable-find org-typst-preview-executable))
  (let* ((cache-directory (make-temp-file "org-typst-preview-dollar-" t))
         (org-typst-preview-image-directory cache-directory)
         (org-format-latex-options
          '(:foreground "red" :background "white" :scale 1.0))
         (org-typst-preview--jobs (make-hash-table :test #'equal))
         (org-typst-preview--queue nil)
         (org-typst-preview--running 0)
         (deadline (+ (float-time) 120.0)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (org-typst-preview-dollar-mode 1)
          (insert "Inline $x$ and display $ x^2 $ here.")
          (should (= (org-typst-preview--preview-region
                      (point-min) (point-max))
                     2))
          (while (and (> (hash-table-count org-typst-preview--jobs) 0)
                      (< (float-time) deadline))
            (accept-process-output nil 0.05))
          (should (= (hash-table-count org-typst-preview--jobs) 0))
          (let ((overlays (org-typst-preview--overlays
                           (point-min) (point-max))))
            (should (= (length overlays) 2))
            (dolist (overlay overlays)
              (should (overlay-get overlay 'display))))
          (dolist (svg (directory-files cache-directory t "\\.svg\\'"))
            (with-temp-buffer
              (insert-file-contents svg)
              (should (looking-at "<\\?xml\\|<svg"))))
          (org-typst-preview-clear)
          (should (= (org-typst-preview--preview-region
                      (point-min) (point-max))
                     2))
          (should (cl-every
                   (lambda (fragment) (eq fragment 'cached))
                   (let ((fragments
                          (org-typst-preview--fragments
                           (point-min) (point-max))))
                     (org-typst-preview-clear)
                     (mapcar #'org-typst-preview--schedule fragments)))))
      (delete-directory cache-directory t))))

(ert-deftest org-typst-preview-dollar-export-passes-inline-transforms-display ()
  (skip-unless (locate-library "ox-typst"))
  (with-temp-buffer
    (org-mode)
    (org-typst-preview-dollar-mode 1)
    (insert "Inline $x$ here.\n\n$ y^2 $")
    (org-typst-preview--export-before-parsing 'typst)
    (let ((result (buffer-string)))
      (should (string-match-p (regexp-quote "Inline $x$ here.") result))
      (should (string-match-p
               (regexp-quote
                "#+begin_export typst\n$ y^2 $\n#+end_export")
               result)))))

(ert-deftest org-typst-preview-dollar-double-parses-display ()
  (with-temp-buffer
    (org-mode)
    (let ((org-typst-preview-dollar-mode 1))
      (insert "$$sum_(i=1)^n i$$")
      (let ((fragment (org-typst-preview--dollar-fragment-at (point-min))))
        (should (plist-get fragment :block))
        (should (equal (plist-get fragment :value) "$ sum_(i=1)^n i $"))
        (should (equal (buffer-substring (plist-get fragment :begin)
                                         (plist-get fragment :end))
                       "$$sum_(i=1)^n i$$")))
      ;; Multi-line body, trimmed.
      (erase-buffer)
      (insert "$$ a\n   b $$")
      (should (equal (plist-get (org-typst-preview--dollar-fragment-at
                                 (point-min))
                                :body)
                     "a\n   b"))
      ;; Double dollars win over the inner `$x$'.
      (erase-buffer)
      (insert "$$x$$")
      (should (equal (plist-get (org-typst-preview--dollar-fragment-at
                                 (point-min))
                                :value)
                     "$ x $")))))

(ert-deftest org-typst-preview-dollar-double-pairs-like-org-latex ()
  (with-temp-buffer
    (org-mode)
    (let ((org-typst-preview-dollar-mode 1))
      ;; Org-equivalent ambiguity: the first pair wins; `\$' escapes.
      (insert "$$5 and $$10")
      (should (equal (plist-get (org-typst-preview--dollar-fragment-at
                                 (point-min))
                                :value)
                     "$ 5 and $")))))

(ert-deftest org-typst-preview-region-ending-inside-dollar-double-fragment ()
  (with-temp-buffer
    (org-mode)
    (let ((org-typst-preview-dollar-mode 1))
      (insert "$$ a +\nb $$ tail")
      (let (jobs)
        (cl-letf (((symbol-function 'org-typst-preview--start-job)
                   (lambda (job) (push job jobs))))
          (should (= 0 (org-typst-preview--preview-region
                        (point-min) (+ (point-min) 5))))
          (should (= 1 (org-typst-preview--preview-region
                        (point-min) (point-max))))
          (should (= 1 (length jobs))))))))

(ert-deftest org-typst-preview-dollar-off-ignores-double-dollars ()
  (with-temp-buffer
    (org-mode)
    (insert "$$x^2$$")
    (should-not org-typst-preview-dollar-mode)
    (should-not (org-typst-preview--fragments (point-min) (point-max)))))

(ert-deftest org-typst-preview-dollar-double-export-wraps-display ()
  (skip-unless (locate-library "ox-typst"))
  (with-temp-buffer
    (org-mode)
    (org-typst-preview-dollar-mode 1)
    (insert "$$x^2$$")
    (org-typst-preview--export-before-parsing 'typst)
    (let ((result (buffer-string)))
      (should (string-match-p
               (regexp-quote
                "#+begin_export typst\n$ x^2 $\n#+end_export")
               result))
      (should-not (string-match-p "\\$\\$" result)))))

(ert-deftest org-typst-preview-dollar-double-previews-display ()
  :tags '(:integration)
  (skip-unless (executable-find org-typst-preview-executable))
  (let* ((cache-directory (make-temp-file "org-typst-preview-double-" t))
         (org-typst-preview-image-directory cache-directory)
         (org-format-latex-options
          '(:foreground "red" :background "white" :scale 1.0))
         (org-typst-preview--jobs (make-hash-table :test #'equal))
         (org-typst-preview--queue nil)
         (org-typst-preview--running 0)
         (deadline (+ (float-time) 120.0)))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (org-typst-preview-dollar-mode 1)
          (insert "Display $$x^2$$ inline.")
          (should (= (org-typst-preview--preview-region
                      (point-min) (point-max))
                     1))
          (while (and (> (hash-table-count org-typst-preview--jobs) 0)
                      (< (float-time) deadline))
            (accept-process-output nil 0.05))
          (should (= (hash-table-count org-typst-preview--jobs) 0))
          (let ((overlay (car (org-typst-preview--overlays
                               (point-min) (point-max)))))
            (should overlay)
            (should (string-prefix-p "\n"
                                     (overlay-get overlay 'before-string)))
            (should (string-match-p
                     "align-to"
                     (prin1-to-string
                      (overlay-get overlay 'before-string))))
            (should (overlay-get overlay 'display)))
          (should (cl-some
                   (lambda (svg)
                     (with-temp-buffer
                       (insert-file-contents svg)
                       (looking-at "<\\?xml\\|<svg")))
                   (directory-files cache-directory t "\\.svg\\'"))))
      (delete-directory cache-directory t))))

(ert-deftest org-typst-preview-schedules-with-unfontified-face ()
  (with-temp-buffer
    (org-mode)
    (let ((org-format-latex-options
           '(:foreground auto :background "white" :scale 1.0)))
      (insert "#(x#)")
      ;; Unfontified text yields a nil face from `face-at-point'; building
      ;; the preview source used to signal "Invalid face" from
      ;; `face-attribute'.  The real scheduling path (not a stub) must
      ;; resolve the `default' face and enqueue the job.
      (should-not (face-at-point (point-min)))
      (let (jobs)
        (cl-letf (((symbol-function 'org-typst-preview--start-job)
                   (lambda (job) (push job jobs))))
          (should (memq (org-typst-preview--schedule
                         (org-typst-preview--fragment-at (point-min)))
                        '(scheduled cached)))
          (should (= 1 (length jobs))))))))

(ert-deftest org-typst-preview-region-ending-inside-dollar-fragment ()
  (with-temp-buffer
    (org-mode)
    (let ((org-typst-preview-dollar-mode 1))
      (insert "$ x +\ny $ tail $x$")
      ;; A region that cuts a fragment must neither error ("Invalid search
      ;; bound") nor return fragments extending past END.
      (let (jobs)
        (cl-letf (((symbol-function 'org-typst-preview--start-job)
                   (lambda (job) (push job jobs))))
          (should (= 0 (org-typst-preview--preview-region
                        (point-min) (+ (point-min) 5))))
          (should (= 2 (org-typst-preview--preview-region
                        (point-min) (point-max))))
          (should (= 2 (length jobs))))))))

(ert-deftest org-typst-preview-auto-run-skips-untrusted-content ()
  (with-temp-buffer
    (org-mode)
    (insert "#(x#)")
    (org-typst-preview-auto-mode 1)
    (let ((untrusted-content t)
          (org--latex-preview-when-risky nil))
      (org-typst-preview-test--with-renderers
        (org-typst-preview--auto-run (current-buffer)
                                     (copy-marker (point-min) t))
        (should-not typst-fragments)))))

(ert-deftest org-typst-preview-font-size-auto-and-override ()
  ;; Auto: derived from the default face, always a positive number.
  (should (cl-plusp (org-typst-preview--font-size)))
  ;; Explicit numeric override wins.
  (let ((org-typst-preview-font-size 13.0))
    (should (= 13.0 (org-typst-preview--font-size)))))

(ert-deftest org-typst-preview-block-overlay-own-line-and-centered ()
  (with-temp-buffer
    (org-mode)
    (let ((org-typst-preview-dollar-mode 1))
      ;; Mid-line display math: forced onto its own, centered line.
      (insert "text $ x $ more")
      (let* ((fragment (org-typst-preview--dollar-fragment-at 6))
             (overlay (org-typst-preview--make-overlay fragment)))
        (should (string-prefix-p "\n" (overlay-get overlay 'before-string)))
        (should (string-match-p
                 "align-to"
                 (prin1-to-string (overlay-get overlay 'before-string))))
        (should (equal "\n" (overlay-get overlay 'after-string))))
      ;; Already alone on its line: no extra blank lines.
      (erase-buffer)
      (insert "$ x $\n")
      (let* ((fragment (org-typst-preview--dollar-fragment-at 1))
             (overlay (org-typst-preview--make-overlay fragment)))
        (should-not (string-prefix-p "\n" (overlay-get overlay 'before-string)))
        (should-not (overlay-get overlay 'after-string))))))

(ert-deftest org-typst-preview-image-scale-follows-text-scale ()
  (let ((org-typst-preview-scale 1.0))
    (should (= 1.0 (plist-get (cdr (org-typst-preview--image-spec "f.svg"))
                              :scale)))
    (cl-progv '(text-scale-mode text-scale-mode-amount text-scale-mode-step)
        (list t 1 1.2)
      (should (< 1.19
                 (plist-get (cdr (org-typst-preview--image-spec "f.svg"))
                            :scale)
                 1.21)))))

(ert-deftest org-typst-preview-image-spec-compensates-font-change ()
  ;; Rendered at 18pt, buffer now at 9pt: image halves.  A numeric
  ;; org-typst-preview-font-size pins the current size.
  (let ((org-typst-preview-font-size 9.0)
        (org-typst-preview-scale 1.0))
    (should (= 0.5 (plist-get
                    (cdr (org-typst-preview--image-spec "f.svg" 18.0))
                    :scale)))
    ;; No recorded height: no ratio.
    (should (= 1.0 (plist-get
                    (cdr (org-typst-preview--image-spec "f.svg"))
                    :scale)))))

(ert-deftest org-typst-preview-source-with-unresolvable-colors ()
  (with-temp-buffer
    (org-mode)
    ;; Faces report literal "unspecified-*" strings before a theme loads;
    ;; `color-values' returns nil for them, which used to build `rgb()'
    ;; (invalid Typst, compiler exit 1).  Must fall back instead.
    (let ((org-format-latex-options
           '(:foreground "unspecified-fg" :background "unspecified-bg" :scale 1.0)))
      (let ((source (org-typst-preview--source "$x$" 'default)))
        (should-not (string-match-p "rgb()" source))
        (should (string-match-p "fill: black" source))
        (should (string-match-p "fill: none" source))))))

(provide 'org-typst-preview-test)

;;; org-typst-preview-test.el ends here
