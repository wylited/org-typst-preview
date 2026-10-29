;;; org-typst-preview.el --- Async Typst previews for Org math -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Roi Fe Wu

;; Author: Roi Fe Wu <roifewu@gmail.com>
;; Maintainer: Roi Fe Wu <roifewu@gmail.com>
;; Version: 0.3.1
;; Package-Requires: ((emacs "30.1"))
;; Keywords: outlines, tex, typst, multimedia
;; URL: https://github.com/roife/org-typst-preview

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Render native Typst math in Org buffers through the interface used by
;; `org-latex-preview'.  Inline formulae use `#(...#)' and display formulae use
;; `#[...#]', so Typst and LaTeX can coexist in one Org document.
;;
;; `org-typst-preview-mode' extends Org's preview commands to handle Typst
;; alongside LaTeX with `C-c C-x C-l', and expands Typst boundaries when
;; exporting through a Typst backend.  Rendering uses asynchronous Typst
;; processes, Org's color and cache settings, and Org's formula overlay
;; conventions.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'subr-x)

(defgroup org-typst-preview nil
  "Asynchronous Typst previews for Org math fragments."
  :group 'org-latex
  :prefix "org-typst-preview-")

(defcustom org-typst-preview-executable "typst"
  "Typst executable used to render previews."
  :group 'org-typst-preview
  :type 'string)

(defcustom org-typst-preview-image-directory nil
  "Directory in which to cache generated SVG previews.

Nil reuses `org-preview-latex-image-directory'."
  :group 'org-typst-preview
  :type '(choice (const :tag "Reuse Org LaTeX preview directory" nil)
                 directory))

(defcustom org-typst-preview-preamble ""
  "Typst code inserted before each preview formula."
  :group 'org-typst-preview
  :type 'string)

(defcustom org-typst-preview-font-size nil
  "Base preview font size in points.
Nil derives it from the default face's height at render time (Typst
renders 1pt as one logical pixel, so the face's point size matches the
surrounding text) and follows font and theme changes."
  :group 'org-typst-preview
  :type '(choice (const :tag "Match the default face height" nil)
                 number))

(defcustom org-typst-preview-font-family nil
  "Optional Typst font family for preview text."
  :group 'org-typst-preview
  :type '(choice (const :tag "Typst default" nil) string))

(defcustom org-typst-preview-scale nil
  "Typst preview scale.

Nil reuses the `:scale' value in `org-format-latex-options'."
  :group 'org-typst-preview
  :type '(choice (const :tag "Use org-format-latex-options" nil) number))

(defcustom org-typst-preview-page-margin "0pt"
  "Margin around each tightly cropped preview."
  :group 'org-typst-preview
  :type 'string)

(defcustom org-typst-preview-max-processes 4
  "Maximum number of concurrent Typst processes."
  :group 'org-typst-preview
  :type 'integer)

(defcustom org-typst-preview-auto-delay 0.45
  "Idle delay before automatically previewing edited math."
  :group 'org-typst-preview
  :type 'number)

(defcustom org-typst-preview-log-buffer "*Org Preview Typst Output*"
  "Buffer used for the latest Typst compiler error."
  :group 'org-typst-preview
  :type 'string)

(defcustom org-typst-preview-export-to-typst t
  "Whether Typst export expands preview boundaries."
  :group 'org-typst-preview
  :type 'boolean)

(cl-defstruct
    (org-typst-preview--job (:constructor org-typst-preview--job-create))
  output source directory overlays input render)

(defvar org-typst-preview--jobs (make-hash-table :test #'equal))
(defvar org-typst-preview--queue nil)
(defvar org-typst-preview--running 0)
(defvar-local org-typst-preview--auto-timer nil)

(defconst org-typst-preview--cache-version 3)

;; Defined by `define-minor-mode' below; declared here for the scanner.
(defvar org-typst-preview-dollar-mode)

;; Typst treats `$ x $' (whitespace hugging the inner delimiters) as display
;; math and `$x$' as inline math.  Org never parses the whitespace form, so it
;; is matched with a dedicated regexp; the inline form reuses Org's canonical
;; `$' fragment regexp from `org-latex-regexps'.
(defconst org-typst-preview--dollar-display-regexp
  "\\$[ \t\n][^$]*[ \t\n]\\$"
  "Regexp matching Typst display math delimited by whitespace dollars.")

(defun org-typst-preview--boundary-context-p ()
  "Return non-nil when a Typst or dollar boundary may start at point."
  (and (not (eq (char-before) ?\\))
       (not (org-element-lineage
             (org-element-context)
             '(code comment comment-block example-block export-block
                    fixed-width inline-src-block keyword node-property
                    property-drawer src-block verbatim)
             t))))

(defun org-typst-preview--dollar-double-fragment (position &optional limit)
  "Return the `$$...$$' display fragment starting at POSITION, or nil.
LIMIT bounds the match, mirroring `re-search-forward' semantics.
`$$body$$' compiles as `$ body $'."
  (goto-char position)
  (when (and (looking-at (or (nth 1 (assoc "$$" org-latex-regexps))
                             "\\$\\$\\(?:.\\|\n\\)*?\\$\\$"))
             (<= (match-end 0) (or limit (point-max))))
    (let* ((end (match-end 0))
           (body (string-trim (buffer-substring-no-properties
                               (+ position 2) (- end 2)))))
      (list :begin position
            :end end
            :body body
            :value (concat "$ " body " $")
            :block t :dollar t))))

(defun org-typst-preview--dollar-display-fragment (position &optional limit)
  "Return the display dollar fragment starting at POSITION, or nil.
LIMIT bounds the match, mirroring `re-search-forward' semantics."
  (goto-char position)
  (when (and (looking-at org-typst-preview--dollar-display-regexp)
             (<= (match-end 0) (or limit (point-max))))
    (list :begin position
          :end (match-end 0)
          :body (buffer-substring-no-properties
                 (+ position 2) (- (match-end 0) 2))
          :value (buffer-substring-no-properties position (match-end 0))
          :block t :dollar t)))

(defun org-typst-preview--dollar-inline-fragment (position &optional limit)
  "Return the inline dollar fragment starting at POSITION, or nil.

Reuses Org's canonical `$' regexps (the single-character `$x$' entry
and the general entry) so the fragment matches what Org recognizes.
LIMIT bounds the match, mirroring `re-search-forward' semantics."
  (let ((case-fold-search nil)
        (entries '("$1" "$"))
        fragment)
    (while (and entries (not fragment))
      (goto-char
       (if (and (> position (point-min))
                (not (memq (char-before position) '(?\\ ?\n ?$))))
           (1- position)
         position))
      (when (and (looking-at (nth 1 (assoc (car entries) org-latex-regexps)))
                 (= (match-beginning
                     (nth 2 (assoc (car entries) org-latex-regexps)))
                    position)
                 (<= (match-end 2) (or limit (point-max))))
        (setq fragment
              (list :begin position
                    :end (match-end 2)
                    :body (buffer-substring-no-properties
                           (1+ position) (1- (match-end 2)))
                    :value (buffer-substring-no-properties
                            position (match-end 2))
                    :block nil :dollar t)))
      (setq entries (cdr entries)))
    fragment))

(defun org-typst-preview--dollar-fragment-at (position &optional limit)
  "Parse a native Typst dollar fragment beginning at POSITION.

Return a fragment plist with the dollar text as `:value', `:block' set
for the display forms and `:dollar' set for all forms.  LIMIT bounds
the match, mirroring `re-search-forward' semantics."
  (save-excursion
    (goto-char position)
    (when (and (eq (char-after) ?$)
               (org-typst-preview--boundary-context-p))
      (or (org-typst-preview--dollar-double-fragment position limit)
          (org-typst-preview--dollar-display-fragment position limit)
          (org-typst-preview--dollar-inline-fragment position limit)))))

(defun org-typst-preview--fragment-at (position &optional limit)
  "Parse a Typst fragment beginning at POSITION, stopping before LIMIT."
  (save-excursion
    (goto-char position)
    (let* ((opener (buffer-substring-no-properties
                    position (min (+ position 2) (point-max))))
           (block-p (equal opener "#["))
           (closer (cond ((equal opener "#(") "#)")
                         (block-p "#]"))))
      (or (when (and closer
                     (org-typst-preview--boundary-context-p))
        (forward-char 2)
        (let (closing)
          (while (and (not closing)
                      (search-forward closer (or limit (point-max)) t))
            (unless (eq (char-before (- (point) 2)) ?\\)
              (setq closing (point))))
          (when closing
            (let ((body (string-trim
                         (buffer-substring-no-properties
                          (+ position 2) (- closing 2)))))
              (list :begin position
                    :end closing
                    :body body
                    :value (if block-p
                               (concat "$ " body " $")
                             (concat "$" body "$"))
                    :block block-p)))))
          (when (and org-typst-preview-dollar-mode
                     (eq (char-after position) ?$))
            (org-typst-preview--dollar-fragment-at position limit))))))

(defun org-typst-preview--fragment-at-point ()
  "Return the Typst fragment containing point."
  (let* ((origin (point))
         (fragment (org-typst-preview--fragment-at origin)))
    (save-excursion
      (while (and (not fragment)
                  (re-search-backward
                   (if org-typst-preview-dollar-mode "[#$]" "#") nil t))
        (when-let* ((candidate (org-typst-preview--fragment-at (point))))
          (when (<= (plist-get candidate :begin)
                    origin
                    (plist-get candidate :end))
            (setq fragment candidate)))))
    fragment))

(defun org-typst-preview--fragments (begin end)
  "Return Typst fragments wholly between BEGIN and END."
  (let (fragments)
    (save-excursion
      (goto-char begin)
      (while (re-search-forward
              (if org-typst-preview-dollar-mode "#[([]\\|\\$" "#[([]")
              end t)
        (if (eq (char-after (match-beginning 0)) ?#)
            (when-let* ((fragment
                         (org-typst-preview--fragment-at
                          (match-beginning 0) end)))
              (push fragment fragments)
              (goto-char (plist-get fragment :end)))
          (when-let* ((fragment
                       (org-typst-preview--dollar-fragment-at
                        (match-beginning 0) end)))
            (push fragment fragments)
            (goto-char (plist-get fragment :end))))))
    (nreverse fragments)))

(defun org-typst-preview--color-rgb (color fallback)
  "Return a Typst rgb() call for COLOR, or FALLBACK if unresolvable.
COLOR may be a color name, an RGB string, or an unspecified face
attribute (e.g. \"unspecified-fg\" in frames before the theme loads);
`color-values' returns nil for those, which must not yield rgb()."
  (let ((values (and (stringp color) (color-values color))))
    (if values
        (format "rgb(%s)"
                (mapconcat
                 (lambda (component)
                   (number-to-string (round (/ component 257.0))))
                 values ","))
      fallback)))
(defvar text-scale-mode)
(defvar text-scale-mode-step)
(defvar text-scale-mode-amount)

(defun org-typst-preview--font-size ()
  "Typst font size in points for the current buffer.
With a numeric `org-typst-preview-font-size', use it; otherwise derive
from the default face's height: Typst renders 1pt as one logical
pixel, so the face's point size matches the surrounding text."
  (or (and (numberp org-typst-preview-font-size)
           org-typst-preview-font-size)
      (let ((height (face-attribute 'default :height nil 'default)))
        (if (and (numberp height) (> height 0))
            (/ height 10.0)
          11.0))))

(defun org-typst-preview--image-spec (file &optional rendered-height)
  "Return an image spec for FILE, scaled like the surrounding text.
RENDERED-HEIGHT is the font size (pt) FILE was rendered at; when the
buffer's font has since changed, ratio-scale the image to match (SVGs
stay crisp).  Also follows `text-scale-adjust'."
  (let* ((base (or org-typst-preview-scale 1.0))
         (zoom (if (and (boundp 'text-scale-mode) text-scale-mode
                        (boundp 'text-scale-mode-step)
                        (boundp 'text-scale-mode-amount))
                   (expt text-scale-mode-step text-scale-mode-amount)
                 1.0))
         (ratio (if (and (numberp rendered-height)
                         (cl-plusp rendered-height))
                    (float (/ (org-typst-preview--font-size)
                              rendered-height))
                  1.0)))
    (list 'image :type 'svg :file file :ascent 'center
          :scale (* base zoom ratio))))

(defun org-typst-preview--apply-image (overlay file)
  "Make OVERLAY display FILE, remembering FILE and its render height."
  (overlay-put overlay 'org-typst-preview-file file)
  (overlay-put overlay 'org-typst-preview-rendered-height
               (org-typst-preview--font-size))
  (overlay-put overlay 'display
               (org-typst-preview--image-spec
                file (overlay-get overlay 'org-typst-preview-rendered-height))))

(defun org-typst-preview--refresh-text-scale ()
  "Rescale preview images after zoom or font-size changes.
Runs from `text-scale-mode-hook' and `post-command-hook'; returns
non-nil when any overlay was updated."
  (let ((updated nil))
    (dolist (overlay (org-typst-preview--overlays (point-min) (point-max)))
      (when-let* ((file (overlay-get overlay 'org-typst-preview-file)))
        (let ((spec (org-typst-preview--image-spec
                     file
                     (overlay-get overlay
                                  'org-typst-preview-rendered-height))))
          (unless (equal spec (overlay-get overlay 'display))
            (overlay-put overlay 'display spec)
            (setq updated t)))))
    updated))

(defun org-typst-preview--source (value face)
  "Return a complete Typst preview document for VALUE at FACE."
  (let* ((foreground-option
          (plist-get org-format-latex-options :foreground))
         (background-option
          (plist-get org-format-latex-options :background))
         (foreground
          (pcase foreground-option
            ('auto (face-attribute face :foreground nil 'default))
            ('default (face-attribute 'default :foreground nil 'default))
            (_ foreground-option)))
         (background
          (pcase background-option
            ('auto (face-attribute face :background nil 'default))
            ('default (face-attribute 'default :background nil 'default))
            (_ background-option)))
         (scale (or org-typst-preview-scale
                    (plist-get org-format-latex-options :scale)
                    1.0))
         (font (if org-typst-preview-font-family
                   (format ", font: %S" org-typst-preview-font-family)
                 "")))
    (concat
     (format "#set page(width: auto, height: auto, margin: %s, fill: %s)\n"
             org-typst-preview-page-margin
             (if (equal background "Transparent")
                 "none"
               (org-typst-preview--color-rgb background "none")))
     (format "#set text(size: %.3fpt, fill: %s%s)\n"
            (* (org-typst-preview--font-size) scale)
             (org-typst-preview--color-rgb foreground "black")
             font)
     "#show math.equation: set text(top-edge: \"bounds\", bottom-edge: \"bounds\")\n"
     (unless (string-empty-p org-typst-preview-preamble)
       (concat org-typst-preview-preamble "\n"))
     value "\n")))

(defun org-typst-preview--overlays (begin end)
  "Return Typst preview overlays between BEGIN and END."
  (cl-remove-if-not
   (lambda (overlay) (overlay-get overlay 'org-typst-preview))
   (overlays-in begin end)))

(defun org-typst-preview--make-overlay (fragment &optional image)
  "Create a preview overlay for FRAGMENT, displaying IMAGE when non-nil.
Display (block) fragments get their own line, centered."
  (let* ((begin (plist-get fragment :begin))
         (end (plist-get fragment :end))
         (block (plist-get fragment :block))
         (overlay (make-overlay begin end)))
    (overlay-put overlay 'org-overlay-type 'org-latex-overlay)
    (overlay-put overlay 'org-typst-preview t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks
                 (list (lambda (changed _after _begin _end &optional _length)
                         (delete-overlay changed))))
    (when block
      ;; Put display math on its own line, centered: `(0.5 . image)'
      ;; is half the width of the image that follows, so centering
      ;; holds at any size, scale, or window width.
      (overlay-put overlay 'before-string
                   (concat
                    (unless (save-excursion (goto-char begin) (bolp)) "\n")
                    (propertize " " 'display
                                '(space :align-to (- center (0.5 . image))))))
      (overlay-put overlay 'after-string
                   (unless (save-excursion (goto-char end) (eolp)) "\n")))
    (when image
      (org-typst-preview--apply-image overlay image))
    overlay))

(defun org-typst-preview--finish (process event)
  "Finish the Typst job belonging to PROCESS, described by EVENT."
  (when (memq (process-status process) '(exit signal))
    (let* ((job (process-get process 'org-typst-preview-job))
           (success (and (eq (process-status process) 'exit)
                         (zerop (process-exit-status process))))
           (output (org-typst-preview--job-output job))
           ;; The process buffer may already be gone; never fall back to
           ;; killing or inserting into whatever buffer happens to be current.
           (process-buffer (or (process-buffer process)
                               (get-buffer-create " *org-typst-preview*"))))
      (condition-case error-data
          (when success
            (rename-file (org-typst-preview--job-render job) output t)
            (dolist (overlay (org-typst-preview--job-overlays job))
              (when (overlay-buffer overlay)
                (org-typst-preview--apply-image overlay output))))
        (error
         (setq success nil)
         (with-current-buffer process-buffer
           (goto-char (point-max))
           (insert "\n" (error-message-string error-data) "\n"))))
      ;; Reporting must not prevent queue bookkeeping below.
      (condition-case nil
          (progn
            (unless success
              (mapc (lambda (overlay)
                      (when (overlay-buffer overlay) (delete-overlay overlay)))
                    (org-typst-preview--job-overlays job))
              (when-let* ((old (get-buffer org-typst-preview-log-buffer)))
                (kill-buffer old))
              (with-current-buffer process-buffer
                (goto-char (point-min))
                (insert (format "%s\n\n" (string-trim event)))
                (compilation-mode)
                (rename-buffer org-typst-preview-log-buffer))
              (display-warning 'org-typst-preview
                               (format "Typst preview failed; see %s"
                                       org-typst-preview-log-buffer)))
            (when success (kill-buffer process-buffer)))
        (error nil))
      (ignore-errors
        (delete-file (org-typst-preview--job-input job))
        (when (file-exists-p (org-typst-preview--job-render job))
          (delete-file (org-typst-preview--job-render job))))
      (remhash output org-typst-preview--jobs)
      (cl-decf org-typst-preview--running)
      (org-typst-preview--start-jobs))))

(defun org-typst-preview--start-job (job)
  "Start asynchronous Typst JOB."
  (let* ((output (org-typst-preview--job-output job))
         (temporary-file-directory
          (file-name-as-directory (org-typst-preview--job-directory job)))
         (input (make-temp-file ".org-typst-preview-" nil ".typ"))
         (render (let ((temporary-file-directory
                        (file-name-directory output)))
                   (make-temp-file ".org-typst-preview-" nil ".svg")))
         (process-buffer (generate-new-buffer " *org-typst-preview*")))
    (with-temp-file input
      (insert (org-typst-preview--job-source job)))
    (setf (org-typst-preview--job-input job) input
          (org-typst-preview--job-render job) render)
    ;; Run the compiler locally: the job directory is local even for remote
    ;; files, so never let a remote `default-directory' send typst and the
    ;; local temp paths to a remote host.
    (let ((process nil))
      (condition-case error-data
          (let ((default-directory (org-typst-preview--job-directory job)))
            (setq process
                  (make-process
                   :name (format "org-typst-preview-%s"
                                 (substring (sha1 output) 0 10))
                   :buffer process-buffer
                   :command (list org-typst-preview-executable
                                  "compile" "--format" "svg" input render)
                   :coding 'utf-8-unix
                   :noquery t
                   :sentinel #'org-typst-preview--finish)))
        (error
         (remhash output org-typst-preview--jobs)
         (delete-file input)
         (dolist (ov (org-typst-preview--job-overlays job))
           (when (overlay-buffer ov) (delete-overlay ov)))
         (kill-buffer process-buffer)
         (display-warning
          'org-typst-preview
          (format "Could not start %s: %s"
                  org-typst-preview-executable
                  (error-message-string error-data)))
         (setq process nil)))
      (when process
        (process-put process 'org-typst-preview-job job)
        (cl-incf org-typst-preview--running)))))

(defun org-typst-preview--start-jobs ()
  "Start queued jobs up to `org-typst-preview-max-processes'."
  (while (and org-typst-preview--queue
              (< org-typst-preview--running org-typst-preview-max-processes))
    (let ((job (pop org-typst-preview--queue)))
      (setf (org-typst-preview--job-overlays job)
            (cl-remove-if-not #'overlay-buffer
                              (org-typst-preview--job-overlays job)))
      (if (org-typst-preview--job-overlays job)
          (org-typst-preview--start-job job)
        (remhash (org-typst-preview--job-output job)
                 org-typst-preview--jobs)))))

(defun org-typst-preview--schedule (fragment)
  "Schedule FRAGMENT for rendering and return its state."
  (let ((begin (plist-get fragment :begin))
        (end (plist-get fragment :end)))
    (if (org-typst-preview--overlays begin end)
        'existing
      (let* ((source (org-typst-preview--source
                      (plist-get fragment :value)
                      ;; `face-at-point' has no default argument and returns
                      ;; nil on unfontified text; `face-attribute' would then
                      ;; signal "Invalid face" for `:foreground auto'.
                      (or (face-at-point begin) 'default)))
             (directory
              (expand-file-name
               (or org-typst-preview-image-directory
                   org-preview-latex-image-directory)
               (if (and (buffer-file-name (buffer-base-buffer))
                        (file-remote-p
                         (buffer-file-name (buffer-base-buffer))))
                   temporary-file-directory
                 default-directory)))
             (source-directory
              (if (file-remote-p default-directory)
                  temporary-file-directory
                default-directory))
             (output
              (expand-file-name
               (format "org-typst-%s.svg"
                       (sha1 (prin1-to-string
                              (list org-typst-preview--cache-version
                                    source-directory source))))
               directory)))
        (make-directory directory t)
        (if (file-exists-p output)
            (progn
              (org-typst-preview--make-overlay fragment output)
              'cached)
          (let ((overlay (org-typst-preview--make-overlay fragment)))
            (if-let* ((job (gethash output org-typst-preview--jobs)))
                (push overlay (org-typst-preview--job-overlays job))
              (let ((job (org-typst-preview--job-create
                          :output output
                          :source source
                          :directory source-directory
                          :overlays (list overlay))))
                (puthash output job org-typst-preview--jobs)
                (setq org-typst-preview--queue
                      (nconc org-typst-preview--queue (list job)))
                (org-typst-preview--start-jobs)))
            'scheduled))))))

(defun org-typst-preview--preview-region (begin end)
  "Preview Typst fragments between BEGIN and END and return their count."
  (let ((fragments (org-typst-preview--fragments begin end)))
    (when fragments
      (org-check-external-command
       org-typst-preview-executable
       "Install Typst or customize `org-typst-preview-executable'.")
      (mapc #'org-typst-preview--schedule fragments))
    (length fragments)))

(defun org-typst-preview--section-bounds ()
  "Return the bounds of the current Org section."
  (list
   (if (org-before-first-heading-p)
       (point-min)
     (save-excursion
       (org-with-limited-levels (org-back-to-heading t) (point))))
   (org-with-limited-levels (org-entry-end-position))))

;;;###autoload
(defun org-typst-preview-clear (&optional begin end)
  "Remove Typst previews between BEGIN and END, or in the whole buffer."
  (interactive)
  (let ((overlays (org-typst-preview--overlays
                   (or begin (point-min)) (or end (point-max)))))
    (mapc #'delete-overlay overlays)
    overlays))

;;;###autoload
(defun org-typst-preview--delegate-latex (argument)
  "Delegate ARGUMENT to `org-latex-preview', degrading on failure.

A missing LaTeX toolchain (no latex/dvipng/dvisvgm) must not abort the
shared command before the Typst half runs; surface a warning instead."
  (condition-case err
      (org-latex-preview argument)
    (error
     (display-warning
      'org-typst-preview
      (format "LaTeX preview skipped (%s); use \\(...\\) only if LaTeX is installed"
              (error-message-string err)))
     nil)))

(defun org-typst-preview (&optional argument)
  "Toggle Typst and LaTeX previews with Org's prefix ARGUMENT semantics.

At point, toggle the Typst or LaTeX formula there.  With an active region,
preview both kinds of formula in the region; otherwise preview the section.
One universal prefix clears the region or current section, two preview the
whole buffer, and three clear the whole buffer.  LaTeX previews are handled
by `org-latex-preview'."
  (interactive "P")
  (cond
   ((not (display-graphic-p))
    ;; Keep Org's LaTeX preview working on TTY instead of disabling it.
    (org-latex-preview argument))
   ((and untrusted-content (not org--latex-preview-when-risky)) nil)
   ((member argument '((4) (64)))
    (org-typst-preview--delegate-latex argument)
    (if (equal argument '(64))
        (org-typst-preview-clear)
      (apply #'org-typst-preview-clear
             (if (use-region-p)
                 (list (region-beginning) (region-end))
               (org-typst-preview--section-bounds))))
    (message "Typst previews cleared"))
   ((equal argument '(16))
    (org-typst-preview--delegate-latex argument)
    (message "%d Typst previews requested"
             (org-typst-preview--preview-region (point-min) (point-max))))
   ((use-region-p)
    (org-typst-preview--delegate-latex argument)
    (message "%d Typst previews requested"
             (org-typst-preview--preview-region
              (region-beginning) (region-end))))
   ((when-let* ((fragment (org-typst-preview--fragment-at-point)))
      (let ((begin (plist-get fragment :begin))
            (end (plist-get fragment :end)))
        (if (org-typst-preview--overlays begin end)
            (org-typst-preview-clear begin end)
          (org-check-external-command org-typst-preview-executable)
          (org-typst-preview--schedule fragment))
        t)))
   ((org-element-type-p (org-element-context)
                        '(latex-environment latex-fragment))
    (org-latex-preview argument))
   (t
    (org-typst-preview--delegate-latex argument)
    (pcase-let ((`(,begin ,end) (org-typst-preview--section-bounds)))
      (message "%d Typst previews requested"
               (org-typst-preview--preview-region begin end))))))

(defun org-typst-preview--export-before-parsing (backend)
  "Expand preview boundaries before parsing Typst export BACKEND."
  (when (and org-typst-preview-export-to-typst
             (eq backend 'typst))
    (dolist (fragment
             (nreverse (org-typst-preview--fragments
                        (point-min) (point-max))))
      (let ((begin (plist-get fragment :begin))
            (end (plist-get fragment :end))
            (value (plist-get fragment :value)))
        (cond
         ;; Inline dollar math survives ox-typst's DEFAULT conversion
         ;; (`org-typst-from-latex-fragment' is naive by default: `$x$' is
         ;; exported verbatim).  Custom settings that drop or LaTeX-convert
         ;; fragments will treat Typst text accordingly; only the display
         ;; form needs shielding (Org's script parser mangles bare `$ x $'),
         ;; leave the text untouched.
         ((and (plist-get fragment :dollar)
               (not (plist-get fragment :block))))
         (t
          (goto-char begin)
          (delete-region begin end)
          (if (plist-get fragment :block)
              (insert "\n#+begin_export typst\n" value
                      "\n#+end_export\n")
            (insert "@@typst:" value "@@"))))))))

(defvar org-typst-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap org-latex-preview] #'org-typst-preview)
    (define-key map [remap org-toggle-latex-fragment] #'org-typst-preview)
    map))

;;;###autoload
(define-minor-mode org-typst-preview-mode
  "Preview Typst alongside LaTeX with Org's commands and enable Typst export."
  :lighter " Typst∑"
  :keymap org-typst-preview-mode-map
  (if org-typst-preview-mode
      (progn
        (add-hook 'org-export-before-parsing-functions
                  #'org-typst-preview--export-before-parsing nil t)
        ;; Previews zoom with C-x C-+/C-x C-- and re-scale when the
        ;; buffer font itself changes; the refresh early-outs when the
        ;; computed spec is unchanged.
        (add-hook 'text-scale-mode-hook
                  #'org-typst-preview--refresh-text-scale nil t)
        (add-hook 'post-command-hook
                  #'org-typst-preview--refresh-text-scale nil t))
    (remove-hook 'org-export-before-parsing-functions
                 #'org-typst-preview--export-before-parsing t)
    (remove-hook 'text-scale-mode-hook
                 #'org-typst-preview--refresh-text-scale t)
    (remove-hook 'post-command-hook
                 #'org-typst-preview--refresh-text-scale t)))

(defun org-typst-preview--auto-run (buffer marker)
  "Preview the fragment near MARKER in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq org-typst-preview--auto-timer nil)
      (when (and (bound-and-true-p org-typst-preview-auto-mode)
                 (not (and untrusted-content
                           (not org--latex-preview-when-risky))))
        (save-excursion
          (goto-char marker)
          (when-let* ((fragment (org-typst-preview--fragment-at-point)))
            (org-typst-preview--schedule fragment)))))
    (set-marker marker nil)))

(defun org-typst-preview--after-change (begin _end _old-length)
  "Schedule auto-preview after a change at BEGIN."
  (when org-typst-preview--auto-timer
    (cancel-timer org-typst-preview--auto-timer))
  (setq org-typst-preview--auto-timer
        (run-with-idle-timer
         org-typst-preview-auto-delay nil
         #'org-typst-preview--auto-run
         (current-buffer) (copy-marker begin t))))

;;;###autoload
(define-minor-mode org-typst-preview-auto-mode
  "Automatically preview edited Typst formulae after an idle delay."
  :lighter " Typst↻"
  (if org-typst-preview-auto-mode
      (progn
        (org-typst-preview-mode 1)
        (add-hook 'after-change-functions
                  #'org-typst-preview--after-change nil t))
    (remove-hook 'after-change-functions
                 #'org-typst-preview--after-change t)
    (when org-typst-preview--auto-timer
      (cancel-timer org-typst-preview--auto-timer)
      (setq org-typst-preview--auto-timer nil))))

;;;###autoload
(define-minor-mode org-typst-preview-dollar-mode
  "Treat Org `$...$' fragments as native Typst math.

`$x$' (no whitespace inside the delimiters) is inline math and `$ x $'
(whitespace inside both edges) is display math, exactly as Typst itself
decides; the fragment text is passed to the compiler verbatim.  There is
no `$$...$$' support.  While the mode is enabled, `$...$' means Typst,
not LaTeX; LaTeX stays available through `\\(...\\)' and latex
environments.  Enable `org-typst-preview-mode' or
`org-typst-preview-auto-mode' alongside for preview commands."
  :init-value nil
  :lighter " Typst$"
  :group 'org-typst-preview)

;;;###autoload
(defun org-typst-preview-refresh-buffer ()
  "Clear and rebuild all Typst previews in the current buffer."
  (interactive)
  (org-typst-preview-clear)
  (org-typst-preview--preview-region (point-min) (point-max)))

;;;###autoload
(defun org-typst-preview-show-errors ()
  "Display the latest Typst compiler error."
  (interactive)
  (pop-to-buffer (get-buffer-create org-typst-preview-log-buffer)))

(provide 'org-typst-preview)

;;; org-typst-preview.el ends here
