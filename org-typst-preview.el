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

(defcustom org-typst-preview-font-size 11.0
  "Base preview font size in points."
  :group 'org-typst-preview
  :type 'number)

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

(defun org-typst-preview--fragment-at (position &optional limit)
  "Parse a Typst fragment beginning at POSITION, stopping before LIMIT."
  (save-excursion
    (goto-char position)
    (let* ((opener (buffer-substring-no-properties
                    position (min (+ position 2) (point-max))))
           (block-p (equal opener "#["))
           (closer (cond ((equal opener "#(") "#)")
                         (block-p "#]"))))
      (when (and closer
                 (not (eq (char-before) ?\\))
                 (not (org-element-lineage
                       (org-element-context)
                       '(code comment comment-block example-block export-block
                              fixed-width inline-src-block keyword node-property
                              property-drawer src-block verbatim)
                       t)))
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
                    :block block-p))))))))

(defun org-typst-preview--fragment-at-point ()
  "Return the Typst fragment containing point."
  (let* ((origin (point))
         (fragment (org-typst-preview--fragment-at origin)))
    (save-excursion
      (while (and (not fragment) (search-backward "#" nil t))
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
      (while (re-search-forward (rx "#" (or "(" "[")) end t)
        (when-let* ((fragment
                     (org-typst-preview--fragment-at
                      (match-beginning 0) end)))
          (push fragment fragments)
          (goto-char (plist-get fragment :end)))))
    (nreverse fragments)))

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
               (format "rgb(%s)"
                       (mapconcat
                        (lambda (component)
                          (number-to-string (round (/ component 257.0))))
                        (color-values background) ","))))
     (format "#set text(size: %.3fpt, fill: rgb(%s)%s)\n"
             (* org-typst-preview-font-size scale)
             (mapconcat
              (lambda (component)
                (number-to-string (round (/ component 257.0))))
              (color-values foreground) ",")
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
  "Create a preview overlay for FRAGMENT, displaying IMAGE when non-nil."
  (let ((overlay (make-overlay (plist-get fragment :begin)
                               (plist-get fragment :end))))
    (overlay-put overlay 'org-overlay-type 'org-latex-overlay)
    (overlay-put overlay 'org-typst-preview t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'modification-hooks
                 (list (lambda (changed _after _begin _end &optional _length)
                         (delete-overlay changed))))
    (when image
      (overlay-put overlay 'display
                   (list 'image :type 'svg :file image :ascent 'center)))
    overlay))

(defun org-typst-preview--finish (process event)
  "Finish the Typst job belonging to PROCESS, described by EVENT."
  (when (memq (process-status process) '(exit signal))
    (let* ((job (process-get process 'org-typst-preview-job))
           (success (and (eq (process-status process) 'exit)
                         (zerop (process-exit-status process))))
           (output (org-typst-preview--job-output job))
           (process-buffer (process-buffer process)))
      (condition-case error-data
          (when success
            (rename-file (org-typst-preview--job-render job) output t)
            (dolist (overlay (org-typst-preview--job-overlays job))
              (when (overlay-buffer overlay)
                (overlay-put overlay 'display
                             (list 'image :type 'svg :file output
                                   :ascent 'center)))))
        (error
         (setq success nil)
         (with-current-buffer process-buffer
           (goto-char (point-max))
           (insert "\n" (error-message-string error-data) "\n"))))
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
      (when success (kill-buffer process-buffer))
      (delete-file (org-typst-preview--job-input job))
      (when (file-exists-p (org-typst-preview--job-render job))
        (delete-file (org-typst-preview--job-render job)))
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
    (let ((process
           (make-process
            :name (format "org-typst-preview-%s"
                          (substring (sha1 output) 0 10))
            :buffer process-buffer
            :command (list org-typst-preview-executable
                           "compile" "--format" "svg" input render)
            :coding 'utf-8-unix
            :noquery t
            :sentinel #'org-typst-preview--finish)))
      (process-put process 'org-typst-preview-job job)
      (cl-incf org-typst-preview--running))))

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
                      (face-at-point begin)))
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
(defun org-typst-preview (&optional argument)
  "Toggle Typst and LaTeX previews with Org's prefix ARGUMENT semantics.

At point, toggle the Typst or LaTeX formula there.  With an active region,
preview both kinds of formula in the region; otherwise preview the section.
One universal prefix clears the region or current section, two preview the
whole buffer, and three clear the whole buffer.  LaTeX previews are handled
by `org-latex-preview'."
  (interactive "P")
  (cond
   ((not (display-graphic-p)) nil)
   ((and untrusted-content (not org--latex-preview-when-risky)) nil)
   ((member argument '((4) (64)))
    (org-latex-preview argument))
   ((equal argument '(16))
    (org-latex-preview argument)
    (message "%d Typst previews requested"
             (org-typst-preview--preview-region (point-min) (point-max))))
   ((use-region-p)
    (org-latex-preview argument)
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
    (org-latex-preview argument)
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
        (goto-char begin)
        (delete-region begin end)
        (if (plist-get fragment :block)
            (insert "\n#+begin_export typst\n" value
                    "\n#+end_export\n")
          (insert "@@typst:" value "@@"))))))

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
      (add-hook 'org-export-before-parsing-functions
                #'org-typst-preview--export-before-parsing nil t)
    (remove-hook 'org-export-before-parsing-functions
                 #'org-typst-preview--export-before-parsing t)))

(defun org-typst-preview--auto-run (buffer marker)
  "Preview the fragment near MARKER in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq org-typst-preview--auto-timer nil)
      (when (bound-and-true-p org-typst-preview-auto-mode)
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
