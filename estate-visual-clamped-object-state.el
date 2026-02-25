;; -*- lexical-binding: t -*-

;; Visual clamped object mode.
;;
;; A mode where the user moves point normally, with an overlay showing
;; the text object at point's current location.  Mark follows point
;; (so the real region is zero-width), and when a non-movement action
;; is triggered, the region expands to the text object boundaries.
;;
;; The key difference from regular visual-modifier: in regular, mark is
;; fixed and only one end expands.  In clamped, mark follows point,
;; and expansion is always the text object AT point.

(require 'estate-default-states)
(require 'estate-visual-modifier-state)

;; Variables

(defvar-local estate--visual-clamped-object-func nil
  "Function to compute text object bounds at a position.
Should be a function of zero arguments, called with point at the
relevant position, returning (BEG . END) or nil.")

(defvar-local estate--visual-clamped-object-name nil
  "Name of the current clamped object type, for display purposes.")

(defvar-local estate--visual-clamped-object-active nil
  "Non-nil when visual clamped object mode is active.")

(defvar-local estate--visual-clamped-pre-command-point nil
  "Point position before the current command, for detecting movement.")

(defvar-local estate--visual-clamped-command-modified-buffer nil
  "Non-nil if the current command has modified the buffer.")

(defvar-local estate--visual-clamped-object-overlays nil
  "List of overlays showing the clamped text object region.")

(defvar-local estate--visual-clamped-region-expanded nil
  "Non-nil when the region has been expanded for an action command.")

(defvar-local estate--visual-clamped-original-point nil
  "Original point position before expansion.")

(defvar-local estate--visual-clamped-original-mark nil
  "Original mark position before expansion.")

(defvar estate-visual-clamped-object-command-predicate nil
  "Function to determine if a command should trigger region expansion.
Should be a function that takes a command symbol as input and returns non-nil
if the command is a non-movement command (e.g. delete, change).
Such commands operate on the expanded (text object) region.
Movement commands (returning nil) cause mark to follow point and
the overlay to update.
If nil, treat all commands as movements (never expand).")

;; Bounds computation

(defun estate--visual-clamped-object-bounds-at-point ()
  "Get the text object bounds at the current point using the active clamped function.
Temporarily deactivates estate core state hooks to prevent state
transitions caused by mark/region changes during bounds computation."
  (when estate--visual-clamped-object-func
    (unwind-protect
        (progn
          (estate--deactivate-initialize-core-states)
          (funcall estate--visual-clamped-object-func))
      (estate--activate-initialize-core-states))))

;; Overlay management

(defun estate--visual-clamped-object-update-overlay ()
  "Update the overlay showing the text object region at point."
  (when estate--visual-clamped-object-active
    ;; Clean up existing overlays
    (dolist (ov estate--visual-clamped-object-overlays)
      (delete-overlay ov))
    (setq-local estate--visual-clamped-object-overlays nil)
    ;; Create overlay for the text object at point
    (let ((bounds (estate--visual-clamped-object-bounds-at-point)))
      (when bounds
        (let ((ov (make-overlay (car bounds) (cdr bounds))))
          (overlay-put ov 'face 'estate-visual-modifier-expanded-region-face)
          (overlay-put ov 'priority 100)
          (push ov estate--visual-clamped-object-overlays))))))

;; Region expansion (for non-movement actions)

(defun estate--visual-clamped-object-do-expansion ()
  "Expand the region to the text object boundaries at point.
Returns non-nil if expansion was performed."
  (when (and estate--visual-clamped-object-active
             (eq estate-state 'visual)
             (not estate--visual-clamped-region-expanded))
    ;; Save original positions
    (setq-local estate--visual-clamped-original-point (point))
    (setq-local estate--visual-clamped-original-mark (mark))
    ;; Expand to text object bounds
    (let ((bounds (estate--visual-clamped-object-bounds-at-point)))
      (when bounds
        (unwind-protect
            (progn
              (estate--deactivate-initialize-core-states)
              (set-mark (car bounds))
              (goto-char (cdr bounds)))
          (estate--activate-initialize-core-states))
        (setq deactivate-mark nil)
        (setq-local estate--visual-clamped-region-expanded t)
        t))))

(defun estate-visual-clamped-object-expand-region ()
  "Manually trigger region expansion during command execution.
This can be called by commands (e.g. via composiphrase advice) that need
the expanded region but could not be determined as non-movement commands
during the pre-command hook."
  (estate--visual-clamped-object-do-expansion))

;; Hook functions

(defun estate--visual-clamped-object-after-change (_beg _end _len)
  "Hook run after buffer changes to track modifications during commands."
  (setq-local estate--visual-clamped-command-modified-buffer t))

(defun estate--visual-clamped-object-pre-command ()
  "Pre-command hook for visual clamped object mode.
Records point position, resets modification flag, and potentially
expands the region for non-movement commands."
  (setq estate--visual-clamped-pre-command-point (point))
  (setq-local estate--visual-clamped-command-modified-buffer nil)
  ;; If the command predicate says this is a non-movement command,
  ;; expand the region before the command runs.
  (when (and estate-visual-clamped-object-command-predicate
             (funcall estate-visual-clamped-object-command-predicate this-command))
    (estate--visual-clamped-object-do-expansion)))

(defun estate--visual-clamped-object-post-command ()
  "Post-command hook for visual clamped object mode.
After movement commands: set mark = point, update overlay.
After non-movement actions: contract region back, or deactivate if buffer modified."
  (when (and estate--visual-clamped-object-active
             (eq estate-state 'visual))
    (cond
     ;; If the buffer was modified while expanded, deactivate.
     ;; The editing command operated on the expanded region and we
     ;; should not try to re-clamp on potentially invalid positions.
     (estate--visual-clamped-command-modified-buffer
      (estate--visual-clamped-object-deactivate))
     ;; If the region was expanded for a non-movement command that
     ;; did NOT modify the buffer, contract back to original positions.
     ((and estate--visual-clamped-region-expanded
           estate--visual-clamped-original-point
           estate--visual-clamped-original-mark)
      (unwind-protect
          (progn
            (estate--deactivate-initialize-core-states)
            (goto-char estate--visual-clamped-original-point)
            (set-mark estate--visual-clamped-original-point))
        (estate--activate-initialize-core-states))
      (setq deactivate-mark nil)
      (setq-local estate--visual-clamped-region-expanded nil)
      (setq-local estate--visual-clamped-original-point nil)
      (setq-local estate--visual-clamped-original-mark nil)
      (estate--visual-clamped-object-update-overlay))
     ;; Normal case (movement): set mark = point, update overlay.
     (t
      (unwind-protect
          (progn
            (estate--deactivate-initialize-core-states)
            (set-mark (point)))
        (estate--activate-initialize-core-states))
      (setq deactivate-mark nil)
      (estate--visual-clamped-object-update-overlay)))))

;; Activation / deactivation

(defun estate-visual-clamped-object-activate (name bounds-func)
  "Activate visual clamped object mode.
NAME is a string or symbol for display.
BOUNDS-FUNC is a function of zero arguments that returns (BEG . END)
for the text object at point, or nil if none."
  (when (not (eq estate-state 'visual))
    (estate-visual-state))
  (setq-local estate--visual-clamped-object-name name)
  (setq-local estate--visual-clamped-object-func bounds-func)
  (setq-local estate--visual-clamped-object-active t)
  (setq-local estate--visual-clamped-command-modified-buffer nil)
  (setq-local estate--visual-clamped-region-expanded nil)
  (setq-local estate--visual-clamped-original-point nil)
  (setq-local estate--visual-clamped-original-mark nil)
  ;; Set mark = point (zero-width region, user moves point normally)
  (unwind-protect
      (progn
        (estate--deactivate-initialize-core-states)
        (set-mark (point)))
    (estate--activate-initialize-core-states))
  (setq deactivate-mark nil)
  ;; Install hooks
  (add-hook 'pre-command-hook #'estate--visual-clamped-object-pre-command nil t)
  (add-hook 'post-command-hook #'estate--visual-clamped-object-post-command nil t)
  (add-hook 'after-change-functions #'estate--visual-clamped-object-after-change nil t)
  ;; Show initial overlay for the text object at point
  (estate--visual-clamped-object-update-overlay))

(defun estate--visual-clamped-object-deactivate ()
  "Deactivate visual clamped object mode."
  (when estate--visual-clamped-object-active
    (setq-local estate--visual-clamped-object-active nil)
    (setq-local estate--visual-clamped-object-func nil)
    (setq-local estate--visual-clamped-object-name nil)
    (setq-local estate--visual-clamped-pre-command-point nil)
    (setq-local estate--visual-clamped-command-modified-buffer nil)
    (setq-local estate--visual-clamped-region-expanded nil)
    (setq-local estate--visual-clamped-original-point nil)
    (setq-local estate--visual-clamped-original-mark nil)
    ;; Clean up overlays
    (dolist (ov estate--visual-clamped-object-overlays)
      (delete-overlay ov))
    (setq-local estate--visual-clamped-object-overlays nil)
    ;; Remove hooks
    (remove-hook 'pre-command-hook #'estate--visual-clamped-object-pre-command t)
    (remove-hook 'post-command-hook #'estate--visual-clamped-object-post-command t)
    (remove-hook 'after-change-functions #'estate--visual-clamped-object-after-change t)))

;; Clean up on visual state exit.
(add-hook 'estate-visual-state-exit-hook #'estate--visual-clamped-object-deactivate)

(provide 'estate-visual-clamped-object-state)
