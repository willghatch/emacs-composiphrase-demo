;; -*- lexical-binding: t -*-

;; Unstable work-in-progress visual modifier

;; The idea is to generalize visual-line-mode that vim and evil-mode have, so that you can effectively make visual-X-mode for any text object X.
;; The basic step is to have a region extender that indicates the region beyond point and mark based on the text object, and sets that expanded region when calling non-movement commands.
;; But visual-line-mode also copies lines in such a way that the paste command pastes on line boundaries.  So a full implementation of this idea should also include a modifier copy command that saves the details about which text object it is, and how to find the right place to paste.  Then the paste command needs to honor that, and potentially include a forward and backward paste variant.  For simplicity, I might just do the first part, but maybe I'll do something about this copy/paste issue, too.
;; However, I think the thing that is most attractive to me, that I am really working on this for, is to try it as a way to select tree siblings.

(require 'estate-default-states)

;; Visual state region modifiers.

(defface estate-visual-modifier-expanded-region-face
  '((t :inherit region))
  "Face for modified visual region overlay.")

(defvar-local estate--visual-modifier nil
  "Current visual region modifier. Either a function or (name-string function).")

(defvar-local estate--visual-modifier-overlays nil
  "List of overlays showing the modified region boundaries.")

(defvar-local estate--visual-region-expanded nil
  "Non-nil when the region has been expanded for a command.")

(defvar-local estate--visual-original-mark nil
  "Original mark position before modification expansion.")

(defvar-local estate--visual-original-point nil
  "Original point position before modification expansion.")

(defvar-local estate--visual-cached-modified-region nil
  "Cached result of the modifier function to avoid recomputation.")

(defvar-local estate--visual-cached-region-input nil
  "The (beg . end) input used to compute the cached modified region.")

(defun estate-visual-state-activate-modifier (modifier)
  "Activate a visual state region modifier.
MODIFIER should be either a function like (mod-func region-beginning region-end) -> (list modified-region-beginning modified-region-end),
or MODIFIER should be a list like (list NAME-STRING MODIFIER-FUNCTION)."
  (when (not (eq estate-state 'visual))
    (error "Visual region modifier can only be activated in visual state"))

  (setq-local estate--visual-modifier modifier)

  ;; Install hooks for region modification
  (add-hook 'pre-command-hook 'estate--visual-pre-command nil t)
  (add-hook 'post-command-hook 'estate--visual-post-command nil t)

  ;; Update the overlay to show modified region
  (estate--visual-update-modifier-overlay))

(defun estate--visual-clear-cache ()
  "Clear the cached modified region computation."
  (setq-local estate--visual-cached-modified-region nil)
  (setq-local estate--visual-cached-region-input nil))

(defun estate-visual-state-deactivate-modifier ()
  "Deactivate the current visual state region modifier."
  (when estate--visual-modifier
    (setq-local estate--visual-modifier nil)

    ;; Remove hooks
    (remove-hook 'pre-command-hook 'estate--visual-pre-command t)
    (remove-hook 'post-command-hook 'estate--visual-post-command t)

    ;; Clean up overlays
    (dolist (overlay estate--visual-modifier-overlays)
      (delete-overlay overlay))
    (setq-local estate--visual-modifier-overlays nil)

    ;; Reset expansion state
    (setq-local estate--visual-region-expanded nil)

    ;; Clear cache
    (setq-local estate--visual-cached-modified-region nil)
    (setq-local estate--visual-cached-region-input nil)))

(defun estate--visual-get-modifier-function ()
  "Get the modifier function from the current modifier."
  (cond
   ((functionp estate--visual-modifier) estate--visual-modifier)
   ((and (listp estate--visual-modifier) (functionp (cadr estate--visual-modifier)))
    (cadr estate--visual-modifier))
   (t nil)))

(defun estate--visual-get-modified-region ()
  "Get the modified region boundaries using the current modifier.
Uses caching to avoid recomputing the modifier function multiple times."
  (when (and estate--visual-modifier (region-active-p))
    (let ((current-input (cons (region-beginning) (region-end))))
      ;; Check if we have a valid cached result for the current region
      (unless (equal current-input estate--visual-cached-region-input)
        ;; Cache is invalid, recompute
        (when-let ((mod-func (estate--visual-get-modifier-function)))
          (setq-local estate--visual-cached-modified-region
                      (unwind-protect
                          (progn
                            (estate--deactivate-initialize-core-states)
                            (funcall mod-func (car current-input) (cdr current-input)))
                        (estate--activate-initialize-core-states)))
          (setq-local estate--visual-cached-region-input current-input)))
      estate--visual-cached-modified-region)))

(defun estate--visual-update-modifier-overlay ()
  "Update the overlay showing modified region boundaries."
  (when estate--visual-modifier
    (when-let ((modified-region (estate--visual-get-modified-region)))
      (let ((mod-beg (car modified-region))
            (mod-end (cadr modified-region))
            (orig-beg (region-beginning))
            (orig-end (region-end)))
        ;; Clean up existing overlays
        (dolist (overlay estate--visual-modifier-overlays)
          (delete-overlay overlay))
        (setq-local estate--visual-modifier-overlays nil)
        ;; Create overlays for extended areas only
        (when (or (< mod-beg orig-beg) (> mod-end orig-end))
          (let ((overlay-ranges '()))
            (when (< mod-beg orig-beg)
              (push (cons mod-beg orig-beg) overlay-ranges))
            (when (> mod-end orig-end)
              (push (cons orig-end mod-end) overlay-ranges))
            (dolist (range overlay-ranges)
              (let ((overlay (make-overlay (car range) (cdr range))))
                (overlay-put overlay 'face 'estate-visual-modifier-expanded-region-face)
                (overlay-put overlay 'priority 100)
                (push overlay estate--visual-modifier-overlays)))))))))

(defvar estate-visual-modifier-expansion-command-predicate nil
  "Function to determine if a command should expand the region.
Should be a function that takes a command symbol as input and returns non-nil
if the command should operate on the expanded/modified region,
or nil if it should operate on the original region (i.e., for movement commands).
If nil, all commands are treated as expansion commands.")

(defun estate--visual-do-region-expansion ()
  "Perform the actual region expansion if conditions are met.
Returns non-nil if expansion was performed."
  (when (and estate--visual-modifier
             (eq estate-state 'visual)
             (region-active-p)
             (not estate--visual-region-expanded))

    ;; Save original region
    (setq-local estate--visual-original-mark (mark))
    (setq-local estate--visual-original-point (point))

    ;; Expand to modified region
    (when-let ((modified-region (estate--visual-get-modified-region)))
      (let ((beg (car modified-region))
            (end (cadr modified-region)))
        (goto-char end)
        (set-mark beg)
        (setq-local estate--visual-region-expanded t)
        t))))

(defun estate-visual-modifier-expand-region ()
  "Manually trigger region expansion during command execution.
This can be called by commands that need the expanded region but couldn't
be determined as non-movement commands during pre-command hook.
The post-command hook will still clean up properly."
  (estate--visual-do-region-expansion))

(defun estate--visual-pre-command ()
  "Hook run before each command in visual state with active modifier."
  (when (and estate-visual-modifier-expansion-command-predicate
             (funcall estate-visual-modifier-expansion-command-predicate this-command))
    (estate--visual-do-region-expansion)))

(defun estate--visual-post-command ()
  "Hook run after each command in visual state with active modifier."
  (when (and estate--visual-modifier (eq estate-state 'visual))
    (cond
     ;; Contract region back after non-movement command
     ((and estate--visual-region-expanded
           estate--visual-original-mark
           estate--visual-original-point)
      (goto-char estate--visual-original-point)
      (set-mark estate--visual-original-mark)
      (setq-local estate--visual-region-expanded nil)
      (setq-local estate--visual-original-mark nil)
      (setq-local estate--visual-original-point nil)
      (estate--visual-clear-cache)
      (estate--visual-update-modifier-overlay))

     ;; Update overlay for movement commands
     ((region-active-p)
      (estate--visual-update-modifier-overlay)))))

;; Hook into visual state exit to clean up modifier
(add-hook 'estate-visual-state-exit-hook 'estate-visual-state-deactivate-modifier)

(provide 'estate-visual-modifier-state)
