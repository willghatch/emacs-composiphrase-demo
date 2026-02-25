;;; -*- lexical-binding: t; -*-
;;; estate-insert-one-command.el --- Execute one normal-state command from insert state, composiphrase-aware

;; Similar to Vim's C-o in insert mode: temporarily switch to normal state
;; to execute a single command, then return to insert state.
;;
;; Composiphrase-aware: if a composiphrase sentence is in progress (e.g.,
;; the user has typed a verb but not yet an object), stay in normal state
;; until the sentence is fully executed and cleared.

(require 'estate)
(require 'composiphrase)

(defvar-local estate-insert-one-command--active nil
  "Non-nil when insert-one-command state is active, waiting to return to insert state.
Set to `pending' during the initiating command, promoted to t on the
next post-command cycle, so the check does not fire prematurely on
the C-o command itself.")

(defun estate-insert-one-command--post-command-check ()
  "Post-command hook to check if the one-command execution is complete.
Returns to insert state when `composiphrase-current-sentence' is empty
and `prefix-arg' is nil, indicating the command (including any multi-key
composiphrase sentence or numeric prefix) has finished executing."
  (when estate-insert-one-command--active
    (if (eq estate-insert-one-command--active 'pending)
        ;; First post-command cycle (the C-o command itself) -- skip,
        ;; but promote to active so next command will be checked.
        (setq estate-insert-one-command--active t)
      (when (and (null composiphrase-current-sentence)
                 (null prefix-arg))
        (estate-insert-one-command--teardown)
        (estate-insert-state)))))

(defun estate-insert-one-command--teardown ()
  "Remove the post-command hook and clear the active flag."
  (setq estate-insert-one-command--active nil)
  (remove-hook 'post-command-hook #'estate-insert-one-command--post-command-check t))

(defun estate-insert-one-command--abort ()
  "Abort insert-one-command if the user manually leaves normal state.
This prevents the hook from lingering if the user switches states manually."
  (when (and estate-insert-one-command--active
             (not (eq estate-state 'normal)))
    (estate-insert-one-command--teardown)))

(add-hook 'estate-state-change-hook #'estate-insert-one-command--abort)

(defun estate-insert-state-one-normal-command ()
  "From insert state, switch to normal state for one composiphrase command.
After the command completes (composiphrase sentence is empty), return
to insert state automatically.  If a composiphrase sentence is being
built (multi-key sequence), stay in normal state until the sentence
is fully resolved."
  (interactive)
  (when (eq estate-state 'insert)
    (setq estate-insert-one-command--active 'pending)
    (add-hook 'post-command-hook #'estate-insert-one-command--post-command-check nil t)
    (estate-normal-state)))

(provide 'estate-insert-one-command)
