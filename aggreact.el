;;; -*- lexical-binding: t; -*-
;;; aggreact.el --- Record commands with rich history, and group them.

;;; Author: William Hatch <william@hatch.uno>
;;; Maintainer: William Hatch <william@hatch.uno>
;;; Version: 0.0
;;; Homepage: https://github.com/willghatch/emacs-aggreact
;;; Git-Repository: git://github.com/willghatch/emacs-aggreact.git
;;; Keywords: keyboard-macro repeat
;;; Package-Requires: ((emacs "28") (this-command-all-keys "0.0"))

;;; License:
;;; This is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; <http://www.gnu.org/licenses/>

;;; Commentary:
;;; See docstring for aggreact-mode.

;;; Code:

;; TODO - I have no idea what the minimum emacs version required here is.  It can probably work with much earlier versions.

(require 'this-command-all-keys)

(setq aggreact--current-groups nil)

(defvar aggreact--recording-baseline-depth 0
  "The recursion depth at which normal recording occurs.
Commands at deeper recursion depths are inside a recursive edit, and their
keys are accumulated rather than recorded as separate commands.")

(setq aggreact--recursive-edit-accumulated-tck-keys nil)
(setq aggreact--recursive-edit-accumulated-single-keys nil)
(setq aggreact--recursive-edit-accumulated-raw-keys nil)

;; Saved keys from pre-command at baseline depth, used because
;; recursive-edit clobbers this-command-keys with the last inner command's keys.
(setq aggreact--pre-command-saved-tck nil)
(setq aggreact--pre-command-saved-single nil)
(setq aggreact--pre-command-saved-raw nil)
(setq aggreact--pre-command-saved-command nil)

(defvar aggreact-command-group-split-predicate nil
  "Predicate for when to split a command group.
Splits when the predicate returns true.
If the predicate itself is nil instead of a function, always split.
Predicate takes a single argument, which is a (reversed) list of commands so far, as alists of details about the commands.
")

(defvar aggreact-command-history-enrichment-functions nil
  "List of functions for enriching command history.
Each function takes a single argument, the command data alist so far, and returns either nil to make no change, or an alist to extend the history with.
IE if you want to add one field, you can return an alist with the one field, which will be appended to the other alist in the enrichment loop.
")

(defvar aggreact-command-group-split-functions nil
  "List of functions to run once a command group is split (according to 'aggreact-command-group-split-predicate').
Each function receives a single argument: the new command group.
The command group is a list of alists, where each alist contains details of a command in the group.
This is useful to eg. keep histories of commands with interesting properties.
")

(defvar aggreact-inhibit-recording nil
  "When non-nil, inhibit recording of commands and setting explicit repeat commands.
This should be set during replay to prevent re-entrant recording.")

(defvar aggreact-explicit-repeat-command nil
  "Command to use for explicit repeat splitting.
When non-nil, this forces aggreact to ignore the predicate and do the split.
The command group will be annotated with an explicit-repeat-command field
containing this command.  This variable is typically nil, but can be set by
commands to force splitting.

When using this to explicitly split a group, the actual group is ignored and
this command is used for repetition instead.
")

(defun aggreact--pre-command ()
  "Capture keys before each command runs.
At baseline depth, save the command's keys so they survive recursive-edit
\(which clobbers `this-command-keys' with the last inner command's values).
At deeper depths, accumulate keys into the recursive-edit accumulators.
Keys are captured in pre-command because `exit-recursive-edit' throws past
post-command-hook, which would cause the exit key to be lost."
  (when (not aggreact-inhibit-recording)
    (if (> (recursion-depth) aggreact--recording-baseline-depth)
        ;; Inside recursive edit: accumulate keys.
        (with-demoted-errors "aggreact pre-command recursive key capture error: %s"
          (push (this-command-keys) aggreact--recursive-edit-accumulated-tck-keys)
          (push (this-single-command-keys) aggreact--recursive-edit-accumulated-single-keys)
          (push (this-single-command-raw-keys) aggreact--recursive-edit-accumulated-raw-keys))
      ;; At baseline depth: save keys for this command in case it enters
      ;; recursive-edit (which clobbers this-command-keys).
      (setq aggreact--pre-command-saved-tck (this-command-keys))
      (setq aggreact--pre-command-saved-single (this-single-command-keys))
      (setq aggreact--pre-command-saved-raw (this-single-command-raw-keys))
      (setq aggreact--pre-command-saved-command this-command))))

(defun aggreact--post-command ()
  (unless aggreact-inhibit-recording
    (if (> (recursion-depth) aggreact--recording-baseline-depth)
        ;; Inside recursive edit: keys already captured in pre-command-hook.
        ;; Don't record as a separate command.
        nil
      ;; At baseline depth: record command, merging any keys accumulated
      ;; from a recursive edit that this command entered and returned from.
      (let* ((recursive-tck (nreverse aggreact--recursive-edit-accumulated-tck-keys))
             (recursive-single (nreverse aggreact--recursive-edit-accumulated-single-keys))
             (recursive-raw (nreverse aggreact--recursive-edit-accumulated-raw-keys))
             ;; If there were recursive-edit keys, this-command-keys is
             ;; clobbered (it reflects the last inner command, not this one).
             ;; Use the keys saved in pre-command instead.
             (own-tck (if recursive-tck
                          (list aggreact--pre-command-saved-tck)
                        (this-command-all-keys 'tck t)))
             (own-single (if recursive-tck
                             (list aggreact--pre-command-saved-single)
                           (this-command-all-keys 'single t)))
             (own-raw (if recursive-tck
                          (list aggreact--pre-command-saved-raw)
                        (this-command-all-keys 'raw t)))
             (recorded-command (if recursive-tck
                                   aggreact--pre-command-saved-command
                                 this-command))
             (new-command-details
              `((command . ,recorded-command)
                ;; TODO - command arguments
                (keys-vectors . ,(append own-tck recursive-tck))
                (single-keys-vectors . ,(append own-single recursive-single))
                (raw-keys-vectors . ,(append own-raw recursive-raw)))))
        (setq aggreact--recursive-edit-accumulated-tck-keys nil)
        (setq aggreact--recursive-edit-accumulated-single-keys nil)
        (setq aggreact--recursive-edit-accumulated-raw-keys nil)
        (setq new-command-details
              (seq-reduce (lambda (accum enrich-func)
                            (let ((enrich-result
                                   (with-demoted-errors
                                       "error during command history enrichment: %s"
                                     (funcall enrich-func accum))))
                              (if enrich-result
                                  (append enrich-result accum)
                                accum)))
                          aggreact-command-history-enrichment-functions
                          new-command-details))
        (setq aggreact--current-groups (cons new-command-details aggreact--current-groups))
        (let ((should-split
               (or aggreact-explicit-repeat-command
                   (if aggreact-command-group-split-predicate
                       (funcall aggreact-command-group-split-predicate aggreact--current-groups)
                     t)))
              (explicit-repeat-cmd aggreact-explicit-repeat-command))
          (when should-split
            (let ((finalized-group (reverse aggreact--current-groups)))
              (when explicit-repeat-cmd
                (setcar finalized-group
                        (cons `(explicit-repeat-command . ,explicit-repeat-cmd)
                              (car finalized-group)))
                (setq aggreact-explicit-repeat-command nil))
              (setq aggreact--current-groups nil)
              (with-demoted-errors
                  "error during aggreact-command-group-split-functions: %s"
                (mapcar (lambda (func) (funcall func finalized-group))
                        aggreact-command-group-split-functions)))))))))

(defun aggreact--get-keys-for-command-group (command-group)
  "Return the raw keys used for the entire COMMAND-GROUP as a flat vector."
  (apply 'seq-concatenate 'vector
         (mapcar (lambda (x) (apply 'seq-concatenate 'vector
                                    (cdr (assq 'keys-vectors x))))
                 command-group)))

(defun aggreact-execute-command-group-as-keyboard-macro (command-group)
  "Use the recorded keys of COMMAND-GROUP to execute as keyboard macro.
This may cause issues if not run in a state where the keys will do the same thing...
If the command group has an explicit-repeat-command field, execute that command instead.
"
  (let ((explicit-repeat-cmd (cdr (assq 'explicit-repeat-command (car command-group)))))
    (if explicit-repeat-cmd
        (funcall explicit-repeat-cmd)
      (execute-kbd-macro (aggreact--get-keys-for-command-group command-group)))))

(defun aggreact-make-explicit-command (command-func)
  "Takes a command func (symbol or closure) and returns a closure that when called:
* explicitly sets the aggreact last command to be a fresh closure that calls the given command with its current arguments
* and calls the command."
  (lambda (&rest args)
    (unless aggreact-inhibit-recording
      (setq aggreact-explicit-repeat-command
            (lambda () (apply command-func args))))
    (apply command-func args)))

(setq aggreact--this-command-all-keys-mode-state-before-aggreact nil)

(define-minor-mode aggreact-mode
  "A minor-mode for recording commands.
For each command, it records an alist that includes the the keys:
* command - the command executed
* keys-vectors - list of vectors of keys, as from `this-single-command-keys'
* raw-keys-vectors - list of vectors of keys, as from `this-single-command-raw-keys'

Additionally, it uses 'aggreact-command-history-enrichment-functions' to add more keys to the alist.

Commands are also grouped.
The variable 'aggreact-command-group-split-predicate' determines when command groups are split.
When a group is split, the 'aggreact-command-gorup-split-functions' list is run.
You can add a function to the list to eg. keep a list of interesting command groups.

This mode also requires this-command-all-keys mode.

The original motivation behind this mode was to provide command recording and replay that can work for my other packages, estate-mode and composiphrase.
That requires replaying groups of commands, and filtering to decide which groups are interesting for replaying.
See the composiphrase demo config at TODO to see an example setup using aggreact-mode.
"
  :global t
  (if aggreact-mode
      (progn
        (setq aggreact--this-command-all-keys-mode-state-before-aggreact
              this-command-all-keys-mode)
        (setq aggreact--recording-baseline-depth (recursion-depth))
        (this-command-all-keys-mode 1)
        (add-hook 'pre-command-hook 'aggreact--pre-command)
        (add-hook 'post-command-hook 'aggreact--post-command))
    (progn
      (remove-hook 'pre-command-hook 'aggreact--pre-command)
      (remove-hook 'post-command-hook 'aggreact--post-command)
      (when (not aggreact--this-command-all-keys-mode-state-before-aggreact)
        (this-command-all-keys-mode -1)))))

(provide 'aggreact)
