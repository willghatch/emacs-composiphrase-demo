;;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq cp-demo-aggreact--editing-groups-ring (make-ring 100))

(setq cp-demo-aggreact--pre-command-prefix-arg-state nil)
(setq cp-demo--aggreact-repeat-active-p nil)

(defun cp-demo-aggreact--pre-command ()
  (setq cp-demo-aggreact--pre-command-prefix-arg-state prefix-arg))
(add-hook 'pre-command-hook 'cp-demo-aggreact--pre-command)

(setq cp-demo-aggreact--current-command-modified-buffers nil)

(defun cp-demo-aggreact--after-change (&rest args)
  (when (not (member (current-buffer) cp-demo-aggreact--current-command-modified-buffers))
    (setq cp-demo-aggreact--current-command-modified-buffers
          (cons (current-buffer) cp-demo-aggreact--current-command-modified-buffers))))
(add-hook 'after-change-functions 'cp-demo-aggreact--after-change)

(setq aggreact-command-group-split-predicate
      (lambda (reversed-command-group)
        (and (equal estate-state 'normal)
             (equal composiphrase-current-sentence nil)
             (equal cp-demo-aggreact--pre-command-prefix-arg-state prefix-arg)
             )))

(setq aggreact-command-history-enrichment-functions
      (cons
       (lambda (x)
         (let ((result
                `(
                  ;;(estate-state-after-command . ,estate-state)
                  ;;(composiphrase-current-sentence-after-command . ,composiphrase-current-sentence)
                  (modified-prefix-arg-p . ,(equal cp-demo-aggreact--pre-command-prefix-arg-state
                                                   prefix-arg))
                  (edited-buffer-that-command-ended-in
                   .
                   ,(member (current-buffer)
                            cp-demo-aggreact--current-command-modified-buffers)))))
           (setq cp-demo-aggreact--pre-command-prefix-arg-state nil)
           (setq cp-demo-aggreact--current-command-modified-buffers nil)
           result))
       aggreact-command-history-enrichment-functions))

(setq aggreact-command-group-split-functions
      (cons
       (lambda (group)
         (let* ((edited-p (seq-find (lambda (cmd) (cdr (assq 'edited-buffer-that-command-ended-in cmd))) group))
                (length-1-command (and (equal (length group) 1)
                                       (cdr (assq 'command (car group)))))
                (single-undo-p (and length-1-command
                                    (member length-1-command
                                            '(undo undo-tree-undo undo-tree-redo)))))
           (when (and edited-p
                      (not single-undo-p)
                      (not cp-demo--aggreact-repeat-active-p)
                      )
             (ring-insert cp-demo-aggreact--editing-groups-ring group))))
       aggreact-command-group-split-functions))


(defun cp-demo-aggreact-repeat-latest-editing (&optional count)
  (interactive "p")
  (setq cp-demo--aggreact-repeat-active-p t)
  (setq aggreact-inhibit-recording t)
  (let ((last-command (ring-ref cp-demo-aggreact--editing-groups-ring 0)))
    (unwind-protect
        (dotimes (i (or count 1))
          (aggreact-execute-command-group-as-keyboard-macro last-command))
      ;; Don't reset flags here -- they must remain set when post-command-hook
      ;; fires for THIS command, otherwise aggreact records the "." key itself
      ;; into the ring, causing infinite recursion on next repeat.
      ;; cp-demo-aggreact--post-repeat-cleanup handles the reset.
      )))

(defun cp-demo-aggreact--post-repeat-cleanup ()
  "Reset repeat flags after aggreact--post-command has finished.
This runs at depth 90 so it executes after aggreact--post-command (depth 0),
ensuring the flags are still set when the split function checks them."
  (when cp-demo--aggreact-repeat-active-p
    (setq cp-demo--aggreact-repeat-active-p nil)
    (setq aggreact-inhibit-recording nil)))
(add-hook 'post-command-hook 'cp-demo-aggreact--post-repeat-cleanup 90)


(provide 'composiphrase-estate-aggreact-config)
