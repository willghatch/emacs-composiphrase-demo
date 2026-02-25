;;; -*- lexical-binding: t; -*-
;;; cpo-location-history.el --- Global buffer location history ring for composiphrase

;;; Commentary:
;;; A global location-history ring that records (buffer . position) pairs after
;;; each command when the position meaningfully changes.  Navigation keys move
;;; forward and backward through the history.  New entries are suppressed while
;;; navigating history, so traversal does not pollute the ring.
;;;
;;; Three navigation modes, selectable via composiphrase modifiers:
;;;   global      -- can change buffers (default)
;;;   local       -- filter ring to entries in the current buffer
;;;   non-local   -- filter to entries that cross buffer boundaries
;;;
;;; No promise of stability on any of the interface for now.

(require 'ring)

(defvar cpo-location-history-ring (make-ring 1000)
  "Ring of (buffer . position) cons cells representing visited locations.")

(defvar cpo-location-history--index 0
  "Current index into the history ring.
0 means the most recent entry; increases toward older entries.")

(defvar cpo-location-history--navigating nil
  "Non-nil while executing a history navigation command.
Suppresses recording new entries during navigation.")

(defvar cpo-location-history--last-buffer nil
  "Buffer at the last recorded location.")

(defvar cpo-location-history--last-position nil
  "Position at the last recorded location.")

(defvar cpo-location-history-min-distance 3
  "Minimum character distance within the same buffer to count as a meaningful change.")

(defun cpo-location-history-record ()
  "Record the current location in the history ring if it meaningfully changed.
Intended for use on `post-command-hook'.
Skips recording when `cpo-location-history--navigating' is non-nil."
  (when (and (not cpo-location-history--navigating)
             (not (minibufferp)))
    (let ((buf (current-buffer))
          (pos (point)))
      (when (or (not (eq buf cpo-location-history--last-buffer))
                (null cpo-location-history--last-position)
                (> (abs (- pos cpo-location-history--last-position))
                   cpo-location-history-min-distance))
        ;; Reset index whenever a real command records a new location.
        (setq cpo-location-history--index 0)
        (ring-insert cpo-location-history-ring (cons buf pos))
        (setq cpo-location-history--last-buffer buf)
        (setq cpo-location-history--last-position pos)))))

(defun cpo-location-history--filtered-indices (mode)
  "Return a list of ring indices matching MODE, ordered oldest-to-newest.
MODE is one of: \\='global, \\='local, \\='non-local.
For \\='local, only entries whose buffer is the current buffer are included.
For \\='non-local, only entries whose buffer differs from the current buffer.
For \\='global, all entries are included."
  (let ((len (ring-length cpo-location-history-ring))
        (cur-buf (current-buffer))
        (result nil))
    (dotimes (i len)
      (let* ((entry (ring-ref cpo-location-history-ring i))
             (entry-buf (car entry)))
        (cond
         ((eq mode 'global)
          (push i result))
         ((eq mode 'local)
          (when (eq entry-buf cur-buf)
            (push i result)))
         ((eq mode 'non-local)
          (when (not (eq entry-buf cur-buf))
            (push i result))))))
    ;; result was built newest-first (index 0 first), reverse for oldest-first
    (nreverse result)))

(defun cpo-location-history--navigate (direction mode)
  "Navigate the location history ring.
DIRECTION is \\='backward (older) or \\='forward (newer).
MODE is \\='global, \\='local, or \\='non-local."
  (let* ((len (ring-length cpo-location-history-ring))
         (mode (or mode 'global)))
    (cond
     ((= len 0)
      (message "Location history is empty."))
     (t
      (let* ((indices (cpo-location-history--filtered-indices mode))
             (n-filtered (length indices)))
        (cond
         ((= n-filtered 0)
          (message "No matching location history entries for mode: %s" mode))
         (t
          ;; Find where the current index sits among filtered indices.
          ;; We want to move relative to the current index within the filtered set.
          (let* ((cur-pos-in-filtered
                  ;; Find the position in the filtered list that is >= current index
                  (let ((pos 0)
                        (found nil))
                    (dolist (idx indices)
                      (when (and (not found) (>= idx cpo-location-history--index))
                        (setq found pos))
                      (setq pos (1+ pos)))
                    (or found (1- n-filtered))))
                 (new-pos-in-filtered
                  (if (eq direction 'backward)
                      (min (1+ cur-pos-in-filtered) (1- n-filtered))
                    (max (1- cur-pos-in-filtered) 0)))
                 (new-index (nth new-pos-in-filtered indices))
                 (entry (ring-ref cpo-location-history-ring new-index))
                 (target-buf (car entry))
                 (target-pos (cdr entry)))
            (if (buffer-live-p target-buf)
                (progn
                  (setq cpo-location-history--index new-index)
                  (unless (eq target-buf (current-buffer))
                    (switch-to-buffer target-buf))
                  (goto-char target-pos)
                  (message "Location history [%d/%d] %s:%d"
                           (1+ new-pos-in-filtered) n-filtered
                           (buffer-name target-buf) target-pos))
              ;; Buffer is dead; remove it and retry.
              (ring-remove cpo-location-history-ring new-index)
              ;; Adjust index if needed.
              (when (> cpo-location-history--index new-index)
                (setq cpo-location-history--index
                      (max 0 (1- cpo-location-history--index))))
              (cpo-location-history--navigate direction mode))))))))))

(defun cpo-location-history-back (&optional mode)
  "Go backward (older) in location history.
MODE is \\='global (default), \\='local, or \\='non-local."
  (interactive)
  (let ((cpo-location-history--navigating t))
    (cpo-location-history--navigate 'backward (or mode 'global))))

(defun cpo-location-history-forward (&optional mode)
  "Go forward (newer) in location history.
MODE is \\='global (default), \\='local, or \\='non-local."
  (interactive)
  (let ((cpo-location-history--navigating t))
    (cpo-location-history--navigate 'forward (or mode 'global))))

;;;###autoload
(define-minor-mode cpo-location-history-mode
  "Global minor mode that records buffer locations to a history ring."
  :global t
  :lighter " LocHist"
  (if cpo-location-history-mode
      (add-hook 'post-command-hook #'cpo-location-history-record)
    (remove-hook 'post-command-hook #'cpo-location-history-record)))

;;; Named interactive functions for each mode, suitable for repeatable-motion.

(defun cpo-location-history-global-back ()
  "Go backward (older) in global location history."
  (interactive)
  (cpo-location-history-back 'global))

(defun cpo-location-history-global-forward ()
  "Go forward (newer) in global location history."
  (interactive)
  (cpo-location-history-forward 'global))

(defun cpo-location-history-local-back ()
  "Go backward (older) in local (current buffer) location history."
  (interactive)
  (cpo-location-history-back 'local))

(defun cpo-location-history-local-forward ()
  "Go forward (newer) in local (current buffer) location history."
  (interactive)
  (cpo-location-history-forward 'local))

(defun cpo-location-history-non-local-back ()
  "Go backward (older) in non-local (cross-buffer) location history."
  (interactive)
  (cpo-location-history-back 'non-local))

(defun cpo-location-history-non-local-forward ()
  "Go forward (newer) in non-local (cross-buffer) location history."
  (interactive)
  (cpo-location-history-forward 'non-local))

(with-eval-after-load 'repeatable-motion
  (repeatable-motion-define-pair 'cpo-location-history-global-forward
                                 'cpo-location-history-global-back)
  (repeatable-motion-define-pair 'cpo-location-history-local-forward
                                 'cpo-location-history-local-back)
  (repeatable-motion-define-pair 'cpo-location-history-non-local-forward
                                 'cpo-location-history-non-local-back))

(provide 'cpo-location-history)
