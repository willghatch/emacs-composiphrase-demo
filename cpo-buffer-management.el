;;; -*- lexical-binding: t; -*-

;; Buffer management helpers for composiphrase demo.
;; Provides functions for cycling through buffers with various filters
;; (skip star-prefixed, file-backed only, modified only, etc.)
;; and wraps them with repeatable-motion for composiphrase integration.

(require 'repeatable-motion)

;;; Buffer predicates

(defun cpo-buffer--star-p (buf)
  "Return non-nil if BUF's name starts with *."
  (string-prefix-p "*" (buffer-name buf)))

(defun cpo-buffer--file-p (buf)
  "Return non-nil if BUF is visiting a file."
  (buffer-file-name buf))

(defun cpo-buffer--modified-p (buf)
  "Return non-nil if BUF is modified."
  (buffer-modified-p buf))

;;; Generic buffer cycling

(defun cpo--next-buffer-matching (predicate &optional n)
  "Switch to the next buffer matching PREDICATE.
N is the number of times to cycle (default 1).
Cycles forward through the buffer list."
  (let ((n (or n 1))
        (orig-buf (current-buffer))
        (bufs (buffer-list)))
    (dotimes (_ n)
      (let ((candidates (cdr (memq (current-buffer) bufs))))
        ;; Wrap around: append the buffers before current
        (setq candidates (append candidates
                                  (seq-take-while (lambda (b) (not (eq b (current-buffer)))) bufs)))
        (let ((found (seq-find predicate candidates)))
          (when found
            (switch-to-buffer found t)))))))

(defun cpo--prev-buffer-matching (predicate &optional n)
  "Switch to the previous buffer matching PREDICATE.
N is the number of times to cycle (default 1).
Cycles backward through the buffer list."
  (let ((n (or n 1))
        (orig-buf (current-buffer))
        (bufs (reverse (buffer-list))))
    (dotimes (_ n)
      (let ((candidates (cdr (memq (current-buffer) bufs))))
        ;; Wrap around
        (setq candidates (append candidates
                                  (seq-take-while (lambda (b) (not (eq b (current-buffer)))) bufs)))
        (let ((found (seq-find predicate candidates)))
          (when found
            (switch-to-buffer found t)))))))

;;; Concrete buffer cycling functions

(defun cpo-next-buffer-no-star (&optional n)
  "Switch to the next buffer, skipping *-prefixed buffers."
  (interactive "p")
  (cpo--next-buffer-matching (lambda (b) (not (cpo-buffer--star-p b))) n))

(defun cpo-prev-buffer-no-star (&optional n)
  "Switch to the previous buffer, skipping *-prefixed buffers."
  (interactive "p")
  (cpo--prev-buffer-matching (lambda (b) (not (cpo-buffer--star-p b))) n))

(defun cpo-next-modified-buffer (&optional n)
  "Switch to the next modified buffer."
  (interactive "p")
  (cpo--next-buffer-matching #'cpo-buffer--modified-p n))

(defun cpo-prev-modified-buffer (&optional n)
  "Switch to the previous modified buffer."
  (interactive "p")
  (cpo--prev-buffer-matching #'cpo-buffer--modified-p n))

(defun cpo-next-file-buffer (&optional n)
  "Switch to the next file-backed buffer."
  (interactive "p")
  (cpo--next-buffer-matching #'cpo-buffer--file-p n))

(defun cpo-prev-file-buffer (&optional n)
  "Switch to the previous file-backed buffer."
  (interactive "p")
  (cpo--prev-buffer-matching #'cpo-buffer--file-p n))

(defun cpo-next-modified-file-buffer (&optional n)
  "Switch to the next modified file-backed buffer."
  (interactive "p")
  (cpo--next-buffer-matching (lambda (b) (and (cpo-buffer--file-p b)
                                               (cpo-buffer--modified-p b)))
                              n))

(defun cpo-prev-modified-file-buffer (&optional n)
  "Switch to the previous modified file-backed buffer."
  (interactive "p")
  (cpo--prev-buffer-matching (lambda (b) (and (cpo-buffer--file-p b)
                                               (cpo-buffer--modified-p b)))
                              n))

;;; Repeatable motion definitions

(repeatable-motion-define-pair 'cpo-next-buffer-no-star
                               'cpo-prev-buffer-no-star)
(repeatable-motion-define-pair 'next-buffer
                               'previous-buffer)
(repeatable-motion-define-pair 'cpo-next-modified-buffer
                               'cpo-prev-modified-buffer)
(repeatable-motion-define-pair 'cpo-next-file-buffer
                               'cpo-prev-file-buffer)
(repeatable-motion-define-pair 'cpo-next-modified-file-buffer
                               'cpo-prev-modified-file-buffer)


(provide 'cpo-buffer-management)
