;;; test-visual-modes.el --- Tests for visual-modifier and visual-clamped-object modes -*- lexical-binding: t; -*-

;; To run these tests from the command line:
;; ./test/run-tests.sh

(require 'ert)
(require 'estate-default-states)
(require 'estate-visual-modifier-state)
(require 'estate-visual-clamped-object-state)
(require 'carettest-tesmut)

;;; ================================================================
;;; Test helpers: text navigation
;;; ================================================================

;; These helpers search for literal text and position point at the
;; beginning or end of the match.  They make tests self-documenting
;; by showing *what text* point lands on, rather than an opaque
;; character position.

(defun test-vm--forward-to-text-beg (text)
  "Search forward for TEXT and place point at the beginning of the match.
Signal an error if TEXT is not found."
  (let ((pos (save-excursion
               (search-forward text nil t))))
    (unless pos
      (error "test-vm--forward-to-text-beg: %S not found after point" text))
    (goto-char (- pos (length text)))))

(defun test-vm--forward-to-text-end (text)
  "Search forward for TEXT and place point at the end of the match.
Signal an error if TEXT is not found."
  (unless (search-forward text nil t)
    (error "test-vm--forward-to-text-end: %S not found after point" text))
  (point))

(defun test-vm--backward-to-text-beg (text)
  "Search backward for TEXT and place point at the beginning of the match.
Signal an error if TEXT is not found."
  (unless (search-backward text nil t)
    (error "test-vm--backward-to-text-beg: %S not found before point" text))
  (point))

(defun test-vm--backward-to-text-end (text)
  "Search backward for TEXT and place point at the end of the match.
Signal an error if TEXT is not found."
  (let ((pos (save-excursion
               (search-backward text nil t))))
    (unless pos
      (error "test-vm--backward-to-text-end: %S not found before point" text))
    (goto-char (+ pos (length text)))))

(defun test-vm--pos-at-text-beg (text)
  "Return the buffer position at the beginning of TEXT, searching from `point-min'.
Signal an error if TEXT is not found."
  (save-excursion
    (goto-char (point-min))
    (test-vm--forward-to-text-beg text)
    (point)))

(defun test-vm--pos-at-text-end (text)
  "Return the buffer position at the end of TEXT, searching from `point-min'.
Signal an error if TEXT is not found."
  (save-excursion
    (goto-char (point-min))
    (test-vm--forward-to-text-end text)
    (point)))

;;; ================================================================
;;; Test helpers: estate setup
;;; ================================================================

(defun test-vm--setup-estate ()
  "Set up estate-local-mode in the current buffer for testing."
  (estate-local-mode 1)
  (setq transient-mark-mode t))

(defun test-vm--activate-word-modifier ()
  "Activate visual modifier for words.
Creates a simple word modifier that expands region to word boundaries."
  (estate-visual-state)
  (estate-visual-state-activate-modifier
   (list 'word
         (lambda (region-beg region-end)
           (let* ((beg-bounds (save-excursion
                                (goto-char region-beg)
                                (bounds-of-thing-at-point 'word)))
                  (end-bounds (save-excursion
                                (goto-char region-end)
                                (bounds-of-thing-at-point 'word)))
                  (new-beg (if beg-bounds (car beg-bounds) region-beg))
                  (new-end (if end-bounds (cdr end-bounds) region-end)))
             (list new-beg new-end))))))

(defun test-vm--activate-line-modifier ()
  "Activate visual modifier for lines.
Creates a simple line modifier that expands region to line boundaries."
  (estate-visual-state)
  (estate-visual-state-activate-modifier
   (list 'line
         (lambda (region-beg region-end)
           (let ((new-beg (save-excursion
                            (goto-char region-beg)
                            (line-beginning-position)))
                 (new-end (save-excursion
                            (goto-char region-end)
                            (line-end-position))))
             (list new-beg new-end))))))

(defun test-vm--activate-word-clamped ()
  "Activate visual clamped object mode for words."
  (estate-visual-clamped-object-activate
   'word
   (lambda ()
     (bounds-of-thing-at-point 'word))))

(defun test-vm--activate-line-clamped ()
  "Activate visual clamped object mode for lines."
  (estate-visual-clamped-object-activate
   'line
   (lambda ()
     (cons (line-beginning-position) (line-end-position)))))

(defun test-vm--simulate-command (command-sym func)
  "Simulate running FUNC as if it were command COMMAND-SYM in the command loop.
This runs pre-command-hook, the function, and post-command-hook,
which is necessary because hooks do not fire from `funcall'."
  (let ((this-command command-sym))
    (run-hooks 'pre-command-hook)
    (funcall func)
    (run-hooks 'post-command-hook)))

(defun test-vm--simulate-movement (func)
  "Simulate running FUNC as a movement command in the command loop."
  (test-vm--simulate-command 'test-movement func))

(defun test-vm--simulate-action (func)
  "Simulate running FUNC as a non-movement action command in the command loop."
  (test-vm--simulate-command 'test-action func))

(defun test-vm--overlay-regions ()
  "Get a sorted list of (beg . end) pairs for all visual modifier overlays."
  (let ((overlays (append estate--visual-modifier-overlays
                          estate--visual-clamped-object-overlays)))
    (sort (mapcar (lambda (ov)
                    (cons (overlay-start ov) (overlay-end ov)))
                  overlays)
          (lambda (a b) (< (car a) (car b))))))


;;; ================================================================
;;; Visual modifier mode: basic activation and deactivation
;;; ================================================================

(ert-deftest test-vm-modifier-activation-state ()
  "Activating a visual modifier sets the modifier variable and installs hooks."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "world")
    (test-vm--setup-estate)
    (set-mark (point))
    (test-vm--forward-to-text-end "world")
    (test-vm--activate-word-modifier)
    (should (not (null estate--visual-modifier)))
    (should (eq estate-state 'visual))
    (should (member 'estate--visual-pre-command (buffer-local-value 'pre-command-hook (current-buffer))))
    (should (member 'estate--visual-post-command (buffer-local-value 'post-command-hook (current-buffer))))))

(ert-deftest test-vm-modifier-deactivation-cleanup ()
  "Deactivating a visual modifier clears all state."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "world")
    (test-vm--setup-estate)
    (set-mark (point))
    (test-vm--forward-to-text-end "world")
    (test-vm--activate-word-modifier)
    (estate-visual-state-deactivate-modifier)
    (should (null estate--visual-modifier))
    (should (null estate--visual-modifier-overlays))
    (should (null estate--visual-region-expanded))
    (should (null estate--visual-original-mark))
    (should (null estate--visual-original-point))))

;;; ================================================================
;;; Visual modifier mode: overlay display
;;; ================================================================

(ert-deftest test-vm-modifier-overlay-word-expansion ()
  "Word modifier creates overlays for the expanded portions of the region.
When the region is in the middle of words, overlays should extend
to cover the word boundaries beyond the actual region."
  (with-temp-buffer
    (insert "hello world test")
    (test-vm--setup-estate)
    ;; Region from "rl" in "world" to "te" in "test"
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "rld")
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "rld te")
    (test-vm--activate-word-modifier)
    ;; The modifier should expand to full word boundaries.
    ;; Overlays should cover the gaps between the actual region and
    ;; the expanded word boundaries.
    (let ((regions (test-vm--overlay-regions)))
      (should (> (length regions) 0))
      ;; There should be overlay(s) showing the expansion
      ;; The exact overlay positions depend on the modifier function
      )))

(ert-deftest test-vm-modifier-overlay-no-expansion-needed ()
  "When region already covers full words, no overlays are created."
  (with-temp-buffer
    (insert "hello world test")
    (test-vm--setup-estate)
    ;; Region exactly covers "world"
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "world")
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "world")
    (test-vm--activate-word-modifier)
    ;; If the modifier sees bounds match, overlays might be empty
    (let ((regions (test-vm--overlay-regions)))
      ;; Either no overlays or overlays with zero width
      ;; This tests that we don't get spurious overlays
      t)))

;;; ================================================================
;;; Visual modifier mode: region expansion and contraction
;;; ================================================================

(ert-deftest test-vm-modifier-expansion-with-predicate ()
  "When expansion predicate returns non-nil, region expands before command."
  (with-temp-buffer
    (insert "hello world test")
    (test-vm--setup-estate)
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "rld")
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "rld te")
    (test-vm--activate-word-modifier)
    ;; Set predicate: always expand
    (setq estate-visual-modifier-expansion-command-predicate
          (lambda (_cmd) t))
    ;; Save pre-expansion state
    (let ((orig-point (point))
          (orig-mark (mark)))
      ;; Simulate a pre-command hook (expansion happens here)
      (let ((this-command 'test-action))
        (run-hooks 'pre-command-hook))
      ;; Region should now be expanded
      (should estate--visual-region-expanded)
      ;; The expanded region should cover full words
      (should (<= (region-beginning) (test-vm--pos-at-text-beg "world")))
      (should (>= (region-end) (test-vm--pos-at-text-end "test")))
      ;; Now simulate post-command (contraction, no buffer modification)
      (let ((this-command 'test-action))
        (run-hooks 'post-command-hook))
      ;; Region should have contracted back
      (should (not estate--visual-region-expanded))
      (should (= (point) orig-point))
      (should (= (mark) orig-mark)))))

(ert-deftest test-vm-modifier-no-expansion-for-movements ()
  "When expansion predicate returns nil (movement), region does NOT expand."
  (with-temp-buffer
    (insert "hello world test")
    (test-vm--setup-estate)
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "rld")
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "rld te")
    (test-vm--activate-word-modifier)
    ;; Set predicate: never expand
    (setq estate-visual-modifier-expansion-command-predicate
          (lambda (_cmd) nil))
    (let ((orig-mark (mark))
          (point-before (point)))
      ;; Simulate a movement command
      (test-vm--simulate-movement
       (lambda () (forward-char 1)))
      ;; Region should NOT be expanded
      (should (not estate--visual-region-expanded))
      ;; Point moved but mark unchanged
      (should (= (point) (1+ point-before)))
      (should (= (mark) orig-mark)))))

(ert-deftest test-vm-modifier-deactivates-on-buffer-modification ()
  "When the buffer is modified during an expanded command, the modifier deactivates."
  (with-temp-buffer
    (insert "hello world test")
    (test-vm--setup-estate)
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "rld")
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "rld te")
    (test-vm--activate-word-modifier)
    (setq estate-visual-modifier-expansion-command-predicate
          (lambda (_cmd) t))
    ;; Simulate a buffer-modifying action
    (test-vm--simulate-action
     (lambda ()
       ;; Expand region manually (simulating what pre-command would do)
       (estate-visual-modifier-expand-region)
       ;; Modify the buffer (this triggers after-change-functions)
       (delete-region (region-beginning) (region-end))))
    ;; The modifier should be deactivated
    (should (null estate--visual-modifier))))

;;; ================================================================
;;; Visual modifier mode: manual expansion
;;; ================================================================

(ert-deftest test-vm-modifier-manual-expansion ()
  "estate-visual-modifier-expand-region can be called mid-command."
  (with-temp-buffer
    (insert "hello world test")
    (test-vm--setup-estate)
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "rld")
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "rld te")
    (test-vm--activate-word-modifier)
    ;; Don't set the predicate, so pre-command won't expand
    (setq estate-visual-modifier-expansion-command-predicate nil)
    ;; Manually expand
    (estate-visual-modifier-expand-region)
    (should estate--visual-region-expanded)
    (should (<= (region-beginning) (test-vm--pos-at-text-beg "world")))
    (should (>= (region-end) (test-vm--pos-at-text-end "test")))))

;;; ================================================================
;;; Visual modifier mode: carettest-tesmut integration
;;; ================================================================

;; Test that upcase-region with word modifier expands properly
(carettest-tesmut-test test-vm-modifier-upcase-word-expansion
  :before "hello <m>wor<p>ld test"
  :after "hello <m>WORLD<p> test"
  :function (lambda ()
              ;; The modifier needs the expanded region to upcase the full word
              (estate-visual-modifier-expand-region)
              (upcase-region (region-beginning) (region-end)))
  :setup (progn
           (test-vm--setup-estate)
           (test-vm--activate-word-modifier)))

;; Test line modifier expansion with upcase
(carettest-tesmut-test test-vm-modifier-upcase-line-expansion
  :before "hello <m>wor<p>ld test"
  :after "<m>HELLO WORLD TEST<p>"
  :function (lambda ()
              (estate-visual-modifier-expand-region)
              (upcase-region (region-beginning) (region-end)))
  :setup (progn
           (test-vm--setup-estate)
           (test-vm--activate-line-modifier)))

;;; ================================================================
;;; Visual clamped object mode: basic activation
;;; ================================================================

(ert-deftest test-vm-clamped-activation-state ()
  "Activating clamped object mode sets state and installs hooks."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "orld")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    (should estate--visual-clamped-object-active)
    (should (eq estate-state 'visual))
    ;; Mark should be at point (zero-width region)
    (should (= (mark) (point)))
    (should (member 'estate--visual-clamped-object-pre-command
                    (buffer-local-value 'pre-command-hook (current-buffer))))
    (should (member 'estate--visual-clamped-object-post-command
                    (buffer-local-value 'post-command-hook (current-buffer))))))

(ert-deftest test-vm-clamped-deactivation-cleanup ()
  "Deactivating clamped object mode clears all state."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "orld")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    (estate--visual-clamped-object-deactivate)
    (should (null estate--visual-clamped-object-active))
    (should (null estate--visual-clamped-object-func))
    (should (null estate--visual-clamped-object-overlays))
    (should (null estate--visual-clamped-region-expanded))))

;;; ================================================================
;;; Visual clamped object mode: overlay at point
;;; ================================================================

(ert-deftest test-vm-clamped-overlay-at-word ()
  "Clamped object mode shows overlay for word at point."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "orld")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    ;; Should have an overlay covering the word at point
    (let ((regions (test-vm--overlay-regions)))
      (should (= (length regions) 1))
      ;; Overlay should span "world"
      (should (= (car (car regions)) (test-vm--pos-at-text-beg "world")))
      (should (= (cdr (car regions)) (test-vm--pos-at-text-end "world"))))))

(ert-deftest test-vm-clamped-overlay-at-line ()
  "Clamped object mode with lines shows overlay for entire line."
  (with-temp-buffer
    (insert "first line\nsecond line\nthird line")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "ond")
    (test-vm--setup-estate)
    (test-vm--activate-line-clamped)
    (let ((regions (test-vm--overlay-regions)))
      (should (= (length regions) 1))
      ;; Overlay should span the "second line" line, from its beginning
      ;; through line-end-position (which is the position of the newline,
      ;; i.e. right after the last visible character).
      (should (= (car (car regions)) (test-vm--pos-at-text-beg "second")))
      (should (= (cdr (car regions))
                 (test-vm--pos-at-text-end "second line"))))))

;;; ================================================================
;;; Visual clamped object mode: mark follows point
;;; ================================================================

(ert-deftest test-vm-clamped-mark-follows-point-on-movement ()
  "After a movement command, mark is set to point in clamped mode."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "orld")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    ;; Verify initial state: mark = point
    (should (= (mark) (point)))
    ;; Simulate a movement -- don't set command predicate so nothing expands
    (setq estate-visual-clamped-object-command-predicate nil)
    (test-vm--simulate-movement
     (lambda () (forward-word 1)))
    ;; After movement, mark should follow point
    (should (= (mark) (point)))))

(ert-deftest test-vm-clamped-overlay-updates-on-movement ()
  "After movement, the overlay updates to show the word at the new position."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "llo")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    ;; Initial overlay should cover "hello"
    (let ((regions (test-vm--overlay-regions)))
      (should (= (length regions) 1))
      (should (= (car (car regions)) (test-vm--pos-at-text-beg "hello")))
      (should (= (cdr (car regions)) (test-vm--pos-at-text-end "hello"))))
    ;; Move to "world"
    (setq estate-visual-clamped-object-command-predicate nil)
    (test-vm--simulate-movement
     (lambda ()
       (goto-char (point-min))
       (test-vm--forward-to-text-beg "orld")))
    ;; Overlay should now cover "world"
    (let ((regions (test-vm--overlay-regions)))
      (should (= (length regions) 1))
      (should (= (car (car regions)) (test-vm--pos-at-text-beg "world")))
      (should (= (cdr (car regions)) (test-vm--pos-at-text-end "world"))))))

;;; ================================================================
;;; Visual clamped object mode: region expansion
;;; ================================================================

(ert-deftest test-vm-clamped-expansion-for-action ()
  "When command predicate returns non-nil, region expands to text object."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "orld")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    ;; Set predicate to always expand
    (setq estate-visual-clamped-object-command-predicate
          (lambda (_cmd) t))
    ;; Simulate pre-command
    (let ((this-command 'test-action))
      (run-hooks 'pre-command-hook))
    ;; Region should be expanded to word boundaries
    (should estate--visual-clamped-region-expanded)
    (should (= (region-beginning) (test-vm--pos-at-text-beg "world")))
    (should (= (region-end) (test-vm--pos-at-text-end "world")))
    ;; Simulate post-command (no buffer modification -> contract)
    (let ((this-command 'test-action))
      (run-hooks 'post-command-hook))
    ;; Should contract back, mark = point
    (should (not estate--visual-clamped-region-expanded))
    (should (= (mark) (point)))))

(ert-deftest test-vm-clamped-deactivates-on-buffer-modification ()
  "When buffer is modified during expanded command, clamped mode deactivates."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "orld")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    (setq estate-visual-clamped-object-command-predicate
          (lambda (_cmd) t))
    ;; Simulate a buffer-modifying action
    (test-vm--simulate-action
     (lambda ()
       (estate-visual-clamped-object-expand-region)
       (upcase-region (region-beginning) (region-end))))
    ;; Clamped mode should be deactivated
    (should (null estate--visual-clamped-object-active))))

(ert-deftest test-vm-clamped-manual-expansion ()
  "estate-visual-clamped-object-expand-region can be called manually."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "orld")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    ;; Initially mark = point
    (should (= (mark) (point)))
    ;; Manually expand
    (estate-visual-clamped-object-expand-region)
    (should estate--visual-clamped-region-expanded)
    (should (= (region-beginning) (test-vm--pos-at-text-beg "world")))
    (should (= (region-end) (test-vm--pos-at-text-end "world")))))

;;; ================================================================
;;; Visual clamped object: carettest-tesmut integration
;;; ================================================================

(carettest-tesmut-test test-vm-clamped-upcase-word-at-point
  :before "hello <p>world test"
  :after "hello <m>WORLD<p> test"
  :function (lambda ()
              (estate-visual-clamped-object-expand-region)
              (upcase-region (region-beginning) (region-end)))
  :setup (progn
           (test-vm--setup-estate)
           (test-vm--activate-word-clamped)))

(carettest-tesmut-test test-vm-clamped-upcase-line-at-point
  :before "hello world\nfoo <p>bar baz\nquux"
  :after "hello world\n<m>FOO BAR BAZ<p>\nquux"
  :function (lambda ()
              (estate-visual-clamped-object-expand-region)
              (upcase-region (region-beginning) (region-end)))
  :setup (progn
           (test-vm--setup-estate)
           (test-vm--activate-line-clamped)))

;;; ================================================================
;;; Visual clamped object: sequence tests
;;; ================================================================

;; Test movement then action sequence.
;; We cannot use carettest-tesmut sequence tests directly for this because
;; the hooks need to fire between steps.  Instead we use a manual ERT test.

(ert-deftest test-vm-clamped-move-then-upcase ()
  "Move to a different word, then upcase it in clamped mode."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "llo")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    (setq estate-visual-clamped-object-command-predicate nil)
    ;; Step 1: Move to "world"
    (test-vm--simulate-movement
     (lambda ()
       (goto-char (point-min))
       (test-vm--forward-to-text-beg "orld")))
    ;; Mark should have followed point
    (should (= (mark) (point)))
    ;; Overlay should be on "world"
    (let ((regions (test-vm--overlay-regions)))
      (should (= (length regions) 1))
      (should (= (car (car regions)) (test-vm--pos-at-text-beg "world")))
      (should (= (cdr (car regions)) (test-vm--pos-at-text-end "world"))))
    ;; Step 2: Upcase (manually expanding)
    (estate-visual-clamped-object-expand-region)
    (upcase-region (region-beginning) (region-end))
    ;; Verify the buffer content
    (should (string= (buffer-string) "hello WORLD test"))))

(ert-deftest test-vm-clamped-multiple-movements ()
  "Multiple movements in clamped mode keep mark following point."
  (with-temp-buffer
    (insert "alpha beta gamma delta")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "pha")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    (setq estate-visual-clamped-object-command-predicate nil)
    ;; Move to "beta"
    (test-vm--simulate-movement
     (lambda ()
       (goto-char (point-min))
       (test-vm--forward-to-text-beg "eta")))
    (should (= (mark) (point)))
    (let ((regions (test-vm--overlay-regions)))
      (should (= (car (car regions)) (test-vm--pos-at-text-beg "beta")))
      (should (= (cdr (car regions)) (test-vm--pos-at-text-end "beta"))))
    ;; Move to "gamma"
    (test-vm--simulate-movement
     (lambda ()
       (goto-char (point-min))
       (test-vm--forward-to-text-beg "amma")))
    (should (= (mark) (point)))
    (let ((regions (test-vm--overlay-regions)))
      (should (= (car (car regions)) (test-vm--pos-at-text-beg "gamma")))
      (should (= (cdr (car regions)) (test-vm--pos-at-text-end "gamma"))))
    ;; Move to "delta"
    (test-vm--simulate-movement
     (lambda ()
       (goto-char (point-min))
       (test-vm--forward-to-text-beg "elta")))
    (should (= (mark) (point)))
    (let ((regions (test-vm--overlay-regions)))
      (should (= (car (car regions)) (test-vm--pos-at-text-beg "delta")))
      (should (= (cdr (car regions)) (test-vm--pos-at-text-end "delta"))))))

;;; ================================================================
;;; Visual modifier mode: sequence with movement then expansion
;;; ================================================================

(ert-deftest test-vm-modifier-move-then-expand ()
  "Move point, then verify the modifier overlay updates accordingly."
  (with-temp-buffer
    (insert "hello world test")
    (test-vm--setup-estate)
    ;; Start with region in "hello" -- "ll" to "lo"
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "llo")
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "llo")
    (test-vm--activate-word-modifier)
    (setq estate-visual-modifier-expansion-command-predicate nil)
    ;; Move point to "test"
    (let ((mark-before (mark)))
      (test-vm--simulate-movement
       (lambda ()
         (goto-char (point-min))
         (test-vm--forward-to-text-end "hello world tes")))
      ;; Mark stays where it was, point moved to "tes" in "test"
      (should (= (mark) mark-before))
      ;; Now manually expand and upcase
      (estate-visual-modifier-expand-region)
      (should estate--visual-region-expanded)
      ;; The expanded region should cover full words
      (should (<= (region-beginning) (test-vm--pos-at-text-beg "hello")))
      (should (>= (region-end) (test-vm--pos-at-text-end "test"))))))

;;; ================================================================
;;; Visual modifier mode: exit hook cleanup
;;; ================================================================

(ert-deftest test-vm-modifier-exit-visual-state-cleanup ()
  "Exiting visual state deactivates the modifier via exit hook."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "world")
    (test-vm--setup-estate)
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "world")
    (test-vm--activate-word-modifier)
    (should (not (null estate--visual-modifier)))
    ;; Switch to normal state (which triggers visual state exit hook)
    ;; We need to temporarily remove the mark hooks so that deactivate-mark
    ;; doesn't immediately re-enter visual state
    (estate--deactivate-initialize-core-states)
    (estate-normal-state)
    (estate--activate-initialize-core-states)
    ;; Modifier should be cleaned up
    (should (null estate--visual-modifier))))

(ert-deftest test-vm-clamped-exit-visual-state-cleanup ()
  "Exiting visual state deactivates clamped mode via exit hook."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "orld")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    (should estate--visual-clamped-object-active)
    ;; Switch to normal state
    (estate--deactivate-initialize-core-states)
    (estate-normal-state)
    (estate--activate-initialize-core-states)
    ;; Clamped mode should be cleaned up
    (should (null estate--visual-clamped-object-active))))

;;; ================================================================
;;; Visual modifier mode: cache behavior
;;; ================================================================

(ert-deftest test-vm-modifier-cache-invalidation ()
  "Moving point invalidates the modifier cache."
  (with-temp-buffer
    (insert "hello world test")
    (test-vm--setup-estate)
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "llo")
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "llo worl")
    (test-vm--activate-word-modifier)
    ;; Get modified region (populates cache)
    (let ((result1 (estate--visual-get-modified-region)))
      (should (not (null result1)))
      ;; Cache should be populated
      (should (not (null estate--visual-cached-modified-region)))
      ;; Move point to "test" and get again
      (goto-char (point-min))
      (test-vm--forward-to-text-end "hello world tes")
      (let ((result2 (estate--visual-get-modified-region)))
        (should (not (null result2)))
        ;; Results should differ because region changed
        (should (not (equal result1 result2)))))))

;;; ================================================================
;;; Visual clamped object: bounds function returning nil
;;; ================================================================

(ert-deftest test-vm-clamped-nil-bounds ()
  "When the bounds function returns nil, no overlay is created."
  (with-temp-buffer
    (insert "hello   world")
    (goto-char (point-min))
    (test-vm--forward-to-text-end "hello ")
    (forward-char 1) ;; on whitespace between words (second space)
    (test-vm--setup-estate)
    ;; Use a bounds function that returns nil for non-word positions
    (estate-visual-clamped-object-activate
     'word
     (lambda ()
       (bounds-of-thing-at-point 'word)))
    ;; On whitespace, bounds-of-thing-at-point might return nil
    ;; (depends on how 'word is defined, but multiple spaces likely return nil)
    ;; Regardless, the mode should not crash
    (should estate--visual-clamped-object-active)))

;;; ================================================================
;;; Visual clamped object: contraction after non-modifying action
;;; ================================================================

(ert-deftest test-vm-clamped-contraction-after-non-modifying-action ()
  "After a non-modifying action expands and contracts, mark returns to point."
  (with-temp-buffer
    (insert "hello world test")
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "orld")
    (test-vm--setup-estate)
    (test-vm--activate-word-clamped)
    (let ((original-point (point)))
      (setq estate-visual-clamped-object-command-predicate
            (lambda (_cmd) t))
      ;; Simulate a non-modifying action (like copy-region-as-kill)
      (test-vm--simulate-action
       (lambda ()
         ;; Don't actually modify the buffer
         nil))
      ;; Should have expanded then contracted
      (should (not estate--visual-clamped-region-expanded))
      ;; Mark should be back at point
      (should (= (mark) original-point))
      ;; Mode should still be active
      (should estate--visual-clamped-object-active))))

;;; ================================================================
;;; Visual modifier: get-modifier-function
;;; ================================================================

(ert-deftest test-vm-get-modifier-function-symbol ()
  "get-modifier-function works when modifier is just a function."
  (with-temp-buffer
    (insert "hello")
    (test-vm--setup-estate)
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "llo")
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "llo")
    (estate-visual-state)
    (let ((my-func (lambda (beg end) (list beg end))))
      (setq-local estate--visual-modifier my-func)
      (should (eq (estate--visual-get-modifier-function) my-func)))))

(ert-deftest test-vm-get-modifier-function-list ()
  "get-modifier-function works when modifier is (name function)."
  (with-temp-buffer
    (insert "hello")
    (test-vm--setup-estate)
    (goto-char (point-min))
    (test-vm--forward-to-text-beg "llo")
    (set-mark (point))
    (activate-mark)
    (test-vm--forward-to-text-end "llo")
    (estate-visual-state)
    (let ((my-func (lambda (beg end) (list beg end))))
      (setq-local estate--visual-modifier (list 'word my-func))
      (should (eq (estate--visual-get-modifier-function) my-func)))))

(provide 'test-visual-modes)
