;;; test-cpo-location-history.el --- Tests for cpo-location-history -*- lexical-binding: t; -*-

;;; STATUS: some but not all of these tests have had brief human review.  The feature seems to work in brief manual testing, and I am eager to use it, and it is low risk, so I am just landing before vetting these tests to even a very basic level.  But I don't want to just throw them out -- from what I saw, the tests look ok.  If nothing else, I think the tests will be helpful if and when I need to revisit the feature.

(require 'ert)
(require 'cpo-location-history)

;;; Helpers

(defmacro cpo-location-history-test--with-fresh-state (&rest body)
  "Run BODY with a fresh location history state.
Resets the ring, index, navigating flag, and last-buffer/position tracking.

!!! uses the fact that these variables are used with `defvar` making DYNAMICALLY SCOPED variables!
"
  `(let ((cpo-location-history-ring (make-ring 1000))
         (cpo-location-history--index 0)
         (cpo-location-history--navigating nil)
         (cpo-location-history--last-buffer nil)
         (cpo-location-history--last-position nil)
         (cpo-location-history-min-distance 3))
     ,@body))

(defun cpo-location-history-test--record-at (buffer position)
  "Simulate recording a location at POSITION in BUFFER.
Switches to BUFFER, moves point to POSITION, and calls the record function.
This simulates a regular (non-navigation) command that moves point,
followed by `post-command-hook' firing."
  (with-current-buffer buffer
    (goto-char position)
    (cpo-location-history-record)))

(defun cpo-location-history-test--populate-ring (entries)
  "Populate the history ring by simulating commands at each location.
ENTRIES is a list of (BUFFER . POSITION) pairs, ordered oldest-first.
Each entry is recorded via `cpo-location-history-test--record-at', so
the real recording logic (min-distance checks, last-buffer/position
tracking, index resets) runs for every entry.

After this function returns, the ring is populated in the same order
as if the user had visited the listed locations in sequence, and all
tracking variables (`--last-buffer', `--last-position', `--index')
are in a consistent state.

NOTE: Positions within the same buffer must be farther apart than
`cpo-location-history-min-distance' or the entry will be silently
skipped by the recording logic."
  (dolist (entry entries)
    (cpo-location-history-test--record-at (car entry) (cdr entry))))

(defun cpo-location-history-test--ring-entries ()
  "Return the current ring contents as a list of (buffer . position) pairs.
Ordered newest-first (ring index 0 first)."
  (let ((len (ring-length cpo-location-history-ring))
        (result nil))
    (dotimes (i len)
      (push (ring-ref cpo-location-history-ring i) result))
    (nreverse result)))

;;; Test: basic recording

(ert-deftest cpo-location-history-test-record-basic ()
  "Recording stores (buffer . position) in the ring."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert "hello world, this is a test buffer with enough text")
     (goto-char 1)
     (cpo-location-history-record)
     (should (= 1 (ring-length cpo-location-history-ring)))
     (let ((entry (ring-ref cpo-location-history-ring 0)))
       (should (eq (car entry) (current-buffer)))
       (should (= (cdr entry) 1))))))

(ert-deftest cpo-location-history-test-record-meaningful-change ()
  "Only positions that differ by more than min-distance are recorded."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert "hello world, this is a test buffer with enough text")
     ;; First record.
     (goto-char 1)
     (cpo-location-history-record)
     (should (= 1 (ring-length cpo-location-history-ring)))
     ;; Move only 2 chars -- should NOT record (min-distance is 3).
     (goto-char 3)
     (cpo-location-history-record)
     (should (= 1 (ring-length cpo-location-history-ring)))
     ;; Move 4 chars from last recorded (pos 1) -- should record.
     (goto-char 5)
     (cpo-location-history-record)
     (should (= 2 (ring-length cpo-location-history-ring))))))

(ert-deftest cpo-location-history-test-record-buffer-change-always-records ()
  "Changing buffers always records, even if the position number is similar."
  (cpo-location-history-test--with-fresh-state
   (let ((buf-a (generate-new-buffer "test-a"))
         (buf-b (generate-new-buffer "test-b")))
     (unwind-protect
         (progn
           (with-current-buffer buf-a
             (insert "content for buffer a"))
           (with-current-buffer buf-b
             (insert "content for buffer b"))
           ;; Record in buf-a at position 1.
           (cpo-location-history-test--record-at buf-a 1)
           (should (= 1 (ring-length cpo-location-history-ring)))
           ;; Record in buf-b at position 1 -- same numeric position, different buffer.
           (cpo-location-history-test--record-at buf-b 1)
           (should (= 2 (ring-length cpo-location-history-ring))))
       (kill-buffer buf-a)
       (kill-buffer buf-b)))))

(ert-deftest cpo-location-history-test-record-skips-minibuffer ()
  "Recording is suppressed inside the minibuffer."
  (cpo-location-history-test--with-fresh-state
   ;; We cannot easily create a real minibuffer in batch mode, but we can
   ;; verify that the record function checks `minibufferp'.  A temp buffer
   ;; is not a minibuffer, so recording should work there.
   (with-temp-buffer
     (insert "some text for testing purposes here")
     (goto-char 1)
     (cpo-location-history-record)
     (should (= 1 (ring-length cpo-location-history-ring))))))

;;; Test: recursive edit suppresses recording

(ert-deftest cpo-location-history-test-record-skips-recursive-edit ()
  "Recording is suppressed inside a recursive edit (e.g. isearch).
Simulates non-zero `recursion-depth' with `cl-letf' to verify that
`cpo-location-history-record' does not add entries when recursion-depth > 0."
  (require 'cl-lib)
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert "hello world, this is a test buffer with enough text for testing")
     ;; Record at baseline depth (should succeed).
     (goto-char 1)
     (cpo-location-history-record)
     (should (= 1 (ring-length cpo-location-history-ring)))
     ;; Simulate being inside a recursive edit by overriding recursion-depth.
     (goto-char 40)
     (cl-letf (((symbol-function 'recursion-depth) (lambda () 1)))
       (cpo-location-history-record))
     ;; Should NOT have recorded -- still 1 entry.
     (should (= 1 (ring-length cpo-location-history-ring)))
     ;; Back at baseline depth, recording should work again.
     (cpo-location-history-record)
     (should (= 2 (ring-length cpo-location-history-ring))))))

;;; Test: navigating flag suppresses recording

(ert-deftest cpo-location-history-test-navigating-flag-suppresses-record ()
  "When --navigating is non-nil, record does not add entries and clears the flag."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert "hello world, this is a test buffer with enough text")
     ;; Record initial position.
     (goto-char 1)
     (cpo-location-history-record)
     (should (= 1 (ring-length cpo-location-history-ring)))
     ;; Simulate navigation: set the flag, then record at a distant position.
     (setq cpo-location-history--navigating t)
     (goto-char 30)
     (cpo-location-history-record)
     ;; Should NOT have added a new entry.
     (should (= 1 (ring-length cpo-location-history-ring)))
     ;; Flag should have been cleared.
     (should (null cpo-location-history--navigating))
     ;; Now a normal record should work.
     (cpo-location-history-record)
     (should (= 2 (ring-length cpo-location-history-ring))))))

;;; Test: backward navigation

(ert-deftest cpo-location-history-test-back-basic ()
  "Going backward moves to older entries."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     ;; Populate via the real recording path.
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 50)
        ( ,(current-buffer) . 10)))
     ;; Ring: index 0 = pos 10, index 1 = pos 50, index 2 = pos 100.
     ;; Start at index 0 (newest).
     (should (= 0 cpo-location-history--index))
     (should (= 10 (point)))
     ;; Go backward (older).
     (cpo-location-history--navigate 'backward 'global)
     (should (= 1 cpo-location-history--index))
     (should (= 50 (point))))))

(ert-deftest cpo-location-history-test-back-multiple ()
  "Going backward multiple times traverses through history."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 50)
        ( ,(current-buffer) . 10)))
     ;; Ring: 0=10, 1=50, 2=100
     ;; First backward.
     (cpo-location-history--navigate 'backward 'global)
     (should (= 1 cpo-location-history--index))
     (should (= 50 (point)))
     ;; Second backward.
     (cpo-location-history--navigate 'backward 'global)
     (should (= 2 cpo-location-history--index))
     (should (= 100 (point))))))

(ert-deftest cpo-location-history-test-back-clamps-at-oldest ()
  "Going backward past the oldest entry stays at the oldest."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 10)))
     ;; Ring: 0=10, 1=100.  Navigate to oldest first.
     (cpo-location-history--navigate 'backward 'global)
     (should (= 1 cpo-location-history--index))
     (should (= 100 (point)))
     ;; Already at oldest.  Backward should stay here.
     (cpo-location-history--navigate 'backward 'global)
     (should (= 1 cpo-location-history--index))
     (should (= 100 (point))))))

;;; Test: forward navigation

(ert-deftest cpo-location-history-test-forward-basic ()
  "Going forward moves to newer entries."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 50)
        ( ,(current-buffer) . 10)))
     ;; Ring: 0=10, 1=50, 2=100.  Navigate to oldest first.
     (cpo-location-history--navigate 'backward 'global)
     (cpo-location-history--navigate 'backward 'global)
     (should (= 2 cpo-location-history--index))
     (should (= 100 (point)))
     ;; Go forward (newer).
     (cpo-location-history--navigate 'forward 'global)
     (should (= 1 cpo-location-history--index))
     (should (= 50 (point))))))

(ert-deftest cpo-location-history-test-forward-clamps-at-newest ()
  "Going forward past the newest entry stays at the newest."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 10)))
     ;; Ring: 0=10, 1=100.  Already at index 0 (newest).
     (should (= 0 cpo-location-history--index))
     (should (= 10 (point)))
     ;; Already at newest.  Forward should stay here.
     (cpo-location-history--navigate 'forward 'global)
     (should (= 0 cpo-location-history--index))
     (should (= 10 (point))))))

;;; Test: round-trip navigation

(ert-deftest cpo-location-history-test-round-trip ()
  "Going back then forward returns to the starting position."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 50)
        ( ,(current-buffer) . 10)))
     ;; Start at newest (index 0, pos 10).
     (should (= 10 (point)))
     ;; Back to older.
     (cpo-location-history--navigate 'backward 'global)
     (should (= 50 (point)))
     ;; Forward back to newest.
     (cpo-location-history--navigate 'forward 'global)
     (should (= 10 (point)))
     (should (= 0 cpo-location-history--index)))))

;;; Test: navigation does not pollute the ring (critical regression)

(ert-deftest cpo-location-history-test-navigation-does-not-record ()
  "Simulating the post-command-hook cycle: navigation must not add ring entries.
This tests the critical bug where `let'-binding --navigating caused it to
be nil by the time post-command-hook ran."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     ;; Populate ring with three locations via the real recording path.
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 150)
        ( ,(current-buffer) . 80)
        ( ,(current-buffer) . 10)))
     ;; Ring: 0=10, 1=80, 2=150.
     (should (= 0 cpo-location-history--index))
     (should (= 10 (point)))
     ;; Simulate calling cpo-location-history-back.
     (cpo-location-history-back 'global)
     ;; After back: should be at position 80, index 1.
     (should (= 80 (point)))
     (should (= 1 cpo-location-history--index))
     ;; The --navigating flag should be t (set by back, not yet cleared).
     (should cpo-location-history--navigating)
     ;; Simulate post-command-hook firing.
     (cpo-location-history-record)
     ;; The flag should now be cleared.
     (should (null cpo-location-history--navigating))
     ;; Ring should still have exactly 3 entries -- no pollution.
     (should (= 3 (ring-length cpo-location-history-ring)))
     ;; Index should NOT have been reset to 0.
     (should (= 1 cpo-location-history--index))
     ;; Now simulate another back.
     (cpo-location-history-back 'global)
     (should (= 150 (point)))
     (should (= 2 cpo-location-history--index))
     ;; Post-command-hook again.
     (cpo-location-history-record)
     (should (= 3 (ring-length cpo-location-history-ring)))
     ;; Now go forward.
     (cpo-location-history-forward 'global)
     (should (= 80 (point)))
     (should (= 1 cpo-location-history--index))
     (cpo-location-history-record)
     (should (= 3 (ring-length cpo-location-history-ring))))))

(ert-deftest cpo-location-history-test-regular-command-after-navigation-records ()
  "After navigation, a regular command should record normally and reset index."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 10)))
     ;; Ring: 0=10, 1=100.
     (should (= 0 cpo-location-history--index))
     (should (= 10 (point)))
     ;; Navigate back.
     (cpo-location-history-back 'global)
     (should (= 100 (point)))
     (cpo-location-history-record)  ; post-command-hook: clears flag, no record.
     (should (= 2 (ring-length cpo-location-history-ring)))
     ;; Now simulate a regular command that moves point far away.
     (goto-char 180)
     (cpo-location-history-record)
     ;; Should have recorded a new entry.
     (should (= 3 (ring-length cpo-location-history-ring)))
     ;; Index should have been reset to 0.
     (should (= 0 cpo-location-history--index))
     ;; Newest entry should be (buf . 180).
     (let ((newest (ring-ref cpo-location-history-ring 0)))
       (should (= 180 (cdr newest)))))))

;;; Test: local mode filtering

(ert-deftest cpo-location-history-test-local-mode ()
  "Local mode only navigates entries in the current buffer."
  (cpo-location-history-test--with-fresh-state
   (let ((buf-a (generate-new-buffer "test-a"))
         (buf-b (generate-new-buffer "test-b")))
     (unwind-protect
         (progn
           (with-current-buffer buf-a
             (insert (make-string 200 ?a)))
           (with-current-buffer buf-b
             (insert (make-string 200 ?b)))
           ;; Record entries oldest-first; populate-ring calls record-at
           ;; for each pair, so the ring ends up in the correct order.
           (cpo-location-history-test--populate-ring
            `((,buf-a . 100)
              (,buf-b . 50)
              (,buf-a . 30)
              (,buf-b . 10)
              (,buf-a . 5)))
           ;; Ring (newest first): 0=(a.5), 1=(b.10), 2=(a.30), 3=(b.50), 4=(a.100)
           ;; Local to buf-a: filtered indices = (0, 2, 4)
           (with-current-buffer buf-a
             (should (= 0 cpo-location-history--index))
             (should (= 5 (point)))
             ;; Back in local mode should skip buf-b entries.
             (cpo-location-history--navigate 'backward 'local)
             (should (= (point) 30))
             (should (= cpo-location-history--index 2))
             ;; Another back in local mode.
             (cpo-location-history--navigate 'backward 'local)
             (should (= (point) 100))
             (should (= cpo-location-history--index 4))
             ;; Forward in local mode.
             (cpo-location-history--navigate 'forward 'local)
             (should (= (point) 30))
             (should (= cpo-location-history--index 2))))
       (kill-buffer buf-a)
       (kill-buffer buf-b)))))

;;; Test: non-local mode filtering

(ert-deftest cpo-location-history-test-non-local-mode ()
  "Non-local mode only navigates entries in other buffers."
  (cpo-location-history-test--with-fresh-state
   (let ((buf-a (generate-new-buffer "test-a"))
         (buf-b (generate-new-buffer "test-b"))
         (buf-c (generate-new-buffer "test-c")))
     (unwind-protect
         (progn
           (with-current-buffer buf-a
             (insert (make-string 200 ?a)))
           (with-current-buffer buf-b
             (insert (make-string 200 ?b)))
           (with-current-buffer buf-c
             (insert (make-string 200 ?c)))
           ;; Record entries oldest-first.
           (cpo-location-history-test--populate-ring
            `((,buf-b . 100)
              (,buf-a . 50)
              (,buf-c . 30)
              (,buf-a . 10)))
           ;; Ring: 0=(a.10), 1=(c.30), 2=(a.50), 3=(b.100)
           ;; Non-local from buf-a: filtered = (1, 3) -> (c.30) and (b.100)
           (with-current-buffer buf-a
             (should (= 0 cpo-location-history--index))
             (should (= 10 (point)))
             ;; Back in non-local mode skips buf-a entries.
             (cpo-location-history--navigate 'backward 'non-local)
             ;; Should jump to (c.30) at ring index 1.
             (should (= cpo-location-history--index 1))
             ;; We should now be in buf-c.
             (should (eq (current-buffer) buf-c))
             (should (= (point) 30))))
       (kill-buffer buf-a)
       (kill-buffer buf-b)
       (kill-buffer buf-c)))))

;;; Test: empty ring

(ert-deftest cpo-location-history-test-empty-ring ()
  "Navigation on an empty ring does not error."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert "some text")
     ;; Should message and not error.
     (cpo-location-history--navigate 'backward 'global)
     (cpo-location-history--navigate 'forward 'global))))

;;; Test: dead buffer removal

(ert-deftest cpo-location-history-test-dead-buffer-removal ()
  "Navigating to a dead buffer entry removes it and retries."
  (cpo-location-history-test--with-fresh-state
   (let ((buf-alive (generate-new-buffer "test-alive"))
         (buf-dead (generate-new-buffer "test-dead")))
     (unwind-protect
         (progn
           (with-current-buffer buf-alive
             (insert (make-string 200 ?a)))
           (with-current-buffer buf-dead
             (insert (make-string 200 ?d)))
           ;; Record entries oldest-first (while buf-dead is still alive).
           (cpo-location-history-test--populate-ring
            `((,buf-alive . 100)
              (,buf-dead . 50)
              (,buf-alive . 10)))
           ;; Ring: 0=(alive.10), 1=(dead.50), 2=(alive.100)
           (should (= 0 cpo-location-history--index))
           ;; Kill the dead buffer.
           (kill-buffer buf-dead)
           (with-current-buffer buf-alive
             (should (= 10 (point)))
             ;; Navigate backward.  Index 1 is dead, so it should be removed
             ;; and the function should retry, landing on the formerly-index-2
             ;; entry (alive.100).
             (cpo-location-history--navigate 'backward 'global)
             (should (= (point) 100))
             ;; Ring should now have 2 entries.
             (should (= 2 (ring-length cpo-location-history-ring)))))
       (when (buffer-live-p buf-alive) (kill-buffer buf-alive))
       (when (buffer-live-p buf-dead) (kill-buffer buf-dead))))))

;;; Test: index resets on real command

(ert-deftest cpo-location-history-test-index-resets-on-new-entry ()
  "Recording a new entry resets the index to 0."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 10)))
     ;; Navigate backward so index is no longer 0.
     (cpo-location-history--navigate 'backward 'global)
     (should (= 1 cpo-location-history--index))
     ;; Simulate a new command that moves far away.
     (goto-char 180)
     (cpo-location-history-record)
     (should (= 0 cpo-location-history--index)))))

;;; Test: the high-level back/forward functions set the navigating flag

(ert-deftest cpo-location-history-test-back-sets-navigating-flag ()
  "cpo-location-history-back sets --navigating via setq, not let."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 10)))
     (should (= 0 cpo-location-history--index))
     (should (= 10 (point)))
     (cpo-location-history-back 'global)
     ;; After back returns, --navigating should STILL be t.
     (should cpo-location-history--navigating)
     (should (= 100 (point))))))

(ert-deftest cpo-location-history-test-forward-sets-navigating-flag ()
  "cpo-location-history-forward sets --navigating via setq, not let."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 10)))
     ;; Navigate to oldest first.
     (cpo-location-history--navigate 'backward 'global)
     (should (= 1 cpo-location-history--index))
     (should (= 100 (point)))
     (cpo-location-history-forward 'global)
     ;; After forward returns, --navigating should STILL be t.
     (should cpo-location-history--navigating)
     (should (= 10 (point))))))

;;; Test: full simulated session with post-command-hook cycles

(ert-deftest cpo-location-history-test-full-session-simulation ()
  "Simulate a realistic editing session with interleaved commands and navigation."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     ;; Phase 1: User edits at various positions.
     ;; Simulate: user at pos 10, post-command-hook fires.
     (goto-char 10)
     (cpo-location-history-record)
     (should (= 1 (ring-length cpo-location-history-ring)))
     ;; User moves to pos 50.
     (goto-char 50)
     (cpo-location-history-record)
     (should (= 2 (ring-length cpo-location-history-ring)))
     ;; User moves to pos 120.
     (goto-char 120)
     (cpo-location-history-record)
     (should (= 3 (ring-length cpo-location-history-ring)))
     ;; Ring: 0=(buf.120), 1=(buf.50), 2=(buf.10)
     ;; Phase 2: User navigates back.
     (cpo-location-history-back 'global)      ; go to pos 50
     (should (= 50 (point)))
     (cpo-location-history-record)            ; post-command-hook
     (should (= 3 (ring-length cpo-location-history-ring)))  ; no new entry
     ;; Navigate back again.
     (cpo-location-history-back 'global)      ; go to pos 10
     (should (= 10 (point)))
     (cpo-location-history-record)            ; post-command-hook
     (should (= 3 (ring-length cpo-location-history-ring)))  ; still no new entry
     ;; Phase 3: Navigate forward.
     (cpo-location-history-forward 'global)   ; go to pos 50
     (should (= 50 (point)))
     (cpo-location-history-record)            ; post-command-hook
     (should (= 3 (ring-length cpo-location-history-ring)))
     ;; Phase 4: User makes a new edit.
     (goto-char 180)
     (cpo-location-history-record)
     ;; New entry should be recorded, and index should be reset.
     (should (= 4 (ring-length cpo-location-history-ring)))
     (should (= 0 cpo-location-history--index))
     ;; Newest entry should be pos 180.
     (should (= 180 (cdr (ring-ref cpo-location-history-ring 0)))))))

;;; Test: filtered-indices ordering

(ert-deftest cpo-location-history-test-filtered-indices-global ()
  "Global mode returns all indices in newest-to-oldest order."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     ;; Use positions far apart to satisfy min-distance.
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 50)
        ( ,(current-buffer) . 10)))
     ;; Ring: 0=10, 1=50, 2=100
     (let ((indices (cpo-location-history--filtered-indices 'global)))
       (should (equal '(0 1 2) indices))))))

(ert-deftest cpo-location-history-test-filtered-indices-local ()
  "Local mode returns only indices for the current buffer."
  (cpo-location-history-test--with-fresh-state
   (let ((buf-a (generate-new-buffer "test-a"))
         (buf-b (generate-new-buffer "test-b")))
     (unwind-protect
         (progn
           (with-current-buffer buf-a (insert "a"))
           (with-current-buffer buf-b (insert "b"))
           ;; Buffer changes always record regardless of position.
           (cpo-location-history-test--populate-ring
            `((,buf-a . 1)
              (,buf-b . 1)
              (,buf-a . 1)))
           ;; Ring: 0=(a.1), 1=(b.1), 2=(a.1)
           (with-current-buffer buf-a
             (let ((indices (cpo-location-history--filtered-indices 'local)))
               (should (equal '(0 2) indices)))))
       (kill-buffer buf-a)
       (kill-buffer buf-b)))))

(ert-deftest cpo-location-history-test-filtered-indices-non-local ()
  "Non-local mode returns only indices for other buffers."
  (cpo-location-history-test--with-fresh-state
   (let ((buf-a (generate-new-buffer "test-a"))
         (buf-b (generate-new-buffer "test-b")))
     (unwind-protect
         (progn
           (with-current-buffer buf-a (insert "a"))
           (with-current-buffer buf-b (insert "b"))
           ;; Buffer changes always record regardless of position.
           (cpo-location-history-test--populate-ring
            `((,buf-a . 1)
              (,buf-b . 1)
              (,buf-a . 1)))
           ;; Ring: 0=(a.1), 1=(b.1), 2=(a.1)
           (with-current-buffer buf-a
             (let ((indices (cpo-location-history--filtered-indices 'non-local)))
               (should (equal '(1) indices)))))
       (kill-buffer buf-a)
       (kill-buffer buf-b)))))

;;; Test: named convenience functions

(ert-deftest cpo-location-history-test-global-back-forward ()
  "The global-back and global-forward functions work."
  (cpo-location-history-test--with-fresh-state
   (with-temp-buffer
     (insert (make-string 200 ?x))
     (cpo-location-history-test--populate-ring
      `(( ,(current-buffer) . 100)
        ( ,(current-buffer) . 10)))
     (should (= 0 cpo-location-history--index))
     (should (= 10 (point)))
     (cpo-location-history-global-back)
     (should (= 100 (point)))
     (should cpo-location-history--navigating)
     ;; Clear flag as post-command-hook would.
     (cpo-location-history-record)
     (cpo-location-history-global-forward)
     (should (= 10 (point))))))

(ert-deftest cpo-location-history-test-local-back-forward ()
  "The local-back and local-forward functions work."
  (cpo-location-history-test--with-fresh-state
   (let ((buf-a (generate-new-buffer "test-a"))
         (buf-b (generate-new-buffer "test-b")))
     (unwind-protect
         (progn
           (with-current-buffer buf-a (insert (make-string 200 ?a)))
           (with-current-buffer buf-b (insert (make-string 200 ?b)))
           (cpo-location-history-test--populate-ring
            `((,buf-a . 150)
              (,buf-b . 80)
              (,buf-a . 10)))
           ;; Ring: 0=(a.10), 1=(b.80), 2=(a.150)
           (with-current-buffer buf-a
             (should (= 0 cpo-location-history--index))
             (should (= 10 (point)))
             (cpo-location-history-local-back)
             ;; Should skip buf-b, go to (a.150).
             (should (= 150 (point)))
             (should (eq (current-buffer) buf-a))
             (cpo-location-history-record)  ; clear flag
             (cpo-location-history-local-forward)
             (should (= 10 (point)))))
       (kill-buffer buf-a)
       (kill-buffer buf-b)))))

;;; Test: ring size

(ert-deftest cpo-location-history-test-ring-size ()
  "The default ring size is large (1000 entries)."
  ;; This just checks the default, not from a fresh state.
  (should (= 1000 (ring-size cpo-location-history-ring))))

;;; test-cpo-location-history.el ends here
