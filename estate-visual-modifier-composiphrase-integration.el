;; -*- lexical-binding: t -*-

;; unstable work-in-progress visual state modifier

;; TODO - A version for tree objects needs even more consideration -- I think the right implementation for tree objects is that the region bounds must either together be selecting one tree OR they must be selecting sibling nodes that share a parent.  I think the sibling use case is actually quite interestsing, much more so than the parent case since selecting the parent is already easy, and I want a good way to select siblings.  So for each end, we must climb the ancestry tree until we find a common node, but then if possible use the bounds of one node below that on each side, and take the max/min of those two regions.

(require 'composiphrase)
(require 'estate-visual-modifier-state)
(require 'cpo-text-object-stuff)

(defvar estate-visual-modifier-composiphrase-movement-verbs '(move DEFAULT)
  "Verbs in composiphrase that indicate movements.")

(defvar cpd--estate-visual-modifier-advice-active nil
  "Non-nil when the composiphrase advice for estate visual modifier is active.")

(defvar cpd--estate-visual-modifier-composiphrase-modification-functions
  (make-hash-table :test 'eq :weakness 'key)
  "Weak hash table tracking functions that modify composiphrase sentences.
Keys are function objects, values are t.")

(defun cpd--composiphrase-execute-advice (orig-fun sentence config)
  "Advice wrapper for composiphrase-execute to trigger region expansion for non-movement commands."
  (let* ((verb-word (seq-find (lambda (word) (eq 'verb (cdr (assq 'word-type word))))
                              sentence))
         (verb-name (if verb-word
                        (composiphrase--get-verb-or-obj-name verb-word)
                      'DEFAULT))
         (is-movement (memq verb-name estate-visual-modifier-composiphrase-movement-verbs)))
    ;; If not a movement command, expand region.  If it is a movement, disable the hooks for deactivating visual mode based on mark state
    (unless is-movement
      (if estate--visual-clamped-object-active
          (estate-visual-clamped-object-expand-region)
        (estate-visual-modifier-expand-region)))
    (funcall orig-fun sentence config)))

(defun cpd--remove-estate-visual-modifier-advice ()
  "Remove the composiphrase advice and this function from the exit hook."
  (when cpd--estate-visual-modifier-advice-active
    (advice-remove 'composiphrase-execute 'cpd--composiphrase-execute-advice)
    (advice-remove 'composiphrase-add-to-current-sentence-with-numeric-handling 'cpd--composiphrase-add-to-sentence-advice)
    (setq cpd--estate-visual-modifier-advice-active nil))
  (remove-hook 'estate-visual-state-exit-hook 'cpd--remove-estate-visual-modifier-advice))

(defun cpd--composiphrase-add-to-sentence-advice (orig-fun exec-after-p &rest words)
  "Advice to track functions created by composiphrase-add-to-current-sentence-with-numeric-handling."
  (let ((result (apply orig-fun exec-after-p words)))
    (when (functionp result)
      (puthash result t cpd--estate-visual-modifier-composiphrase-modification-functions))
    result))
;; This advice has to be loaded before I bind the keys and create all of these lambdas.
(advice-add 'composiphrase-add-to-current-sentence-with-numeric-handling :around 'cpd--composiphrase-add-to-sentence-advice)

(defun cpd--estate-visual-modifier-expansion-predicate (command)
  "Predicate to determine if COMMAND should expand the region.
Returns non-nil for non-composiphrase commands when no composiphrase sentence is active,
but returns nil for composiphrase-execute-current-sentence (which uses the advice instead).

This is a bit of a hack, but I want this to start working and this was the best thing I came up with.  In practice, this is the only way I modify the current sentence.
"
  (and (null composiphrase-current-sentence)
       (not (eq command 'composiphrase-execute-current-sentence))
       (not (gethash command cpd--estate-visual-modifier-composiphrase-modification-functions))))

(setq estate-visual-modifier-expansion-command-predicate
      'cpd--estate-visual-modifier-expansion-predicate)

(defun cpd--activate-estate-visual-modifier-hook ()
  "Activate the advice wrapper and set up cleanup on visual state exit."
  (unless cpd--estate-visual-modifier-advice-active
    (advice-add 'composiphrase-execute :around 'cpd--composiphrase-execute-advice)
    (setq cpd--estate-visual-modifier-advice-active t))
  (add-hook 'estate-visual-state-exit-hook 'cpd--remove-estate-visual-modifier-advice))

(defun cpd--thing-to-modifier (thing)
  "Convert a THING symbol (for use with thing-at-point) into a modifier function.
This version intelligently only expands region bounds that are not already
at the appropriate object boundaries."
  (lambda (region-beg region-end)
    "Modified region function created from thing symbol."
    (let* (;; Check if region bounds are already at thing boundaries
           (beg-at-start-p (save-excursion
                            (goto-char region-beg)
                            (cpo-text-object-stuff--at-thing-beginning-p thing)))
           (end-at-end-p (save-excursion
                          (goto-char region-end)
                          (cpo-text-object-stuff--at-thing-end-p thing)))
           ;; Get thing bounds at each position
           (beg-bounds (save-excursion
                        (goto-char region-beg)
                        (bounds-of-thing-at-point thing)))
           (end-bounds (save-excursion
                        (goto-char region-end)
                        (bounds-of-thing-at-point thing)))
           ;; Calculate new boundaries
           (new-beg (if (and beg-at-start-p beg-bounds)
                        region-beg  ; Already at thing start
                      (if beg-bounds (car beg-bounds) region-beg)))
           (new-end (if (and end-at-end-p end-bounds)
                        region-end  ; Already at thing end
                      (if end-bounds (cdr end-bounds) region-end))))
      (list new-beg new-end))))

(defun cpd-estate-visual-modifier-basic-activate (thing)
  "Activate estate visual modifier with NAME and THING symbol.
THING should be a symbol suitable for use with thing-at-point (e.g., 'word, 'sentence)."
  (estate-visual-state)
  (cpd--activate-estate-visual-modifier-hook)
  (estate-visual-state-activate-modifier
   (list thing (cpd--thing-to-modifier thing))))

(defun cpd-estate-visual-modifier-tree-activate (tree-name tree-modifier)
  "Activate estate visual modifier with NAME and THING symbol.
THING should be a symbol suitable for use with thing-at-point (e.g., 'word, 'sentence)."
  (estate-visual-state)
  (cpd--activate-estate-visual-modifier-hook)
  (estate-visual-state-activate-modifier
   (list tree-name tree-modifier)))


;;; Visual clamped object mode integration with composiphrase.

(require 'estate-visual-clamped-object-state)

(defun cpd--thing-to-clamped-bounds-func (thing)
  "Convert a THING symbol into a bounds function for clamped object mode.
Returns a function of zero arguments that returns (BEG . END) at point."
  (lambda ()
    (bounds-of-thing-at-point thing)))

(defun cpd-estate-visual-clamped-object-basic-activate (thing)
  "Activate visual clamped object mode for a basic THING type.
THING should be a symbol suitable for use with `bounds-of-thing-at-point'."
  (setq estate-visual-clamped-object-command-predicate
        'cpd--estate-visual-modifier-expansion-predicate)
  (cpd--activate-estate-visual-modifier-hook)
  (estate-visual-clamped-object-activate
   thing
   (cpd--thing-to-clamped-bounds-func thing)))

(defun cpd-estate-visual-clamped-object-tree-activate (tree-name tree-bounds-func)
  "Activate visual clamped object mode for a tree-based object.
TREE-NAME is a symbol for display.
TREE-BOUNDS-FUNC is a function of zero arguments returning (BEG . END)."
  (setq estate-visual-clamped-object-command-predicate
        'cpd--estate-visual-modifier-expansion-predicate)
  (cpd--activate-estate-visual-modifier-hook)
  (estate-visual-clamped-object-activate
   tree-name
   tree-bounds-func))

(provide 'estate-visual-modifier-composiphrase-integration)
