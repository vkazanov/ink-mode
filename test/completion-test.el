;;; completion-test.el --- tests for ink-completion-at-point  -*- lexical-binding: t; -*-

(require 'ert)
(require 'ink-mode)

(defconst ink-test--capf-buffer
  "== garden ==
= shed
- (label) Some text
-> garden.shed
<- garden.shed
-> shed
->target
->
Some plain text
"

  ;; NOTE: the bare \"->\" line in this fixture has no trailing space; the
  ;; empty-target test positions point right after the arrow.
)

(defmacro ink-test--with-capf-buffer (point-marker &rest body)
  "Set up a temp ink buffer with point at POINT-MARKER, then run BODY.
POINT-MARKER is a string; point is placed at its first occurrence."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ink-test--capf-buffer)
     (ink-mode)
     (goto-char (point-min))
     (search-forward ,point-marker)
     ,@body))

(ert-deftest ink-capf-returns-bounds-when-point-inside-target ()
  "CAPF returns completion bounds when point is inside target text."
  (ink-test--with-capf-buffer "garden.sh"
    (let ((start-point (point))
          (result (ink-completion-at-point)))
      (should result)
      (should (= (nth 0 result) (- (point) (length "garden.sh"))))
      (should (= (nth 1 result) (+ (point) (length "ed"))))
      (should (= (point) start-point)))))

(ert-deftest ink-capf-returns-bounds-for-reverse-divert ()
  "CAPF returns completion bounds when point is inside reverse divert target."
  (ink-test--with-capf-buffer "<- garden.sh"
    (let ((start-point (point))
          (result (ink-completion-at-point)))
      (should result)
      (should (= (nth 0 result) (- (point) (length "garden.sh"))))
      (should (= (nth 1 result) (+ (point) (length "ed"))))
      (should (= (point) start-point)))))

(ert-deftest ink-capf-returns-bounds-when-target-has-no-space-after-arrow ()
  "CAPF returns bounds when divert target starts immediately after arrow."
  (ink-test--with-capf-buffer "->target"
    ;; Point is now after \"->target\"; back up to start of \"target\".
    (goto-char (- (point) (length "target")))
    (let ((start-point (point))
          (result (ink-completion-at-point)))
      (should result)
      (should (= (nth 0 result) start-point))
      (should (= (nth 1 result) (+ start-point (length "target"))))
      (should (= (point) start-point)))))

(ert-deftest ink-capf-returns-bounds-when-point-at-target-start ()
  "CAPF returns bounds when point is right at the beginning of target."
  (ink-test--with-capf-buffer "-> shed"
    ;; Point is now after "shed" on the "-> shed" line; back up to start of "shed"
    (goto-char (- (point) (length "shed")))
    (let ((start-point (point))
          (result (ink-completion-at-point)))
      (should result)
      (should (= (nth 0 result) start-point))
      (should (= (point) start-point)))))

(ert-deftest ink-capf-returns-bounds-at-empty-target ()
  "CAPF returns zero-width bounds when divert has no target yet."
  (ink-test--with-capf-buffer "->\n"
    ;; Point is right after "->"; that is where the empty target sits.
    (goto-char (- (point) 1))
    (let ((start-point (point))
          (result (ink-completion-at-point)))
      (should result)
      (should (= (nth 0 result) (nth 1 result)))
      (should (= (point) start-point)))))

(ert-deftest ink-capf-returns-nil-when-point-in-gap ()
  "CAPF returns nil when point is in whitespace between arrow and target."
  (ink-test--with-capf-buffer "-> garden.shed"
    ;; Move point into the space between -> and garden
    (goto-char (- (point) (length " garden.shed")))
    (let ((start-point (point))
          (result (ink-completion-at-point)))
      (should-not result)
      (should (= (point) start-point)))))

(ert-deftest ink-capf-returns-nil-when-point-on-arrow ()
  "CAPF returns nil when point is on the arrow itself."
  (ink-test--with-capf-buffer "-> garden.shed"
    (goto-char (- (point) (length "> garden.shed")))
    (let ((start-point (point))
          (result (ink-completion-at-point)))
      (should-not result)
      (should (= (point) start-point)))))

(ert-deftest ink-capf-returns-nil-without-divert ()
  "CAPF returns nil on plain text with no divert."
  (ink-test--with-capf-buffer "plain text"
    (let ((start-point (point))
          (result (ink-completion-at-point)))
      (should-not result)
      (should (= (point) start-point)))))

(provide 'completion-test)
;;; completion-test.el ends here
