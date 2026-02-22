;;; comint-filter-test.el --- tests for ink-play output filtering  -*- lexical-binding: t; -*-

(require 'ert)
(require 'ink-mode)

;;; ink-filter-output-line

(ert-deftest ink-filter-line-passes-through-when-not-filtering ()
  "When filtering is off, lines are returned with a trailing newline."
  (let ((ink-comint-do-filter nil))
    (should (equal (ink-filter-output-line "hello") "hello\n"))
    (should (equal (ink-filter-output-line "") "\n"))))

(ert-deftest ink-filter-line-keeps-error-lines ()
  "When filtering, ERROR/WARNING/TODO lines are kept."
  (let ((ink-comint-do-filter t))
    (should (equal (ink-filter-output-line "ERROR: 'test.ink' line 3: bad")
                   "ERROR: 'test.ink' line 3: bad\n"))
    (should (equal (ink-filter-output-line "WARNING: 'test.ink' line 5: hm")
                   "WARNING: 'test.ink' line 5: hm\n"))
    (should (equal (ink-filter-output-line "TODO: 'test.ink' line 7: fix")
                   "TODO: 'test.ink' line 7: fix\n"))))

(ert-deftest ink-filter-line-drops-non-matching-lines ()
  "When filtering, ordinary lines are discarded."
  (let ((ink-comint-do-filter t))
    (should (equal (ink-filter-output-line "Some story text") ""))
    (should (equal (ink-filter-output-line "1: Choice one") ""))
    (should (equal (ink-filter-output-line "") ""))))

(ert-deftest ink-filter-line-consumes-prompt-and-disables-filter ()
  "When filtering, the ?> prompt line disables the filter and returns content."
  (let ((ink-comint-do-filter t))
    (should (equal (ink-filter-output-line "?> What now") "What now\n"))
    (should-not ink-comint-do-filter)))

(ert-deftest ink-filter-line-handles-bare-prompt-and-inline-marker ()
  "Bare prompt is handled safely; inline ?> marker does not disable filtering."
  (let ((ink-comint-do-filter t))
    (should (equal (ink-filter-output-line "?>") "\n"))
    (should-not ink-comint-do-filter))
  (let ((ink-comint-do-filter t))
    (should (equal (ink-filter-output-line "Story ?> marker") ""))
    (should ink-comint-do-filter)))

;;; ink-comint-filter-output

(ert-deftest ink-comint-filter-output-passes-through-when-not-filtering ()
  "When filtering is off, output is returned unchanged."
  (let ((ink-comint-do-filter nil))
    (should (equal (ink-comint-filter-output "line one\nline two\n")
                   "line one\nline two\n"))))

(ert-deftest ink-comint-filter-output-filters-per-line-when-active ()
  "When filtering is on, each line is processed individually."
  (let ((ink-comint-do-filter t))
    (should (equal (ink-comint-filter-output
                    "Story intro\nERROR: 'f' line 1: oops\n?> Choose")
                   "ERROR: 'f' line 1: oops\nChoose\n"))
    ;; Filter should now be disabled after the ?> prompt.
    (should-not ink-comint-do-filter)))

(provide 'comint-filter-test)
;;; comint-filter-test.el ends here
