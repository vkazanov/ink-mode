;;; play-test.el --- tests for ink-play preconditions  -*- lexical-binding: t; -*-

(require 'ert)
(require 'ink-mode)

(ert-deftest ink-play-errors-on-unsaved-buffer ()
  "Calling `ink-play' in a buffer not visiting a file signals user-error."
  (with-temp-buffer
    (ink-mode)
    (should-error (ink-play) :type 'user-error)))

(ert-deftest ink-play-errors-on-missing-compiler ()
  "Calling `ink-play' with an unresolvable compiler signals user-error."
  (with-temp-buffer
    (ink-mode)
    ;; Pretend the buffer visits a file so the first guard passes.
    (setq buffer-file-name "/tmp/test-ink-play.ink")
    (let ((ink-inklecate-command "/nonexistent/inklecate-binary"))
      (should-error (ink-play) :type 'user-error))))

(ert-deftest ink-play-knot-errors-on-unsaved-buffer ()
  "Calling `ink-play-knot' in a buffer not visiting a file signals user-error."
  (with-temp-buffer
    (ink-mode)
    (should-error (ink-play-knot) :type 'user-error)))

(ert-deftest ink-play-knot-errors-on-missing-compiler ()
  "Calling `ink-play-knot' with an unresolvable compiler signals user-error."
  (with-temp-buffer
    (ink-mode)
    (setq buffer-file-name "/tmp/test-ink-play.ink")
    (let ((ink-inklecate-command "/nonexistent/inklecate-binary"))
      (should-error (ink-play-knot) :type 'user-error))))

(ert-deftest ink-play-replay-errors-without-cached-file ()
  "Replaying from *Ink* without a cached file name signals user-error."
  (with-temp-buffer
    (ink-play-mode)
    (setq ink-last-played-file-name nil)
    (should-error (ink-play) :type 'user-error)))

(provide 'play-test)
;;; play-test.el ends here
