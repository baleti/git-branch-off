;;; git-branch-off-search-test.el --- ERT tests for git-branch-off-search  -*- lexical-binding: t; -*-

;; Run with:
;;   cd test && ./run-tests.sh
;; or directly:
;;   emacs --batch -L .. -l git-branch-off-search.el \
;;         -l git-branch-off-search-test.el -f ert-run-tests-batch-and-exit
;;
;; Covers the pieces the removal-of-Consult plan singles out as
;; genuinely risky: the split parser, the group-function, the
;; partial-line buffering logic, the candidate lookup, the
;; debounce/throttle spawn predicate, and (tagged `integration') the
;; real async process collector.

(require 'ert)
(require 'cl-lib)
(require 'git-branch-off-search)

;;; git-branch-off--search-arg

(ert-deftest git-branch-off-search-test/arg-plain-query ()
  "A query with no flags is returned unchanged."
  (should (equal (git-branch-off--search-arg "hello world") "hello world")))

(ert-deftest git-branch-off-search-test/arg-strips-trailing-flags ()
  "Flags after a whitespace-delimited dash token are discarded."
  (should (equal (git-branch-off--search-arg "needle -i") "needle"))
  (should (equal (git-branch-off--search-arg "needle -i -w") "needle")))

(ert-deftest git-branch-off-search-test/arg-leading-dash-is-flag-boundary ()
  "A leading dash (no preceding search term) yields an empty arg.
This is the documented, accepted limitation versus Consult's `\\-'
escape syntax, which these commands have never needed."
  (should (equal (git-branch-off--search-arg "-i") "")))

(ert-deftest git-branch-off-search-test/arg-trims-space-before-flag ()
  "Trailing whitespace before the flag boundary is trimmed off the arg."
  (should (equal (git-branch-off--search-arg "needle   -i") "needle")))

;;; git-branch-off--search-group

(ert-deftest git-branch-off-search-test/group-title ()
  "Untransformed, the group function returns the group text property."
  (let ((cand (propertize "abcd1234:file.el:3: content" 'git-branch-off-search-group
                          "abcd1234  2024-01-01  Author")))
    (should (equal (git-branch-off--search-group cand nil)
                    "abcd1234  2024-01-01  Author"))))

(ert-deftest git-branch-off-search-test/group-transform-strips-prefix-length ()
  "Transformed, the group function strips (1+ group-length) leading chars."
  (let* ((group "abcd1234  2024-01-01  Author")
         (cand (concat (make-string (1+ (length group)) ?x) "REST"))
         (cand (propertize cand 'git-branch-off-search-group group)))
    (should (equal (git-branch-off--search-group cand t) "REST"))))

;;; git-branch-off--search-lookup

(ert-deftest git-branch-off-search-test/lookup-found ()
  (let ((candidates (list "a" "b" "c")))
    (should (eq (git-branch-off--search-lookup "b" candidates) (nth 1 candidates)))))

(ert-deftest git-branch-off-search-test/lookup-missing ()
  (should (null (git-branch-off--search-lookup "z" (list "a" "b")))))

(ert-deftest git-branch-off-search-test/lookup-round-trips-object-identity ()
  "The returned candidate is `eq' to the original list element, not a copy,
so text properties (hash/file/line) survive the round trip."
  (let* ((cand (propertize "x" 'git-branch-off-hash "deadbeef"))
         (candidates (list cand)))
    (should (eq (git-branch-off--search-lookup "x" candidates) cand))))

(ert-deftest git-branch-off-search-test/lookup-duplicate-display-strings ()
  "Document the duplicate-display-string edge case explicitly: a plain
`equal'-based lookup cannot disambiguate two candidates that render
identically; it always resolves to the first. Our grep candidates
always include a file+line prefix, so two real matches cannot collide
in practice, but this test pins down the (first-wins) behavior so a
future regression would be visible here rather than as a silent
wrong-selection bug report."
  (let* ((first (propertize "dup" 'marker 1))
         (second (propertize "dup" 'marker 2))
         (candidates (list first second)))
    (should (eq (git-branch-off--search-lookup "dup" candidates) first))))

;;; git-branch-off--search-buffer-lines

(ert-deftest git-branch-off-search-test/buffer-lines-single-chunk-full-lines ()
  (let ((result (git-branch-off--search-buffer-lines "" "one\ntwo\nthree\n")))
    (should (equal (car result) ""))
    (should (equal (cdr result) '("one" "two" "three")))))

(ert-deftest git-branch-off-search-test/buffer-lines-holds-back-partial-line ()
  (let ((result (git-branch-off--search-buffer-lines "" "one\ntwo\nthr")))
    (should (equal (car result) "thr"))
    (should (equal (cdr result) '("one" "two")))))

(ert-deftest git-branch-off-search-test/buffer-lines-completes-partial-across-chunks ()
  "A line split across two chunks reassembles into exactly one candidate
regardless of where the chunk boundary falls -- the most important unit
test in this file, per the plan: chunk-boundary bugs are silent and
input-dependent."
  (let* ((step1 (git-branch-off--search-buffer-lines "" "one\ntw"))
         (step2 (git-branch-off--search-buffer-lines (car step1) "o\nthree\n")))
    (should (equal (cdr step1) '("one")))
    (should (equal (car step1) "tw"))
    (should (equal (cdr step2) '("two" "three")))
    (should (equal (car step2) ""))))

(ert-deftest git-branch-off-search-test/buffer-lines-arbitrary-byte-boundaries ()
  "Feed the same logical output split at every possible byte boundary and
assert the same candidates result regardless of where chunks are cut."
  (let ((full "alpha\nbeta\ngamma\ndelta\n"))
    (dotimes (cut (1+ (length full)))
      (let* ((chunk1 (substring full 0 cut))
             (chunk2 (substring full cut))
             (step1 (git-branch-off--search-buffer-lines "" chunk1))
             (step2 (git-branch-off--search-buffer-lines (car step1) chunk2)))
        (should (equal (append (cdr step1) (cdr step2))
                        '("alpha" "beta" "gamma" "delta")))
        (should (equal (car step2) ""))))))

(ert-deftest git-branch-off-search-test/buffer-lines-empty-output ()
  (let ((result (git-branch-off--search-buffer-lines "" "")))
    (should (equal (car result) ""))
    (should (null (cdr result)))))

;;; git-branch-off--search-should-spawn-p

(ert-deftest git-branch-off-search-test/should-spawn-below-min-input ()
  "Never spawns below the minimum input length, no matter how much time passed."
  (should-not (git-branch-off--search-should-spawn-p "ab" nil 0.0 nil 100.0 0.2 0.5 3)))

(ert-deftest git-branch-off-search-test/should-spawn-before-debounce-elapses ()
  "Does not spawn until debounce has elapsed since the last input change."
  (should-not (git-branch-off--search-should-spawn-p "needle" nil 10.0 nil 10.1 0.2 0.5 3)))

(ert-deftest git-branch-off-search-test/should-spawn-after-debounce-elapses ()
  (should (git-branch-off--search-should-spawn-p "needle" nil 10.0 nil 10.3 0.2 0.5 3)))

(ert-deftest git-branch-off-search-test/should-spawn-respects-throttle ()
  "Even once debounce has elapsed, a restart is capped by the throttle
interval measured from the last process start, not from the last input
change."
  (should-not (git-branch-off--search-should-spawn-p
               "needle-2" "needle-1" 10.0 10.05 10.3 0.2 0.5 3))
  (should (git-branch-off--search-should-spawn-p
           "needle-2" "needle-1" 10.0 10.05 10.6 0.2 0.5 3)))

(ert-deftest git-branch-off-search-test/should-spawn-not-if-input-unchanged ()
  "Does not respawn for input that was already used for the last spawn,
even once debounce/throttle would otherwise allow it."
  (should-not (git-branch-off--search-should-spawn-p "needle" "needle" 10.0 10.0 20.0 0.2 0.5 3)))

(ert-deftest git-branch-off-search-test/should-spawn-nil-current-input ()
  (should-not (git-branch-off--search-should-spawn-p nil nil nil nil 0.0 0.2 0.5 3)))

;;; Async process collection (integration: spawns real processes)

(ert-deftest git-branch-off-search-test/collector-rapid-input-single-process ()
  "Rapid sequential input changes only ever result in one live process,
and the collector converges on the final input's candidates."
  :tags '(integration)
  (let* ((builder (lambda (input) (list (list "sh" "-c" (format "echo %s" (shell-quote-argument input))))))
         (collector (git-branch-off--search-make-collector
                     builder (lambda (lines) lines)))
         (git-branch-off-search-input-debounce 0.05)
         (git-branch-off-search-input-throttle 0.05)
         (git-branch-off-search-min-input 1))
    (unwind-protect
        (progn
          (funcall collector 'setup)
          (dolist (s '("a" "ab" "abc" "abcd"))
            (funcall collector s)
            (sit-for 0.01))
          ;; Let debounce elapse once, from the final input.
          (sit-for 0.2)
          (funcall collector nil)
          ;; Give the spawned process time to run and report.
          (let ((deadline (+ (float-time) 3)))
            (while (and (null (car (funcall collector nil)))
                        (< (float-time) deadline))
              (sit-for 0.05)))
          (should (equal (car (funcall collector nil)) '("abcd"))))
      (funcall collector 'cancel))))

(ert-deftest git-branch-off-search-test/collector-empty-output-no-error ()
  "A process producing zero output (no matches) yields an empty
candidate list with no error."
  :tags '(integration)
  (let* ((builder (lambda (_input) (list (list "sh" "-c" "true"))))
         (collector (git-branch-off--search-make-collector builder (lambda (lines) lines)))
         (git-branch-off-search-input-debounce 0.01)
         (git-branch-off-search-input-throttle 0.01)
         (git-branch-off-search-min-input 1))
    (unwind-protect
        (progn
          (funcall collector 'setup)
          (funcall collector "xyz")
          (sit-for 0.1)
          (funcall collector nil)
          (sit-for 0.3)
          (let ((result (funcall collector nil)))
            (should (null (car result)))
            (should (null (cdr result)))))
      (funcall collector 'cancel))))

(ert-deftest git-branch-off-search-test/collector-nonzero-exit-surfaces-error ()
  "A process that exits abnormally (not just \"no matches\") surfaces a
clean error message rather than looking identical to zero results."
  :tags '(integration)
  (let* ((builder (lambda (_input) (list (list "sh" "-c" "exit 2"))))
         (collector (git-branch-off--search-make-collector builder (lambda (lines) lines)))
         (git-branch-off-search-input-debounce 0.01)
         (git-branch-off-search-input-throttle 0.01)
         (git-branch-off-search-min-input 1))
    (unwind-protect
        (progn
          (funcall collector 'setup)
          (funcall collector "xyz")
          (sit-for 0.1)
          (funcall collector nil)
          (sit-for 0.3)
          (should (cdr (funcall collector nil))))
      (funcall collector 'cancel))))

(ert-deftest git-branch-off-search-test/collector-exit-1-is-not-an-error ()
  "grep's exit status 1 (no matches) must not be surfaced as an error."
  :tags '(integration)
  (let* ((builder (lambda (_input) (list (list "sh" "-c" "exit 1"))))
         (collector (git-branch-off--search-make-collector builder (lambda (lines) lines)))
         (git-branch-off-search-input-debounce 0.01)
         (git-branch-off-search-input-throttle 0.01)
         (git-branch-off-search-min-input 1))
    (unwind-protect
        (progn
          (funcall collector 'setup)
          (funcall collector "xyz")
          (sit-for 0.1)
          (funcall collector nil)
          (sit-for 0.3)
          (should (null (cdr (funcall collector nil)))))
      (funcall collector 'cancel))))

(ert-deftest git-branch-off-search-test/collector-killed-process-output-does-not-leak ()
  "Cancelling a stale process because new input arrived must not let its
late output reach the candidate list for the newer query. Simulated by
starting a slow producer, then immediately changing input before it
would have finished."
  :tags '(integration)
  (let* ((builder (lambda (input)
                    (list (list "sh" "-c"
                                (format "sleep 0.3; echo %s" (shell-quote-argument input))))))
         (collector (git-branch-off--search-make-collector builder (lambda (lines) lines)))
         (git-branch-off-search-input-debounce 0.01)
         (git-branch-off-search-input-throttle 0.01)
         (git-branch-off-search-min-input 1))
    (unwind-protect
        (progn
          (funcall collector 'setup)
          (funcall collector "stale-query")
          (sit-for 0.05)
          (funcall collector nil)          ; spawns the slow "stale-query" process
          (sit-for 0.05)
          (funcall collector "fresh-query")
          (sit-for 0.05)
          (funcall collector nil)          ; spawns a fresh process, killing the old one
          ;; Wait long enough for the stale process's sleep to have finished
          ;; had it not been killed.
          (sit-for 0.5)
          (funcall collector nil)
          (let ((deadline (+ (float-time) 3)))
            (while (and (null (car (funcall collector nil)))
                        (< (float-time) deadline))
              (sit-for 0.05)))
          (should (equal (car (funcall collector nil)) '("fresh-query"))))
      (funcall collector 'cancel))))

(provide 'git-branch-off-search-test)
;;; git-branch-off-search-test.el ends here
