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

(ert-deftest git-branch-off-search-test/group-transform-strips-hash-prefix-only ()
  "Transformed, the group function strips exactly the
`git-branch-off-search-group-prefix-len' leading characters (the
duplicated short hash) -- NOT the length of the group title text
itself, which is unrelated commit-date/author text and is not
literally a prefix of the candidate.

This regresses a real display bug found via manual/interactive
verification: with the old (group-title-length-based) stripping, the
group title (\"hash  date  author\", often 25-40 chars) was longer than
the actual redundant prefix (just the 8-char hash), so grouped display
silently chopped real content (the file name and part of the matched
line) off the front of every candidate."
  (let* ((group "abcd1234  2024-01-01  Author With A Long Name")
         (cand (concat "abcd1234" ":file.el:12: the matched line"))
         (cand (propertize cand
                           'git-branch-off-search-group group
                           'git-branch-off-search-group-prefix-len 8)))
    (should (equal (git-branch-off--search-group cand t) ":file.el:12: the matched line"))))

(ert-deftest git-branch-off-search-test/group-transform-against-real-parse-line ()
  "End-to-end: a real candidate from `git-branch-off--search-parse-line'
transforms to exactly its file/line/content portion, with the
duplicated hash (and only the hash) stripped."
  (let* ((full-hash (concat "abcd1234" (make-string 32 ?f)))
         (cache (let ((tbl (make-hash-table :test #'equal)))
                  (puthash full-hash "2024-01-01  Author" tbl)
                  tbl))
         (line (format "%s:file.el:12:the matched line" full-hash))
         (cand (git-branch-off--search-parse-line line cache)))
    ;; `cand' carries a trailing invisible disambiguation character
    ;; (see `git-branch-off--search-tag-candidate'); strip it before
    ;; comparing against the expected visible text.
    (should (equal (substring (git-branch-off--search-group cand t) 0 -1)
                   ":file.el:12: the matched line"))))

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
  "A plain `equal'-based lookup over two candidates that render
identically always resolves to the first; this pins down that
(first-wins) fallback behavior for candidates that are genuinely
indistinguishable, so a caller relying on `git-branch-off--search-lookup'
directly with such input has predictable (if unhelpful) behavior."
  (let* ((first (propertize "dup" 'marker 1))
         (second (propertize "dup" 'marker 2))
         (candidates (list first second)))
    (should (eq (git-branch-off--search-lookup "dup" candidates) first))))

(ert-deftest git-branch-off-search-test/parse-line-disambiguates-short-hash-collisions ()
  "Two distinct commits whose 8-char short hashes collide (a real
possibility in a large repo: 32 bits of hash space, ~50% birthday
collision by ~77k commits) and which touch the same file/line/content
(common for an unchanged line across history, especially with
`git-branch-off-search-all-grep', which intentionally surfaces every
commit touching a blob) would render byte-for-byte identical visible
candidate text. Confirm `git-branch-off--search-parse-line' tags each
candidate with a unique disambiguation suffix so the two full candidate
strings are never `equal', even though their visible portion is
identical -- and that lookup then resolves each one correctly rather
than silently collapsing to whichever appears first."
  (let* ((full-hash-1 (concat "11111111" (make-string 31 ?a) "b"))
         (full-hash-2 (concat "11111111" (make-string 31 ?a) "c"))
         (cache (let ((tbl (make-hash-table :test #'equal)))
                  (puthash full-hash-1 "2024-01-01  Alice" tbl)
                  (puthash full-hash-2 "2024-01-01  Alice" tbl)
                  tbl))
         (line1 (format "%s:same/file.txt:42:identical content" full-hash-1))
         (line2 (format "%s:same/file.txt:42:identical content" full-hash-2))
         (cand1 (git-branch-off--search-parse-line line1 cache))
         (cand2 (git-branch-off--search-parse-line line2 cache)))
    ;; Precondition: both hashes really do share the same 8-char short
    ;; hash, and file/line/content are identical, so the *visible*
    ;; (property-stripped) portion of the two candidates is identical --
    ;; except for the one trailing invisible disambiguation character
    ;; each candidate is tagged with, which we strip off here to check
    ;; the precondition on the part a human would actually see.
    (should (equal (substring-no-properties cand1 0 -1) (substring-no-properties cand2 0 -1)))
    (should-not (equal (get-text-property 0 'git-branch-off-hash cand1)
                        (get-text-property 0 'git-branch-off-hash cand2)))
    ;; The full candidate strings (with disambiguation suffix) must
    ;; nonetheless differ, so `member'/`equal'-based lookup never
    ;; collides.
    (should-not (equal cand1 cand2))
    (let ((candidates (list cand1 cand2)))
      (should (eq (git-branch-off--search-lookup cand1 candidates) cand1))
      (should (eq (git-branch-off--search-lookup cand2 candidates) cand2)))))

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

;;; End-to-end pipeline against a real git repository

(ert-deftest git-branch-off-search-test/full-pipeline-against-real-repo ()
  "Exercise commit-cache, the all-grep builder, and the async collector
together against a real repository, without going through the
minibuffer. Regresses the actual `git' invocations, not just the pure
helpers above."
  :tags '(integration)
  (git-branch-off-test--with-temp-repo
    (git-branch-off-test--commit-file "a.txt" "hello world\n" "add a")
    (git-branch-off-test--commit-file "b.txt" "needle-value here\n" "add b")
    (let* ((cache (git-branch-off--search-commit-cache))
           (collector (git-branch-off--search-make-collector
                       #'git-branch-off--search-all-grep-builder
                       (lambda (lines) (git-branch-off--search-format-lines lines cache))))
           (git-branch-off-search-input-debounce 0.01)
           (git-branch-off-search-input-throttle 0.01)
           (git-branch-off-search-min-input 1))
      (unwind-protect
          (progn
            (funcall collector 'setup)
            (funcall collector "needle-value")
            (sit-for 0.1)
            (funcall collector nil)
            (let ((deadline (+ (float-time) 5)))
              (while (and (null (car (funcall collector nil)))
                          (< (float-time) deadline))
                (sit-for 0.1)))
            (let* ((result (funcall collector nil))
                   (candidates (car result)))
              (should (null (cdr result)))
              (should (= (length candidates) 1))
              (let ((cand (car candidates)))
                (should (equal (get-text-property 0 'git-branch-off-file cand) "b.txt"))
                (should (equal (get-text-property 0 'git-branch-off-line cand) 1))
                (should (git-branch-off--search-lookup cand candidates)))))
        (funcall collector 'cancel)))))

(ert-deftest git-branch-off-search-test/filename-collect-against-real-repo ()
  "The filename-history collector reflects real add/delete events."
  :tags '(integration)
  (git-branch-off-test--with-temp-repo
    (git-branch-off-test--commit-file "keep.txt" "x\n" "add keep")
    (git-branch-off-test--commit-file "gone.txt" "y\n" "add gone")
    (call-process "git" nil nil nil "rm" "-q" "gone.txt")
    (call-process "git" nil nil nil "commit" "-q" "-m" "remove gone")
    (let* ((cache (git-branch-off--search-commit-cache))
           (cands (git-branch-off--search-filename-collect cache))
           (statuses (mapcar (lambda (c) (get-text-property 0 'git-branch-off-status c)) cands))
           (files (mapcar (lambda (c) (get-text-property 0 'git-branch-off-file c)) cands)))
      (should (member "keep.txt" files))
      (should (member "gone.txt" files))
      (should (member "A" statuses))
      (should (member "D" statuses)))))

;;; default-directory correctness across the async collector's lifetime
;;
;; `git-branch-off--read-with-preview' cannot be driven end-to-end here:
;; ERT runs under `--batch', and batch Emacs has no display/frame, so a
;; real `completing-read'/minibuffer session cannot be opened at all --
;; confirmed empirically (a `read-from-minibuffer' call in `--batch'
;; immediately errors trying to read from stdin, since there is no
;; keyboard event source). Manual/interactive verification of the real
;; minibuffer session is tracked separately.
;;
;; What *can* be tested headlessly, and what the correctness of
;; `git-branch-off--read-with-preview' actually reduces to (see the
;; comment on `mb-setup' in git-branch-off-search.el): the collector
;; must spawn its process using whatever `default-directory' is
;; dynamically in effect at the moment something asks it for
;; candidates (i.e. at spawn time), not whatever directory happened to
;; be current when the collector was constructed. This test proves
;; exactly that, using a real temp git repository and a real spawned
;; process, without needing a minibuffer at all.

(ert-deftest git-branch-off-search-test/collector-uses-directory-current-at-spawn-time ()
  "The collector's spawned process runs in whichever directory is
current-buffer's `default-directory' at spawn time, not wherever
`default-directory' was when the collector was constructed. This is
the exact property `git-branch-off--read-with-preview' relies on: it
captures the minibuffer buffer once, then always does
`(with-current-buffer mb-buffer ...)' before touching the collector,
so correctness does not depend on any surrounding `let'-binding of
`default-directory' remaining dynamically live for the whole session."
  :tags '(integration)
  (git-branch-off-test--with-temp-repo
    (git-branch-off-test--commit-file "marker.txt" "x\n" "add marker")
    (let* ((repo-dir (file-truename default-directory))
           (builder (lambda (_input) (list (list "sh" "-c" "pwd"))))
           (collector
            ;; Construct the collector while a directory other than the
            ;; repo -- e.g. /tmp -- is current, deliberately not repo-dir.
            (let ((default-directory (file-truename temporary-file-directory)))
              (git-branch-off--search-make-collector builder (lambda (lines) lines))))
           (git-branch-off-search-input-debounce 0.01)
           (git-branch-off-search-input-throttle 0.01)
           (git-branch-off-search-min-input 1))
      (unwind-protect
          ;; Drive the collector from a dynamic extent where
          ;; `default-directory' is repo-dir -- mirroring what `poll'
          ;; does via `(with-current-buffer mb-buffer ...)'.
          (let ((default-directory repo-dir))
            (funcall collector 'setup)
            (funcall collector "x")
            (sit-for 0.1)
            (funcall collector nil)
            (let ((deadline (+ (float-time) 3)))
              (while (and (null (car (funcall collector nil)))
                          (< (float-time) deadline))
                (sit-for 0.05)))
            (let ((candidates (car (funcall collector nil))))
              (should (= (length candidates) 1))
              (should (equal (file-truename (string-trim (car candidates))) repo-dir))))
        (funcall collector 'cancel)))))

;;; git-branch-off--read-with-preview abort path
;;
;; Found via manual/interactive verification (a real `completing-read'
;; session cannot run in `--batch', but the `quit' handling itself can
;; be exercised headlessly by stubbing out `completing-read' to signal
;; `quit' immediately, so it never actually needs a real minibuffer):
;; the `state' function's contract is always a 2-argument call, `(action
;; cand)', but the abort path only ever called it with 1 argument,
;; `(funcall state 'exit)', which threw `wrong-number-of-arguments' on
;; every real `C-g' abort -- invisible to the whole batch ERT suite
;; since nothing else exercised this specific path.

(ert-deftest git-branch-off-search-test/read-with-preview-calls-state-exit-with-two-args ()
  "On abort (`quit'), STATE must be called as `(funcall state \\='exit nil)',
matching its 2-argument `(action cand)' contract everywhere else."
  (let* ((calls nil)
         (state (lambda (action cand) (push (list action cand) calls))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (signal 'quit nil))))
      (git-branch-off--read-with-preview (list "a" "b") "prompt: " state))
    (should (equal calls '((exit nil))))))

(ert-deftest git-branch-off-search-test/read-with-preview-calls-state-return-on-success ()
  "On a successful selection, STATE is called with \\='return and the
looked-up candidate object."
  (let* ((calls nil)
         (state (lambda (action cand) (push (list action cand) calls))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "b")))
      (git-branch-off--read-with-preview (list "a" "b") "prompt: " state))
    (should (equal calls '((return "b"))))))

;;; git-branch-off--search-original-window
;;
;; Found via manual/interactive verification, the most serious bug
;; caught by this pass: `git-branch-off--search-make-state's \\='preview
;; action calls `(selected-window)' directly, assuming it is the window
;; the user was looking at before the search started. That assumption
;; held for the original Consult-driven version only because Consult's
;; own preview machinery always invoked the state function via
;; `(with-selected-window (consult--original-window) ...)' -- porting
;; the state function unchanged (correctly, since it never called any
;; Consult preview helper) missed that this surrounding wrapper was
;; Consult's responsibility, not the state function's own. Without it,
;; `(selected-window)' inside the state function returned the
;; *minibuffer's own window*, and swapping that window's buffer to the
;; preview buffer reliably crashed Vertico's next redisplay with
;; `(wrong-type-argument number-or-marker-p nil)' inside
;; `vertico--arrange-candidates' (traced to `vertico--window-width'
;; returning nil from `cl-loop ... minimize' over an empty window
;; list). Confirmed via a real -nw Emacs session under tmux, reproduced
;; reliably across every one of several stress runs before the fix, and
;; reproduced zero times across 5 repeat runs after it.

(ert-deftest git-branch-off-search-test/original-window-fallback-outside-minibuffer ()
  "Outside any minibuffer session, falls back to the currently selected
window (mirroring Consult's `consult--original-window', which never
returns nil)."
  (should (eq (git-branch-off--search-original-window) (selected-window))))

(provide 'git-branch-off-search-test)
;;; git-branch-off-search-test.el ends here
