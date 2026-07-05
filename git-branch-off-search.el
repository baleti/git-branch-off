;;; git-branch-off-search.el --- Git history search  -*- lexical-binding: t; -*-

;; Four commands sharing a common preview mechanism:
;;   git-branch-off-search-filename-history  — file add/remove events
;;   git-branch-off-search-pickaxe-g         — commits where lines matching regex changed
;;   git-branch-off-search-pickaxe-s         — commits where literal match count changed
;;   git-branch-off-search-all-grep          — git grep across every committed blob
;;
;; This module used to depend on several unstable, double-dash (internal)
;; Consult APIs (`consult--read', `consult--process-collection', etc).
;; That dependency has been removed in favor of a small, purpose-built
;; implementation using only built-in Emacs primitives and `magit'.  See
;; the project history for the rationale: Consult's maintainer has said
;; publicly that packages depending on its internal API "would have to be
;; updated in lock-step with consult," and that surface has already had
;; breaking changes across its own history.

(require 'cl-lib)
(require 'subr-x)

;;; Customization
;;
;; Consult's own async pipeline uses four independently-tunable delays
;; rather than one; conflating them either stutters the UI (redisplaying
;; too often) or feels unresponsive (throttling too aggressively).  We
;; keep the same four knobs, with Consult's documented defaults as our
;; starting point.

(defcustom git-branch-off-search-input-debounce 0.2
  "Seconds of no further input change before a search may (re)start."
  :type 'number :group 'git-branch-off)

(defcustom git-branch-off-search-input-throttle 0.5
  "Minimum seconds between successive search process restarts."
  :type 'number :group 'git-branch-off)

(defcustom git-branch-off-search-min-input 3
  "Minimum input length before an async search process is started."
  :type 'integer :group 'git-branch-off)

(defcustom git-branch-off-search-refresh-delay 0.2
  "Seconds between checks for newly arrived asynchronous candidates."
  :type 'number :group 'git-branch-off)

;;; Faces
;;
;; Locally defined so this file no longer borrows `consult-file',
;; `consult-line-number', and `consult-preview-line'.

(defface git-branch-off-search-file
  '((t :inherit font-lock-string-face))
  "Face for the file name component of a search candidate."
  :group 'git-branch-off)

(defface git-branch-off-search-line-number
  '((t :inherit line-number))
  "Face for the line number component of a search candidate."
  :group 'git-branch-off)

(defface git-branch-off-search-preview-line
  '((t :inherit highlight))
  "Face used to mark the previewed line in a preview buffer."
  :group 'git-branch-off)

;;; History

(defvar git-branch-off--search-history nil
  "Minibuffer history for the async git-grep search commands.")

(defvar git-branch-off--search-filename-history nil
  "Minibuffer history for `git-branch-off-search-filename-history'.")

(defun git-branch-off--search-check-deps ()
  "Signal `user-error' if required packages are not loaded."
  (unless (require 'magit nil t)
    (user-error "Package `magit' is required for git-branch-off search commands")))

(defun git-branch-off--search-commit-cache ()
  "Return a hash table mapping full SHA → \"YYYY-MM-DD  author\" for all commits."
  (let ((tbl (make-hash-table :test #'equal :size 256)))
    (with-temp-buffer
      (call-process "git" nil t nil "log" "--all" "--format=%H\t%as\t%an")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (string-match "^\\([0-9a-f]\\{40\\}\\)\t\\([^\t]*\\)\t\\(.*\\)$" line)
            (puthash (match-string 1 line)
                     (concat (match-string 2 line) "  " (match-string 3 line))
                     tbl)))
        (forward-line 1)))
    tbl))

(defun git-branch-off--search-parse-line (line cache)
  "Parse one `git grep -n' history line into a propertized candidate, or nil.
Expected format: <40-sha>:<file>:<lineno>:<content>"
  (when (string-match
         "^\\([0-9a-f]\\{40\\}\\):\\([^:\n]+\\):\\([0-9]+\\):\\(.*\\)$"
         line)
    (let* ((hash   (match-string 1 line))
           (file   (match-string 2 line))
           (lineno (string-to-number (match-string 3 line)))
           (cont   (match-string 4 line))
           (short  (substring hash 0 8))
           (info   (gethash hash cache ""))
           (cand   (concat (propertize short 'face 'magit-hash)
                           ":" (propertize file 'face 'git-branch-off-search-file)
                           ":" (propertize (number-to-string lineno)
                                           'face 'git-branch-off-search-line-number)
                           ": " cont)))
      (put-text-property 0 1 'git-branch-off-hash  hash   cand)
      (put-text-property 0 1 'git-branch-off-file  file   cand)
      (put-text-property 0 1 'git-branch-off-line  lineno cand)
      (put-text-property 0 1 'git-branch-off-search-group
                         (concat short "  " info) cand)
      (git-branch-off--search-tag-candidate cand))))

(defun git-branch-off--search-format-lines (lines cache)
  "Filter and format a batch of git grep output LINES into candidates using CACHE."
  (delq nil (mapcar (lambda (l) (git-branch-off--search-parse-line l cache)) lines)))

(defun git-branch-off--search-apply-highlights (query &optional cur-beg cur-end case-sensitive)
  "Highlight QUERY matches in current buffer; use `isearch' face at CUR-BEG..CUR-END."
  (let ((case-fold-search (not case-sensitive))
        (bg (or (and (fboundp 'doom-color) (doom-color 'orange))
                (face-background 'lazy-highlight nil t)
                "#af7800")))
    (when (and query (not (string-blank-p query)))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (regexp-quote query) nil t)
          (let* ((mbeg (match-beginning 0))
                 (mend (match-end 0))
                 (current-p (and cur-beg cur-end
                                 (>= mbeg cur-beg) (<= mend cur-end)))
                 (ov (make-overlay mbeg mend)))
            (overlay-put ov 'face (if current-p 'isearch `(:background ,bg :extend nil)))
            (overlay-put ov 'priority (if current-p 4 3))
            (overlay-put ov 'git-branch-off-query-hl t)))))))

;;; Input splitting
;;
;; Consult's `consult--command-split' supports arbitrary quoted flags,
;; escaped dashes, and an explicit `--' end-of-options marker.  None of
;; our four commands ever consume the "flags" half of that split — every
;; call site here only ever used the search-term half and discarded the
;; rest — so a fixed, simple split is sufficient and preferable.

(defun git-branch-off--search-arg (input)
  "Return the search-term portion of INPUT.
Splits at the first whitespace-delimited token that begins with a dash;
everything from there on is discarded, since none of these commands
forward extra flags to `git grep'.  A literal leading dash typed as
part of the search term itself will be misparsed as a flag boundary;
there is no escape syntax for that here (unlike Consult's `\\-'),
since it has never been exercised by these commands."
  (if (string-match "\\(?:\\`\\|[ \t]\\)-" input)
      (string-trim-right (substring input 0 (match-beginning 0)))
    input))

;;; Grouping
;;
;; A group-function under the standard `completion-metadata' protocol
;; (used directly by Vertico and friends) rather than Consult's
;; `consult--prefix-group' convenience wrapper around the same protocol.

(defun git-branch-off--search-group (cand transform)
  "Group function for search candidates.
CAND must carry a `git-branch-off-search-group' text property giving
its group title."
  (if transform
      (substring cand (1+ (length (get-text-property 0 'git-branch-off-search-group cand))))
    (get-text-property 0 'git-branch-off-search-group cand)))

;;; Candidate disambiguation
;;
;; Candidate lookup below is `equal'-based (plain string comparison),
;; and the four candidate-producing functions in this file only ever
;; encode identifying information (commit hash, file, line) as *visible*
;; text, not purely as text properties -- so two distinct candidates
;; that happen to render identical visible text would otherwise resolve
;; to whichever one is `member' finds first.
;;
;; This is not just a theoretical concern: `git-branch-off--search-parse-line'
;; only shows an 8-hex-char short hash (32 bits of space -- a ~50%
;; birthday-collision chance by around 77k distinct commits, which real
;; large repos can reach), and `git-branch-off-search-all-grep'
;; deliberately searches every commit that ever touched a blob, so the
;; same file/line/content recurring across many commits (an unchanged
;; line persisting through history) is the common case, not an edge
;; case. Two different commits whose short hashes happen to collide,
;; both showing the same unchanged file/line/content, would otherwise
;; be visually and programmatically indistinguishable.
;;
;; We fix this exactly the way Consult's own `consult--tofu-char'
;; mechanism does: append a small, invisible, per-candidate
;; disambiguation suffix so that no two candidates from the same
;; collection can ever compare `equal', regardless of what their
;; visible text looks like.

(defconst git-branch-off--search-tofu-char #x100000
  "Base codepoint (Unicode Private Use Area B) for disambiguation suffixes.
Mirrors Consult's own `consult--tofu-char'.")

(defconst git-branch-off--search-tofu-range #xfffe
  "Number of distinct disambiguation codepoints available.")

(defvar git-branch-off--search-tofu-counter 0
  "Monotonic counter backing `git-branch-off--search-tag-candidate'.
Never reset, so uniqueness holds across an entire Emacs session, not
just within one collection.")

(defun git-branch-off--search-tag-candidate (cand)
  "Return CAND with a unique invisible disambiguation suffix appended.
Guarantees CAND can never compare `equal' to any other tagged
candidate, even one with byte-for-byte identical visible text."
  (let ((n (cl-incf git-branch-off--search-tofu-counter)))
    (concat cand
            (propertize (string (+ git-branch-off--search-tofu-char
                                    (mod n git-branch-off--search-tofu-range)))
                        'invisible t))))

;;; Candidate lookup

(defun git-branch-off--search-lookup (selected candidates)
  "Return the element of CANDIDATES `equal' to SELECTED, or nil.
Uses plain `equal' over the full (tagged) candidate string; it is up
to callers to ensure CANDIDATES came from a candidate-producing
function that ran each one through `git-branch-off--search-tag-candidate',
so that two visually-identical candidates never actually collide here."
  (car (member selected candidates)))

;;; Async process collection
;;
;; A from-scratch replacement for `consult--process-collection'.  The
;; gotchas below are drawn from reading Consult's own implementation
;; (`consult--async-process', `consult--async-dynamic',
;; `consult--async-min-input', `consult--async-throttle'):
;;
;;  - Debounce and throttle are different knobs: debounce waits for a
;;    pause in typing; throttle caps how often a process may restart
;;    even during sustained typing-with-pauses.
;;  - A killed process's filter can still fire after `delete-process'
;;    returns, so every spawn is tagged with a generation counter and
;;    the filter/sentinel discard output once their generation is stale.
;;  - `set-process-query-on-exit-flag' is set to nil immediately so a
;;    killed process never blocks Emacs shutdown or prompts.
;;  - Output arrives in arbitrary chunks, not clean lines; a line can be
;;    split across filter calls, so partial output is buffered and only
;;    complete lines are ever handed to TRANSFORM.

(defun git-branch-off--search-buffer-lines (pending chunk)
  "Split PENDING + CHUNK into complete lines and a new pending remainder.
Returns (NEW-PENDING . LINES), where LINES is the list of complete
lines found (in order), and NEW-PENDING is the trailing partial line
(possibly empty) to be prefixed onto the next chunk."
  (let ((buf (concat pending chunk))
        (lines nil)
        (start 0))
    (while (string-match "\n" buf start)
      (push (substring buf start (match-beginning 0)) lines)
      (setq start (match-end 0)))
    (cons (substring buf start) (nreverse lines))))

(defun git-branch-off--search-should-spawn-p
    (current-input spawned-input last-change-time last-start-time now
                    debounce throttle min-input)
  "Return non-nil if a new search process should be (re)started now.
Pure predicate, factored out of the collector for direct unit testing.
CURRENT-INPUT is the latest known input; SPAWNED-INPUT is the input the
most recent process was started for (or nil).  NOW, LAST-CHANGE-TIME,
and LAST-START-TIME are `float-time'-style timestamps (LAST-CHANGE-TIME
/ LAST-START-TIME may be nil, meaning \"never\")."
  (and current-input
       (>= (length current-input) min-input)
       (not (equal current-input spawned-input))
       last-change-time
       (>= (- now last-change-time) debounce)
       (or (null last-start-time)
           (>= (- now last-start-time) throttle))))

(defun git-branch-off--search-make-collector (builder transform)
  "Return a dynamic candidate collector for BUILDER and TRANSFORM.
BUILDER is called with the current input string and must return either
nil (no process should run for this input) or a cons whose car is the
argument list for `make-process's :command.  TRANSFORM is called with
a list of complete raw output lines and must return a list of finished
candidate strings.

The returned closure implements a three-action protocol, mirroring the
shape of Consult's own dynamic-collection helpers:

  \\='setup    one-time initialization; returns nil.
  \\='cancel   stop any live process and release resources; returns nil.
  STRING    record that the input changed to STRING.  Does not itself
            spawn a process — recomputation happens lazily, only when
            a result is actually requested below.
  nil       return the candidates collected so far, first (re)starting
            the async process if the debounce/throttle/min-input
            conditions call for it.

Keeping \"input changed\" and \"actually recompute\" separate means a
burst of keystrokes only invalidates cheaply; the one real
recomputation happens only when something asks for a result."
  (let (proc pending-output candidates error-message
        (generation 0)
        current-input spawned-input
        last-change-time last-start-time)
    (cl-labels
        ((cleanup ()
           (when (process-live-p proc)
             (set-process-filter proc #'ignore)
             (set-process-sentinel proc #'ignore)
             (delete-process proc))
           (setq proc nil))
         (spawn ()
           (cleanup)
           (cl-incf generation)
           (let ((gen generation)
                 (cmd (funcall builder current-input)))
             (setq pending-output "" error-message nil candidates nil)
             (when cmd
               (setq last-start-time (float-time))
               (setq proc
                     (make-process
                      :name "git-branch-off-search"
                      :command (car cmd)
                      :connection-type 'pipe
                      :noquery t
                      :filter
                      (lambda (_proc chunk)
                        (when (= gen generation)
                          (pcase-let ((`(,new-pending . ,lines)
                                       (git-branch-off--search-buffer-lines
                                        pending-output chunk)))
                            (setq pending-output new-pending)
                            (when lines
                              (setq candidates
                                    (append candidates (funcall transform lines)))))))
                      :sentinel
                      (lambda (p _event)
                        (when (= gen generation)
                          (unless (process-live-p p)
                            (let ((status (process-exit-status p)))
                              ;; Exit 1 from `git grep' means "no matches",
                              ;; not an error; anything else (bad pattern,
                              ;; not a repo, missing binary) is surfaced
                              ;; rather than silently looking like a
                              ;; zero-result search.
                              (unless (memq status '(0 1))
                                (setq error-message
                                      (format "git-branch-off search: process exited with status %s"
                                              status)))))))))
               (set-process-query-on-exit-flag proc nil)))))
      (lambda (action)
        (pcase action
          ('setup nil)
          ('cancel (cleanup) nil)
          ((pred stringp)
           (unless (equal action current-input)
             (setq current-input action
                   last-change-time (float-time))))
          ('nil
           (when (git-branch-off--search-should-spawn-p
                  current-input spawned-input last-change-time last-start-time
                  (float-time)
                  git-branch-off-search-input-debounce
                  git-branch-off-search-input-throttle
                  git-branch-off-search-min-input)
             (setq spawned-input current-input)
             (spawn))
           (cons candidates error-message)))))))

;;; Preview-driving read loop
;;
;; A narrow replacement for `consult--read', supporting only what the
;; four commands below actually need: a dynamic or static candidate
;; source, a `:state' function using the existing
;; \\='preview/\\='return/\\='exit protocol, and plain `completing-read'
;; underneath so Vertico (or any other completing-read front-end)
;; continues to work unmodified.
;;
;; One inherent limitation, not specific to this implementation: knowing
;; which candidate is currently *highlighted* (as opposed to literally
;; typed) is a front-end concept that plain `completing-read' has no
;; standard way to query — Consult itself only solves this via its own
;; per-front-end `consult--completion-candidate-hook' and
;; `consult--completion-refresh-hook' (e.g. `consult--vertico-candidate'
;; / `consult--vertico-refresh', which poke at Vertico's own
;; `vertico--candidate' / `vertico--exhibit').  We replicate the same
;; minimal, soft-loaded technique, scoped to Vertico only, since that is
;; the front-end this package's users run; it no-ops harmlessly if
;; Vertico is not loaded, and requires zero changes to Vertico itself.

(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--exhibit "ext:vertico")
(defvar vertico--input)

(defun git-branch-off--search-current-candidate ()
  "Return the candidate currently highlighted in the completion UI.
Falls back to the literal minibuffer text if no supported front-end is
active, in which case preview degrades from
preview-as-you-move-selection to preview-as-you-type."
  (if (bound-and-true-p vertico--input)
      (vertico--candidate)
    (minibuffer-contents-no-properties)))

(defun git-branch-off--search-force-refresh ()
  "Best-effort nudge for the completion UI to redisplay.
Needed because new asynchronous candidates can arrive with no
keystroke to trigger the front-end's own recomputation."
  (when (bound-and-true-p vertico--input)
    (setq vertico--input t)
    (vertico--exhibit)))

(defun git-branch-off--read-with-preview (source prompt state &optional history add-history)
  "Read a candidate via `completing-read', driving STATE for preview.
SOURCE is either a plain list of candidate strings, or a collector
closure created by `git-branch-off--search-make-collector'.  PROMPT is
the minibuffer prompt.  STATE is called with \\='preview and a
candidate (or nil) on each selection change, with \\='return and the
selected candidate on success, and with \\='exit (no candidate) on
abort.  HISTORY is a history variable symbol; ADD-HISTORY, if non-nil,
is made available as the first `M-n' suggestion without being inserted."
  (let* ((collector (and (functionp source) source))
         (static-list (unless collector source))
         (last-candidate 'git-branch-off--unset)
         (last-count -1)
         (refresh-timer nil)
         (mb-buffer nil))
    (cl-labels
        ((current-candidates ()
           (if collector (car (funcall collector nil)) static-list))
         (table (str pred action)
           (if (eq action 'metadata)
               '(metadata (category . git-branch-off-grep)
                          (group-function . git-branch-off--search-group)
                          (display-sort-function . identity)
                          (cycle-sort-function . identity))
             (complete-with-action action (current-candidates) str pred)))
         (poll ()
           (when (buffer-live-p mb-buffer)
             (with-current-buffer mb-buffer
               (when collector
                 (funcall collector (minibuffer-contents-no-properties)))
               (let ((cands (current-candidates)))
                 (unless (= (length cands) last-count)
                   (setq last-count (length cands))
                   (git-branch-off--search-force-refresh))))))
         (preview-tick ()
           (when (and (buffer-live-p mb-buffer) (eq (current-buffer) mb-buffer))
             (let ((cand (git-branch-off--search-current-candidate)))
               (unless (equal cand last-candidate)
                 (setq last-candidate cand)
                 (funcall state 'preview
                          (and cand (git-branch-off--search-lookup cand (current-candidates))))))))
         (mb-setup ()
           (setq mb-buffer (current-buffer))
           (when collector (funcall collector 'setup))
           (add-hook 'post-command-hook #'preview-tick nil t)
           (setq refresh-timer
                 (run-with-timer git-branch-off-search-refresh-delay
                                  git-branch-off-search-refresh-delay
                                  #'poll))))
      (unwind-protect
          (condition-case nil
              (let* ((hist-var (or history 'git-branch-off--search-history))
                     (result (minibuffer-with-setup-hook #'mb-setup
                               (if add-history
                                   (let ((saved (symbol-value hist-var)))
                                     (unwind-protect
                                         (progn
                                           (set hist-var (cons add-history saved))
                                           (completing-read prompt #'table nil t nil hist-var))
                                       (set hist-var saved)))
                                 (completing-read prompt #'table nil t nil hist-var))))
                     (cand (git-branch-off--search-lookup result (current-candidates))))
                (funcall state 'return cand)
                cand)
            (quit (funcall state 'exit) nil))
        (when refresh-timer (cancel-timer refresh-timer))
        (when collector (funcall collector 'cancel))))))

;;; Command builders

(defun git-branch-off--search-all-grep-builder (input)
  "Command builder: git grep across every committed blob for INPUT."
  (let ((arg (git-branch-off--search-arg input)))
    (unless (string-blank-p arg)
      (cons (list "sh" "-c"
                  (format "git --no-pager grep -In -e %s \
$(git rev-list --all 2>/dev/null) 2>/dev/null"
                          (shell-quote-argument arg)))
            nil))))

(defun git-branch-off--search-pickaxe-s-builder (input)
  "Command builder: git grep limited to commits where the literal
count of INPUT changed."
  (let ((arg (git-branch-off--search-arg input)))
    (unless (string-blank-p arg)
      (let ((q (shell-quote-argument arg)))
        (cons (list "sh" "-c"
                    (format "git --no-pager grep -In -e %s \
$(git log --all -S%s --format=%%H 2>/dev/null | head -n 500) 2>/dev/null"
                            q q))
              nil)))))

(defun git-branch-off--search-pickaxe-g-builder (input)
  "Command builder: git grep limited to commits where a line
matching INPUT changed."
  (let ((arg (git-branch-off--search-arg input)))
    (unless (string-blank-p arg)
      (let ((q (shell-quote-argument arg)))
        (cons (list "sh" "-c"
                    (format "git --no-pager grep -In -e %s \
$(git log --all -G%s --format=%%H 2>/dev/null | head -n 500) 2>/dev/null"
                            q q))
              nil)))))

(defun git-branch-off--search-filename-collect (cache)
  "Return propertized candidates for file add/remove events from git history."
  (let (result cur-hash)
    (with-temp-buffer
      (call-process "git" nil t nil "log" "--all"
                    "--diff-filter=AD" "--name-status"
                    "--format=COMMIT\t%H\t%as\t%an")
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "^COMMIT\t\\([0-9a-f]\\{40\\}\\)" line)
            (setq cur-hash (match-string 1 line)))
           ((and cur-hash (string-match "^\\([AD]\\)\t\\(.*\\)$" line))
            (let* ((status (match-string 1 line))
                   (file   (match-string 2 line))
                   (short  (substring cur-hash 0 8))
                   (info   (gethash cur-hash cache ""))
                   (label  (if (equal status "A") "Added" "Deleted"))
                   (cand   (concat (propertize short 'face 'magit-hash)
                                   ":" (propertize file 'face 'git-branch-off-search-file)
                                   ":1: [" label "] " file)))
              (put-text-property 0 1 'git-branch-off-hash   cur-hash cand)
              (put-text-property 0 1 'git-branch-off-file   file     cand)
              (put-text-property 0 1 'git-branch-off-line   1        cand)
              (put-text-property 0 1 'git-branch-off-status status   cand)
              (put-text-property 0 1 'git-branch-off-search-group
                                 (concat short "  " info) cand)
              (push (git-branch-off--search-tag-candidate cand) result)))))
        (forward-line 1)))
    (nreverse result)))

;;; Preview state machine
;;
;; Unchanged in spirit from the Consult-based version: this never called
;; any Consult preview helper, only conformed to the `:state' calling
;; convention `consult--read' expected.  It is now driven by
;; `git-branch-off--read-with-preview' instead.

(defun git-branch-off--search-make-state (on-return &optional highlight-query case-sensitive)
  "Return a state function; ON-RETURN is called with the selected candidate."
  (let ((pbuf (get-buffer-create " *git-branch-off-preview*"))
        restore-fn
        line-ov)
    (lambda (action cand)
      (pcase action
        ('preview
         (when restore-fn (funcall restore-fn) (setq restore-fn nil))
         (when cand
           (let* ((hash   (get-text-property 0 'git-branch-off-hash cand))
                  (file   (get-text-property 0 'git-branch-off-file cand))
                  (line   (get-text-property 0 'git-branch-off-line cand))
                  (status (get-text-property 0 'git-branch-off-status cand))
                  (rev    (if (equal status "D")
                              (concat hash "^")
                            hash))
                  (win    (selected-window)))
             (when (and hash file line)
               (with-current-buffer pbuf
                 (let* ((inhibit-read-only t)
                        (ext (downcase (or (file-name-extension file) "")))
                        (binary-p (member ext '("pdf" "png" "jpg" "jpeg" "gif"
                                                "bmp" "webp" "ico" "tiff" "svg"
                                                "zip" "gz" "tar" "bz2" "xz"
                                                "jar" "class" "so" "dylib"
                                                "exe" "dll" "o" "elc" "pyc"))))
                   (when (overlayp line-ov) (delete-overlay line-ov) (setq line-ov nil))
                   (erase-buffer)
                   (when (and (not binary-p)
                              (= 0 (call-process "git" nil t nil "show"
                                                 (format "%s:%s" rev file))))
                     (pcase-let ((`(,fl-defs . ,syn-tbl)
                                  (condition-case nil
                                      (with-temp-buffer
                                        (let ((buffer-file-name file))
                                          (delay-mode-hooks (set-auto-mode))
                                          (setq delayed-mode-hooks nil))
                                        (cons font-lock-defaults (syntax-table)))
                                    (error (cons nil nil)))))
                       (when fl-defs
                         (set-syntax-table syn-tbl)
                         (setq-local font-lock-defaults fl-defs)
                         (font-lock-mode 1)
                         (font-lock-ensure)))
                     (goto-char (point-min))
                     (forward-line (1- line))
                     (if highlight-query
                         (let* ((cur-beg (line-beginning-position))
                                (cur-end (line-end-position))
                                (mbquery (condition-case nil
                                             (with-current-buffer
                                                 (window-buffer (active-minibuffer-window))
                                               (minibuffer-contents-no-properties))
                                           (error nil)))
                                (arg (git-branch-off--search-arg (or mbquery ""))))
                           (git-branch-off--search-apply-highlights
                            arg cur-beg cur-end case-sensitive))
                       (setq line-ov
                             (make-overlay (line-beginning-position)
                                           (min (1+ (line-end-position)) (point-max))))
                       (overlay-put line-ov 'face 'git-branch-off-search-preview-line)
                       (overlay-put line-ov 'priority 2))
                     (setq buffer-read-only t))))
               (let ((prev-buf (window-buffer win))
                     (prev-pt  (window-point win)))
                 (setq restore-fn
                       (lambda ()
                         (when (window-live-p win)
                           (set-window-buffer win prev-buf)
                           (set-window-point win prev-pt))))
                 (set-window-buffer win pbuf)
                 (set-window-point win (with-current-buffer pbuf (point))))))))
        ('return
         (when (overlayp line-ov) (delete-overlay line-ov) (setq line-ov nil))
         (when restore-fn (funcall restore-fn) (setq restore-fn nil))
         (when (buffer-live-p pbuf) (kill-buffer pbuf))
         (when cand (funcall on-return cand)))
        ('exit
         (when (overlayp line-ov) (delete-overlay line-ov) (setq line-ov nil))
         (when restore-fn (funcall restore-fn) (setq restore-fn nil))
         (when (buffer-live-p pbuf) (kill-buffer pbuf)))))))

(defun git-branch-off--search-state (&optional highlight-query case-sensitive)
  "State: preview git blobs; open blob in magit-find-file on return."
  (git-branch-off--search-make-state
   (lambda (cand)
     (let* ((hash (get-text-property 0 'git-branch-off-hash cand))
            (file (get-text-property 0 'git-branch-off-file cand))
            (line (get-text-property 0 'git-branch-off-line cand)))
       (when (and hash file line)
         (magit-find-file hash file)
         (goto-char (point-min))
         (forward-line (1- line))
         (recenter))))
   highlight-query case-sensitive))

(defun git-branch-off--search-filename-state ()
  "State: preview git blobs; open commit via magit-show-commit on return."
  (git-branch-off--search-make-state
   (lambda (cand)
     (when-let ((hash (get-text-property 0 'git-branch-off-hash cand)))
       (magit-show-commit hash)))))

;;; Commands

(defun git-branch-off--search-grep-read (builder prompt &optional highlight-query case-sensitive)
  "Run an async git grep search session using BUILDER and PROMPT."
  (git-branch-off--search-check-deps)
  (let* ((top   (or (magit-toplevel) (user-error "Not in a git repository")))
         (default-directory top)
         (cache (git-branch-off--search-commit-cache))
         (collector (git-branch-off--search-make-collector
                     builder
                     (lambda (lines) (git-branch-off--search-format-lines lines cache)))))
    (git-branch-off--read-with-preview
     collector prompt
     (git-branch-off--search-state highlight-query case-sensitive)
     'git-branch-off--search-history
     (thing-at-point 'symbol))))

(defun git-branch-off-search-all-grep ()
  "Search ALL committed blobs for a pattern (may show duplicates across commits)."
  (interactive)
  (git-branch-off--search-grep-read #'git-branch-off--search-all-grep-builder
                                     "All-commits grep: " t))

(defun git-branch-off-search-pickaxe-s ()
  "Pickaxe -S: search commits where the literal count of a string changed."
  (interactive)
  (git-branch-off--search-grep-read #'git-branch-off--search-pickaxe-s-builder
                                     "Pickaxe -S (count changed): " t t))

(defun git-branch-off-search-pickaxe-g ()
  "Pickaxe -G: search commits where a line matching a regex changed."
  (interactive)
  (git-branch-off--search-grep-read #'git-branch-off--search-pickaxe-g-builder
                                     "Pickaxe -G (regex changed): " t t))

(defun git-branch-off-search-filename-history ()
  "Show all commits where a file was Added or Deleted; filter by filename."
  (interactive)
  (git-branch-off--search-check-deps)
  (let* ((top   (or (magit-toplevel) (user-error "Not in a git repository")))
         (default-directory top)
         (cache (git-branch-off--search-commit-cache))
         (cands (git-branch-off--search-filename-collect cache)))
    (if (null cands)
        (message "No file add/remove events found in git history")
      (git-branch-off--read-with-preview
       cands "File history (add/remove): "
       (git-branch-off--search-filename-state)
       'git-branch-off--search-filename-history))))

(provide 'git-branch-off-search)
;;; git-branch-off-search.el ends here
