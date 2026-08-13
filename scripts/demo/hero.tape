# The README recording. Driven by scripts/demo/record, which substitutes
# @GITTER@ / @REPO@ / @HOME@ / @OUT@ and runs this through vhs.
#
# The story SPEC.md pitches, in order: shape a commit, step out to a shell
# when you need real git, then review the stack. It ends on the review flow
# because that is the part no other git TUI does well.
#
# Timing is CLOSED LOOP. Panes are filled by shelling out to git
# asynchronously, so a fixed Sleep is a guess that is either flaky or slow.
# `Wait+Screen /regex/` blocks until the text actually renders, which means
# the recording runs at the speed the app responds and cannot capture a
# half-drawn pane. Sleeps that remain are deliberate holds for the viewer,
# not waits for the program.
#
# Wait patterns match on CONTENT that only the diff pane can show (a symbol
# from inside the hunk), never on a filename — filenames are already on
# screen in the tree, so waiting on one would return immediately and prove
# nothing.

Output "@OUT@.gif"
Output "@OUT@.txt"

# Recorded at 2x. GitHub renders README images at ~896px wide, so a 1792px
# asset displays at exactly half size: crisp on HiDPI instead of resampled.
Set Shell "bash"
Set FontSize 24
Set Width 1792
Set Height 700
Set Padding 16
Set Framerate 24
Set TypingSpeed 55ms
Set WaitTimeout 25s

Env COLORTERM "truecolor"
# The shell overlay runs $SHELL from $HOME. Pin both: otherwise the
# recording picks up the operator's prompt, aliases and rc files — neither
# reproducible nor anyone else's business.
Env SHELL "/bin/bash"
Env HOME "@HOME@"

Hide
Type "cd @REPO@ && clear"
Enter
Sleep 350ms
Type "@GITTER@"
Enter
Show
Wait+Screen /no file selected/
Sleep 840ms

# --- act 1: shape a commit ------------------------------------------------
# Select the staged file. The diff pane fills with the index-side diff.
Type "j"
Wait+Screen /is_empty/
Sleep 1120ms

# Over to Changes and find the Python file by SEARCH. `/` opens a prompt in
# the pane's OWN bottom border; the tree narrows to fuzzy matches as you
# type, first match selected live — so the diff pane fills with rollup.py
# without a single arrow key.
Tab
Sleep 300ms
Type "/"
Sleep 450ms
Type "roll"
Wait+Screen /MAX_BUCKETS/
Sleep 1300ms

# Enter accepts the match into a dimmed register (kept in the border for
# n/N recall); the tree un-narrows on the accepted file.
Enter
Sleep 940ms

# Stage it: the row leaves Changes and lands in Staged. Esc clears the
# now-matchless register.
Type "s"
Sleep 1360ms
Escape
Sleep 520ms

# Back to the top of Changes (k clamps there) and onto the Rust file, which
# still has an unstaged hunk of its own.
Type "k"
Type "k"
Type "k"
Type "k"
Type "k"
Type "j"
Type "j"
Wait+Screen /Buckets are evicted/
Sleep 979ms

# Stage just that hunk from inside the diff pane.
Tab
Sleep 420ms
Type "s"
Sleep 1260ms

# That file is now fully staged, so it leaves Changes and the cursor lands
# on a directory. Step onto the file below it, or the next beat opens on an
# empty diff pane. Two tabs: the cycle is Staged -> Changes -> Diff, so from
# Diff it wraps through Staged before reaching Changes.
Tab
Tab
Type "j"
Wait+Screen /MAX_POINTS/
Sleep 840ms

# --- act 2: the escape hatch ----------------------------------------------
# Space opens a which-key menu, so the app is discoverable without a cheat
# sheet. g t drops a real shell into the repo root.
Space
Sleep 630ms
Type "g"
Sleep 630ms
Type "t"
Wait+Screen /bash-/
Sleep 560ms
Type "git log --oneline --graph -6"
Enter
Wait+Screen /telemetry: ingest/
Sleep 1680ms

# Ctrl-T toggles back, and gitter refreshes git state on the way out.
Ctrl+T
Sleep 1260ms

# --- act 3: review the stack ----------------------------------------------
Space
Sleep 560ms
Type "w"
Sleep 560ms
Type "r"
Wait+Screen /Committed vs/
Sleep 1400ms

# Review layout is Committed + Stack + Diff, and Tab cycles them in that
# order, wrapping. Act 1 left focus on Changes, which w r just hid, so focus
# has already moved to the first visible leaf — Committed. No Tab needed.
Sleep 350ms
Type "j"
Wait+Screen /quantile/
Sleep 1540ms

# Onto ingest.py, whose committed diff ELIDES the unchanged lines between
# hunks into collapsible rules — the "N hidden" bars with the little
# up/down chevrons. (Committed: window.rs -> ingest.py.)
Type "j"
Type "j"
Wait+Screen /2 hidden/
Sleep 940ms

# Zoom is applied to the FOCUSED pane, so step onto the diff first
# (Committed -> Stack -> Diff) before zooming, or the file list zooms
# instead of the code.
Tab
Tab
Sleep 300ms
Space
Sleep 360ms
Type "w"
Sleep 360ms
Type "z"
Sleep 1000ms

# Drop the cursor past the class header, then K opens the elided run above
# it — the dataclass body (source / name / value / at) fills straight into
# the gutter, real line numbers and all (J opens below, X folds it back).
# No re-running git, no losing your place.
Type "jjjjjjjjjjj"
Sleep 500ms
Type "K"
Wait+Screen /value: float/
Sleep 1300ms

# Search runs over the WHOLE interleaved file — even lines hidden inside
# collapsed context, and removed lines git no longer ships. The border
# counts total matches and how many are hidden. Enter commits the query to
# a dimmed register; n / N walk the matches, revealing as they land.
Type "/"
Sleep 300ms
Type "Event"
Sleep 1000ms
Enter
Sleep 820ms
Type "n"
Sleep 900ms
Type "n"
Sleep 940ms
Escape
Sleep 600ms

# Unzoom back to the review layout.
Space
Sleep 300ms
Type "w"
Sleep 300ms
Type "z"
Sleep 979ms

# Into the stack, and zoom it. Zoom is not decoration here: at this width
# the Stack pane is ~36 columns and truncates the flag to "(needs r", so
# the beat only reads once the pane owns the screen.
# Two tabs again: unzooming left focus on Diff, which wraps through
# Committed before reaching Stack.
Tab
Tab
Sleep 420ms
Space
Sleep 350ms
Type "w"
Sleep 350ms
Type "z"
# feat/window-engine needs a restack because its parent was amended after
# it branched — derived from git alone, no Graphite metadata anywhere.
Wait+Screen /needs restack/
Sleep 1819ms

# Still zoomed: climb to the trunk (k clamps at the top row) and re-base the
# review onto it.
Type "k"
Type "k"
Type "k"
Type "k"
Type "k"
Sleep 489ms
Enter
Sleep 840ms

# Unzoom to see what that did: the title changes and two more files appear.
Space
Sleep 300ms
Type "w"
Sleep 300ms
Type "z"
Wait+Screen /Committed vs main/
Sleep 1540ms

# Back to the file list (Stack -> Diff -> Committed), to the top, then walk
# the three languages.
Tab
Tab
Sleep 350ms
Type "k"
Type "k"
Type "k"
Type "k"
Type "k"
Type "k"
Type "k"
Type "j"
Wait+Screen /quantile/
Sleep 1120ms
Type "j"
Type "j"
Wait+Screen /IngestError/
Sleep 1260ms
Type "j"
Type "j"
Type "j"
Wait+Screen /export function summarize/
Sleep 1260ms

# Mark it reviewed. The mark is keyed on the blob pair, so it survives a
# restack and invalidates the moment either side's content changes.
Type "r"
Sleep 3000ms
