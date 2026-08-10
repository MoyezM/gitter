# The README recording. Driven by scripts/demo/record, which substitutes
# @GITTER@ / @REPO@ / @OUT@ and runs this through vhs.
#
# The story is the one SPEC.md pitches, in order: shape a commit, then
# review the stack. It deliberately ends on the review flow — that is the
# part no other git TUI does well, so it gets the last frame.
#
# Sleeps are generous on purpose. Panes are populated by shelling out to
# git asynchronously, so a keystroke that lands before the previous fetch
# resolves records a half-drawn pane. The fixture repo is tiny to keep
# those waits short; scripts/demo/record asserts the final frame afterwards
# so a bad take fails loudly instead of shipping.

Output "@OUT@.gif"
Output "@OUT@.txt"

Set Shell "bash"
Set FontSize 15
Set Width 1240
Set Height 520
Set Padding 10
Set TypingSpeed 90ms
Env COLORTERM "truecolor"

# Launch off-camera: nobody needs to watch an absolute path being typed.
Hide
Type "cd @REPO@ && clear"
Enter
Sleep 1s
Type "@GITTER@"
Enter
Sleep 5s
Show
Sleep 1500ms

# --- act 1: shape a commit ------------------------------------------------
# Focus opens on Staged. Select the staged file: the diff pane fills with
# the index-side diff, syntax highlighted.
Type "j"
Sleep 2s

# Over to Changes and down to a Python file — a second grammar on screen,
# and the file we are about to stage.
Tab
Sleep 800ms
Type "j"
Sleep 600ms
Type "j"
Sleep 600ms
Type "j"
Sleep 2s

# Stage it. The row leaves Changes and lands in Staged.
Type "s"
Sleep 2500ms

# Into the diff pane to stage a single hunk rather than a whole file.
Tab
Sleep 800ms
Type "j"
Sleep 400ms
Type "j"
Sleep 400ms
Type "j"
Sleep 1200ms
Type "s"
Sleep 2500ms

# --- act 2: review the stack ----------------------------------------------
# The Space menu is which-key style; letting it sit on screen shows the app
# is discoverable without a cheat sheet.
Space
Sleep 1800ms
Type "w"
Sleep 1500ms
Type "r"
Sleep 3s

# Review layout is Committed + Stack + Diff. Tab cycles them in that order
# and wraps, and focus was left on Diff by the hunk beat above — so one Tab
# lands on Committed. (Verified from the frame capture; if the layout order
# ever changes, scripts/demo/record's assertions catch it.)
Tab
Sleep 1s
Type "j"
Sleep 2500ms

# Into the stack and up to the trunk. Four k's because k at the top row is a
# no-op — that way the beat does not depend on where the cursor started.
Tab
Sleep 1200ms
Type "k"
Sleep 400ms
Type "k"
Sleep 400ms
Type "k"
Sleep 400ms
Type "k"
Sleep 800ms

# Re-base the review onto main: the title and the file list both change,
# because the diff is now this branch against the trunk rather than against
# its immediate parent.
Enter
Sleep 3s

# Back to the file list (Stack -> Diff -> Committed) and mark one reviewed.
# The mark is keyed on the blob pair, so it survives a restack and
# invalidates the moment either side's content changes.
Tab
Tab
Sleep 1s

# Climb to the top (k clamps there) then step onto the first FILE. Landing
# on a file rather than a directory matters twice over: a directory has no
# diff, so the pane would end on "no file selected", and r on a directory
# marks the whole subtree — a fine feature, but not the frame to end on.
Type "k"
Sleep 250ms
Type "k"
Sleep 250ms
Type "k"
Sleep 250ms
Type "k"
Sleep 600ms
Type "j"
Sleep 2s
Type "r"
Sleep 3500ms
