# Git Commit Style

Commit messages should explain **why** the change was made (the intent, motivation, or problem being solved). If the *how* is not obvious from reading the diff, explain that too — but skip mechanical descriptions of what the code does when the diff speaks for itself.

Good commit message structure:
- Subject line: concise summary of the change
- Body: the *why* — what was broken, what was missing, what goal this serves
- Body (if needed): the *how* — only when the mechanism isn't intuitive (e.g. a non-obvious workaround, a tricky invariant, an unintuitive API behavior)
