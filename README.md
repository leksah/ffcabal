# ffcabal — a fail-fast cabal wrapper

`ffcabal build` gets you to the first compile error as fast as possible by
type-checking each local component in a **cached `cabal repl`** (`:reload`)
before spending time on real builds — the same trick the Leksah IDE uses, made
standalone.

## What it does

1. `cabal build --dry-run` to elaborate a fresh `plan.json`.
2. Walks your project's local components in dependency order, type-checking
   each one in a persistent GHCi started with `cabal repl <component>`.
   * Repls live in **tmux** (default server, session `ffcabal`, one window per
     component) so they survive between runs — a recheck is just `:reload`,
     usually seconds — and you can attach and use them interactively:
     `tmux attach -t ffcabal`.
   * A repl is **reused only when the component's UnitId is unchanged** (same
     flags/deps); otherwise its window is respawned with a fresh repl.
3. As soon as a component's check passes, its real `cabal build <unit>` starts
   in the background; anything whose dependencies are already built runs in
   parallel (`-j`).
4. When every check has passed and the per-component builds are done, one
   final `cabal build <targets>` runs (mostly linking / no-op) and its exit
   code is ffcabal's.

The first error — from a `:reload` or a build — stops everything immediately.

## Usage

```
ffcabal build [TARGET…] [--builddir=DIR] [-jN] [--repl-only] [--timeout=SECS] [CABAL-OPTS…]
ffcabal repl  TARGET    [--builddir=DIR] [CABAL-OPTS…]
ffcabal check TARGET    [--builddir=DIR] [CABAL-OPTS…]
```

* `build` — the pipeline above.  With no TARGETs, all local components in the
  plan (except test/bench components) are checked and built.
* `repl` — ensure TARGET's repl exists (creating it in tmux if needed), run a
  `:reload` check, print the output, and tell you how to attach.
* `check` — like `repl` but exits quietly with the check status (for scripts).
* TARGETs may be `pkg`, `exe:name`, `lib:name`, or full `pkg:comp:name` forms.
* Unrecognised options are passed through to every `cabal` invocation.
* Run it from the project root (where `cabal.project` lives).
* `FFCABAL_TMUX_ARGS` (environment) is word-split and prefixed to **every**
  tmux invocation — e.g. `FFCABAL_TMUX_ARGS="-L my-socket"` moves the repls to
  a separate tmux server.  Unset = the default server.  The test suite uses
  this to run against a throwaway server.

## Notes and caveats

* The environment (notably `$PATH`) ffcabal runs under is captured into each
  repl window, so `cabal` inside tmux sees the *same configuration* as the
  `cabal` that ran the dry-run — cabal treats a changed environment as
  "configuration changed" and would otherwise rebuild the world.
* Parallel `cabal build` invocations share the builddir.  With dependencies
  pre-built each job only compiles its own unit; library registrations within
  the *same package* are serialised as a precaution.
* Output from concurrent jobs is funnelled through a single writer (stderr
  lines to stderr, stdout lines to stdout, each prefixed `[pkg:comp]`), so
  compiler errors are never interleaved mid-line.
* GHCi output is captured via `tmux pipe-pane`; a command is fenced by an
  echoed nonce (GHCi runs commands sequentially, and a PTY is a single ordered
  stream, so no stdout/stderr resynchronisation is needed — unlike driving
  ghci over separate pipes).
* Filtering/deduplicating GHCi diagnostics (ghcid-style) is future work; the
  captured output is shown verbatim.

## Tests

`cabal test ffcabal-test` — pure unit tests (plan parsing, graph ordering,
ANSI stripping, verdict/nonce discipline, quoting) plus guarded integration
tests that drive the real binary against a generated two-package project on a
throwaway tmux server (`FFCABAL_TMUX_ARGS="-L ffcabal-test-<pid>"`).
Integration is skipped — not failed — when `ghc`/`cabal`/`tmux` or a built
`ffcabal` binary (`cabal list-bin ffcabal`, or `$FFCABAL_BIN`) is unavailable.
