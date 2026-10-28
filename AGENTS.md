# meeting-mode development

- Use Org (`.org`) for documentation. `AGENTS.md` is the required OpenCode
  exception; use Markdown only when a tool requires a specific filename or
  format.
- The package implementation is `meeting-mode.el`.
- The test suite is `test/meeting-mode-tests.el`.
- Run `make all` before committing.
- Run `make test` for the fast ERT loop.
- Keep public symbols under the `meeting-` prefix and internals under
  `meeting--`.
- Keep the package standalone: do not add keybindings, configuration-specific
  symbols, or dependencies on a user's Emacs configuration.
- Keep Transient integration optional; the package must load and work without
  Transient installed.
- Prefer built-in Emacs libraries and declare the supported Emacs version in
  `Package-Requires`.
