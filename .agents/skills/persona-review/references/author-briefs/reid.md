# Author brief: Reid

Paste this into a coding worker's prompt, together with the worker brief,
before the worker writes or updates code that Reid will review. It lists the
things Reid most often asks agents to fix after the fact. Fixing them up front
saves a review round.

## Workflows and tooling

1. Put release, validation, and digest logic in unit-tested Dart, not in
   multi-line `run: |` bash blocks in `.github/workflows/`.
2. Reuse the repository's existing CI actions for format, analyze, and test
   instead of repeating those steps in a new workflow.

## One source of truth

3. Parse YAML, JSON, and URIs once, with the package the repository already
   uses, into typed objects. Don't check several spellings of a value by hand
   or parse structured files with a regex.
4. Keep a parameter's default value in the class that owns it. If callers
   must pass "no value", make the parameter nullable rather than repeating the
   default in a registry or caller.
5. Give each main class its own file.

## Docs and prose

6. Name the noun instead of writing `it`, `this`, `that`, or `they` in
   descriptions, rule docs, and doc comments.
7. Don't use Markdown tables in runbooks such as `RELEASING.md`.
8. Document all parameters of a method, or none.
9. Use named parameters when a method takes three or more `String`
   arguments.
10. Keep comments and doc comments short. If a doc comment needs many lines to
    explain one narrow case, look for a more general concept first.

## Tests

11. Add new tests to the existing test file for that feature, and reuse its
    fixtures, instead of creating a parallel test file.
12. Test behavior with round trips (generate, reload, verify) rather than
    asserting exact output that only detects change. Say in the test name what
    behavior it protects.
13. When a test passes a magic or empty value (for example `''` for a dry
    run), add a short comment at the call site that says why.

## Follow-up rounds

14. When review feedback says a design is confusing, restructure it. Don't
    add a boolean flag or an extra branch on top of the old structure.
15. When you answer a reviewer's question about non-obvious behavior, also put
    the answer in a code comment.
16. Keep each PR to its own change. Merge the base branch so commits from
    other PRs that already landed don't show up in the diff.
