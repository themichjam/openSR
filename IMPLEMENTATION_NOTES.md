# Implementation notes (final hardening pass)

## Completed in this pass

- Added a single canonical end-to-end toy workflow script: `inst/scripts/end_to_end_toy_review.R`.
- Added an automated end-to-end test (`tests/testthat/test-e2e-toy-workflow.R`) that executes the full workflow path.
- Added stable-format output regression tests for printed objects, PRISMA flow, PRISMA checklist, reproducibility report schema, and archive manifest schema (`tests/testthat/test-output-stability.R`).
- Tightened RIS/BibTeX ingestion behavior by explicitly validating required markers and failing with clear error messages when unsupported input variants are detected.
- Audited and standardized provenance details for file-writing operations:
  - project event log now records a deterministic `event_id`
  - protocol export stores file hash in provenance
  - archive bundles record archive SHA-256
  - OSF manifest records manifest hash
  - `sr_init_targets()` and `sr_init_renv()` log file write events when project logs exist.
- Updated README messaging to clarify lifecycle stage, scope boundaries, PRISMA-aware/not-certifying stance, OSF-optional support, and FOSS-only constraints.

## Verification status in this environment

The following commands were attempted but could not be executed because the sandbox lacks R:

- `devtools::document()`
- `devtools::test()`
- `devtools::check()`
- `pkgdown::build_site()`
- Quarto vignette renders

Blocker observed: `/bin/bash: line 1: R: command not found`.

## Remaining partial items (intentional)

- RIS/BibTeX support is intentionally narrowed to explicit, common variants with strict validation rather than broad permissive parsing.
- PRISMA outputs remain structured scaffolds and workflow aids, not compliance certification.
- OSF upload remains optional and user-auth dependent.
