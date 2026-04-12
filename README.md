# opensr

`opensr` is a fully free and open-source orchestration framework for transparent systematic reviews and meta-analyses.

## Lifecycle stage

**Status: MVP / alpha (experimental).**

The package is suitable for reproducible workflow prototyping and transparent audit trails, with iterative hardening expected as usage grows.

## What opensr is for

- Standardized project structure for review workflows.
- Machine-readable provenance and event logging.
- Structured PRISMA-aware objects for protocol/search/screening/extraction/reporting.
- Thin wrappers around mature open-source synthesis engines (`metafor`, `meta`).
- OSF-compatible manifests and archive bundles (OSF use optional).

## What opensr is not for

- Not a replacement for methodological expertise.
- Not an automated risk-of-bias judge.
- Not a PRISMA compliance certifier.
- Not dependent on proprietary tools or paid APIs.

> `opensr` is **PRISMA-aware**, not a guarantee of PRISMA compliance.

## Standards alignment

- **PRISMA 2020**: flow-ready reporting objects and checklist scaffolds.
- **PRISMA-P**: structured protocol schema and validation.
- **PRISMA-S**: search provenance schema and query hash capture.
- **OSF-style workflows**: manifest, archive bundle, optional `osfr` upload.

## FOSS-only policy

- Core workflows use open-source R packages only.
- No RevMan, Rayyan, Meta-Essentials, Excel, Word, or closed APIs in the core workflow.
- ASReview integration is import/export only.

## Canonical end-to-end toy workflow

A full toy workflow is provided at:

- `inst/scripts/end_to_end_toy_review.R` (`run_opensr_toy_workflow()`)
- tested by `tests/testthat/test-e2e-toy-workflow.R`

This workflow exercises project creation, search logging, record import + dedup, dual screening + reconciliation, full-text exclusion log, extraction, effect-size wrapping, meta-analysis, PRISMA flow, archive bundle, and reproducibility report.

## Reproducible pipeline example (`targets` + `renv`)

```r
library(opensr)

proj <- sr_project_create("review", name = "pipeline-demo")
sr_init_targets("review")
sr_init_renv("review")

# then:
# renv::init()
# tar_make()
```

## Local development commands

```r
devtools::document()
devtools::test()
devtools::check()
pkgdown::build_site()
quarto::quarto_render("vignettes/mini-workflow.qmd")
quarto::quarto_render("vignettes/standards-alignment.qmd")
```
