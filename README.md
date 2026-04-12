# opensr

`opensr` is a fully free and open-source orchestration and standards layer for transparent systematic reviews and meta-analysis.

## Purpose

The package standardises project structure, metadata, provenance logging, screening records, extraction templates, synthesis wrappers, and archival exports while interoperating with mature analysis engines (e.g., `metafor`, `meta`, `revtools`, `robvis`) instead of replacing them.

## FOSS-only policy

- Core workflows use only open-source dependencies.
- No proprietary tools/platforms are required.
- Interoperability with ASReview is supported through open import/export.

## Standards alignment

- **PRISMA 2020**: reporting scaffolds and flow summaries.
- **PRISMA-P**: protocol object and validation helpers.
- **PRISMA-S**: search log provenance and structured capture.

> Note: `opensr` is PRISMA-aware, **not a guarantee of PRISMA compliance**.

## OSF workflow note

`opensr` supports OSF-style workflows and can upload via `osfr`, but OSF use remains optional.
All core workflows are runnable locally from code alone.

## Dependency philosophy

- Prefer battle-tested ecosystem packages for analysis (`meta`, `metafor`).
- Focus `opensr` on orchestration, standards, and auditability.
- Keep reproducibility first-class with file hashes, logs, and optional `targets`/`renv` integration.

## Quickstart

```r
library(opensr)

proj <- sr_project_create("review")
protocol <- sr_protocol_init(
  title = "Exercise and blood pressure",
  question = "What is the effect of exercise on systolic blood pressure?"
)

search <- sr_search_log_init() |>
  sr_search_add("PubMed", "exercise AND blood pressure", n_results = 245)

records <- sr_import_csv(system.file("extdata", "toy_records.csv", package = "opensr")) |>
  sr_deduplicate()

screen <- sr_screen_template(records) |>
  sr_screen_dual()

flow <- sr_flow_data(records, screen)
prisma <- sr_prisma_flow(flow)

archive <- sr_archive_bundle("review")
```

## Mini demo data

A toy dataset is provided in `inst/extdata/toy_records.csv`.
