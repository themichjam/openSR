# End-to-end toy review workflow for opensr MVP.
# This script is intentionally minimal and deterministic.

run_opensr_toy_workflow <- function(root = file.path(tempdir(), "opensr-e2e")) {
  if (dir.exists(root)) unlink(root, recursive = TRUE)

  project <- sr_project_create(root, name = "toy-e2e")

  search <- sr_search_log_init() |>
    sr_search_add(source = "PubMed", query = "exercise AND blood pressure", n_results = 3)

  csv <- system.file("extdata", "toy_records.csv", package = "opensr")
  records <- sr_import_csv(csv) |>
    sr_deduplicate()

  screen <- sr_screen_template(records, reviewers = c("alice", "bob"))
  screen$data$decision_1 <- c("include", "exclude", "include")
  screen$data$decision_2 <- c("include", "include", "include")
  screen <- sr_screen_dual(screen)
  screen <- sr_screen_resolve(screen, data.frame(record_id = 2, final_decision = "exclude"))

  fulltext <- sr_fulltext_log(data.frame(record_id = 2, reason = "wrong population"))

  extraction <- sr_extract_template(records)
  extraction$data$study_id <- c("s1", "s1", "s2")
  extraction$data$outcome_type <- "generic"
  extraction$data$yi <- c(0.10, 0.10, 0.30)
  extraction$data$vi <- c(0.04, 0.04, 0.05)

  effects <- sr_es_generic(extraction$data, yi = "yi", vi = "vi")
  meta_fit <- sr_meta_random(effects)

  flow <- sr_flow_data(records, screen, fulltext)
  prisma <- sr_prisma_flow(flow)

  archive <- sr_archive_bundle(root)
  repro <- sr_report_reproducibility(root)

  list(
    project = project,
    search = search,
    records = records,
    screen = screen,
    fulltext = fulltext,
    extraction = extraction,
    effects = effects,
    meta = meta_fit,
    flow = flow,
    prisma = prisma,
    archive = archive,
    repro = repro
  )
}
