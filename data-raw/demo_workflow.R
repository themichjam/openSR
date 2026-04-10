# Demo workflow script for opensr
library(opensr)

project <- sr_project_create("review", "toy-review")
protocol <- sr_protocol_init("Toy protocol", "Does intervention reduce outcome?")
search <- sr_search_log_init() |> sr_search_add("PubMed", "toy query", n_results = 10)
records <- sr_import_csv(system.file("extdata", "toy_records.csv", package = "opensr")) |> sr_deduplicate()
flow <- sr_flow_data(records)
print(sr_prisma_flow(flow))
