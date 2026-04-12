# Demo workflow script for opensr
library(opensr)

project <- sr_project_create("review", "toy-review")
protocol <- sr_protocol_init("Toy protocol", "Does intervention reduce outcome?")
protocol <- sr_protocol_validate(protocol)

search <- sr_search_log_init() |> sr_search_add("PubMed", "toy query", n_results = 10)
records <- sr_import_csv(system.file("extdata", "toy_records.csv", package = "opensr")) |> sr_deduplicate()

screen <- sr_screen_template(records)
screen$data$decision_1 <- "include"
screen$data$decision_2 <- "include"
screen <- sr_screen_dual(screen)

flow <- sr_flow_data(records, screen)
print(sr_prisma_flow(flow)$data)
