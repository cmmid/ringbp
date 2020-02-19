library(ringbp)
library(tidyverse)

# Make the log file
logs <- file.path("log.txt")
con <- file(logs, open = "wt")
# # Send Output to log
sink(con)
sink(con, type = "message")

scenarios <- tidyr::expand_grid(
  ## Put parameters that are grouped by disease into this data.frame
  delay_group = list(tibble::tibble(
    delay = c("SARS", "Wuhan"),
    delay_shape = c(1.651524, 2.305172),
    delay_scale = c(4.287786, 9.483875)
  )),
  k_group = list(tibble::tibble(
    theta = c("<1%", "15%", "30%"),
    k = c(30, 1.95, 0.7)
  )),
  index_R0 = c(1.5, 2.5, 3.5),
  prop.asym = c(0, 0.1),
  control_effectiveness = seq(0, 1, 0.2),
  num.initial.cases = c(5, 20, 40)) %>%
  tidyr::unnest("k_group") %>%
  tidyr::unnest("delay_group") %>%
  dplyr::mutate(scenario = 1:dplyr::n())

## Parameterise fixed paramters
sim_with_params <- purrr::partial(ringbp::scenario_sim,
                                  cap_max_days = 365,
                                  cap_cases = 5000,
                                  r0isolated = 0,
                                  disp.iso = 1,
                                  disp.com = 0.16,
                                  quarantine = FALSE)

## Set up multicore if using see ?future::plan for details
## Use the workers argument to control the number of cores used.
future::plan("multiprocess")


## Run paramter sweep
sweep_results <- ringbp::parameter_sweep(scenarios,
                                         sim_fn = sim_with_params,
                                         samples = 1000,
                                         show_progress = TRUE)

saveRDS(sweep_results, file = "data-raw/res.rds")

sink(type = "message")
sink()
