#' Sweep across parameters
#'
#' @description Explore scenarios using gridding with sampling for parameters not in the grid. Parameters that
#' are included in the grid are currently hard coded. Use the `future` package to control parallisation
#' outside of the function.
#'
#' @param scenarios A dataframe containing all gridded  scenarios - see the examples for the required structure.
#' Defaults to NULL.
#' @param samples Numeric, defaults to 1. The number of samples to take.
#' @param sim_fn Function, defaults to NULL. The vectorised model simulation function - see the examples
#' for usage.
#' @param show_progress Logical, defaults to `TRUE`. Show the progress of the parameter sweep.
#' @author Sam Abbott
#'
#' @return A nested tibble containing the parameters for each scenario and a nested list of output
#' from `wuhan_sim`.
#' @export
#' @importFrom dplyr group_by mutate ungroup sample_frac
#' @importFrom tidyr nest unnest
#' @importFrom furrr future_map
#' @importFrom purrr safely
#' @examples
#'
#'
#'\dontrun{
#' library(ringbp)
#' library(tibble)
#'
#' scenarios <- tidyr::expand_grid(
#' ## Put parameters that are grouped by disease into this data.frame
#' delay_group = list(tibble::tibble(
#'  delay = c("SARS","Wuhan"),
#'  delay_shape = c(1.651524,2.305172),
#'  delay_scale = c(4.287786,9.483875)
#' )),
#' k_group = list(tibble::tibble(
#'  theta = c("<1%","15%","30%"),
#'  k = c(1,0.88,0.47)
#' )),
#' index_R0 = c(1.5,2.5,3.5),
#' control_effectiveness = seq(0,1,0.2),
#' num.initial.clusters = c(5,20,40)) %>%
#'  tidyr::unnest("k_group") %>%
#'  tidyr::unnest("delay_group") %>%
#'  dplyr::mutate(scenario = 1:dplyr::n())
#'
#' ## Parameterise fixed paramters
#' sim_with_params <- purrr::partial(ringbp::scenario_sim,
#'                                  num.initial.cases=1,
#'                                  cap_max_days = 365,
#'                                  cap_cases = 5000,
#'                                  r0isolated = 0,
#'                                  disp.iso=1,
#'                                  disp.com = 0.16,
#'                                  mu_ip = 5.8, # incubation period mean
#'                                  sd_ip = 2.6, # incubation period sd
#'                                  mu_si = 7.5, # serial interval mean
#'                                  sd_si = 3.4) # serial interval sd
#'
#'
#' ## Default is to run sequntially on a single core
#' future::plan("sequential")
#' ## Set up multicore if using see ?future::plan for details
#' ## Use the workers argument to control the number of cores used.
#' future::plan("multiprocess")
#'
#'
#' ## Run paramter sweep
#' sweep_results <- ringbp::parameter_sweep(scenarios, sim_fn = sim_with_params, samples = 1)
#'
#'
#' sweep_results
#' }
parameter_sweep <- function(scenarios = NULL, samples = 1,
                            sim_fn = NULL, show_progress = TRUE) {

  safe_sim_fn <- purrr::safely(sim_fn)

  scenario_sims <- scenarios %>%
    dplyr::group_by(scenario) %>%
    tidyr::nest() %>%
    dplyr::ungroup() %>%
    ##Randomise the order of scenarios - helps share the load across cores
    dplyr::sample_frac(size = 1, replace = FALSE) %>%
    dplyr::mutate(sims = furrr::future_map(
      data,
      ~ safe_sim_fn(n.sim = samples,
               num.initial.cases = .$num.initial.cases,
               r0community = .$index_R0,
               k = .$k,
               delay_shape = .$delay_shape,
               delay_scale = .$delay_scale,
               prop.ascertain = .$control_effectiveness,
               prop.asym = .$prop.asym
      )[[1]],
      .progress = show_progress
    )) %>%
    tidyr::unnest(cols = "data")

  return(scenario_sims)
}
