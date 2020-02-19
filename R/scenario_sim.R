#' Run a specified number of simulations with identical parameters
#' @author Joel Hellewell
#' @param n.sim number of simulations to run
#' @param num.initial.cases Initial number of cases in each initial cluster
#' @param num.initial.clusters Number of initial clusters
#' @param prop.ascertain Probability that cases are ascertained by contact tracing
#' @param cap_max_days Maximum number of days to run process for
#' @param cap_cases Maximum number of cases to run process for
#' @param r0isolated basic reproduction number for isolated cases
#' @param r0community basic reproduction number for non-isolated cases
#' @param disp.iso dispersion parameter for negative binomial distribution for isolated cases
#' @param disp.com dispersion parameter for negative binomial distribution for non-isolated cases
#' @param delay_shape shape of distribution for delay between symptom onset and isolation
#' @param delay_scale scale of distribution for delay between symptom onset and isolation
#'
#' @importFrom purrr safely
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' res <- scenario_sim(n.sim = 5,
#' num.initial.cases = 5,
#' cap_max_days = 365,
#' cap_cases = 2000,
#' r0isolated = 0,
#' r0community = 2.5,
#' disp.iso = 1,
#' disp.com = 0.16,
#' k = 0.7,
#' delay_shape = 2.5,
#' delay_scale = 5,
#' prop.asym = 0,
#' prop.ascertain = 0)
#' #' }
#'
scenario_sim <- function(n.sim = NULL, prop.ascertain = NULL, cap_max_days = NULL, cap_cases = NULL,
                         r0isolated = NULL, r0community = NULL, disp.iso = NULL, disp.com = NULL, k = NULL,
                         delay_shape = NULL, delay_scale = NULL, num.initial.cases = NULL, prop.asym = NULL,
                         quarantine = NULL) {

  # Run n.sim number of model runs and put them all together in a big data.frame
  res <- purrr::map(.x = 1:n.sim, ~ outbreak_model(num.initial.cases = num.initial.cases,
                                             prop.ascertain = prop.ascertain,
                                             cap_max_days = cap_max_days,
                                             cap_cases = cap_cases,
                                             r0isolated = r0isolated,
                                             r0community = r0community,
                                             disp.iso = disp.iso,
                                             disp.com = disp.com,
                                             delay_shape = delay_shape,
                                             delay_scale = delay_scale,
                                             k = k,
                                             prop.asym = prop.asym,
                                             quarantine = quarantine))


  # bind output together and add simulation index
  res <- data.table::rbindlist(res)
  res[, sim := rep(1:n.sim, rep(floor(cap_max_days / 7) + 1, n.sim)), ]
  return(res)
}
