#' Create partial function to sample from gamma distributions
#' @author Joel Hellewell
#' @param dist_shape numeric shape parameter of Weibull distribution
#' @param dist_scale numeric scale parameter of Weibull distribution
#'
#' @return partial function that takes a numeric argument for number of samples
#' @export
#' @importFrom purrr partial
#' @examples
#'
dist_setup <- function(dist_shape = NULL, dist_scale = NULL) {
  out <- purrr::partial(rweibull,
                 shape = dist_shape,
                 scale = dist_scale)
  return(out)
}


#' Samples the serial interval for given incubation period samples
#'
#' @param inc_samp vector of samples from the incubation period distribution
#' @param k numeric skew parameter for sampling the serial interval from the incubation period
#'
#' @return
#' @export
#' @importFrom sn rsn
#' @examples
#'
inf_fn <- function(inc_samp = NULL, k = NULL) {

  out <- sn::rsn(n = length(inc_samp),
                 xi = inc_samp,
                 omega = 2,
                 alpha = k)

  out <- ifelse(out < 1, 1, out)

  return(out)
}

#' Calculate proportion of runs that have controlled outbreak
#'
#' @author Joel Hellewell
#' @return
#' @export
#' @inheritParams detect_extinct
#' @examples
#'
extinct_prob <- function(outbreak_df_week  = NULL, cap_cases  = NULL) {

  n_sim <- max(outbreak_df_week$sim)

  out <- outbreak_df_week %>%
    # new variable extinct = 1 if cases in weeks 10-12 all 0, 0 if not
    detect_extinct(cap_cases) %>%
    # number of runs where extinct = TRUE / number of runs
    .$extinct %>%
    sum(.) / n_sim

  return(out)
}


#' Calculate proportion of outbreaks that went extinct
#' @author Joel Hellewell
#' @param outbreak_df_week data.table  weekly cases producted by the outbreak model
#' @param cap_cases integer number of cumulative cases at which the branching process was terminated
#'
#' @return
#' @export
#' @importFrom dplyr group_by filter summarise ungroup
#' @examples
#'
detect_extinct <- function(outbreak_df_week  = NULL, cap_cases  = NULL) {

  outbreak_df_week %>%
    dplyr::group_by(sim) %>% # group by simulation run
    dplyr::filter(week %in% 12:16) %>%
    dplyr::summarise(extinct =
                       ifelse(all(weekly_cases == 0 &
                                    cumulative < cap_cases),
                              1, 0)) %>%
    dplyr::ungroup()

}


#' Create sub-plot for supplementary figures
#'
#' @param theta.in character filtering value for theta
#' @param delay.in character filtering value for delay
#' @param prop.asym.in numeric filtering value for proportion of asymptomatic cases
#' @param num.initial.cases.in integer filtering value for number of initial cases
#' @param index_R0.in numeric filtering value for community R0 value
#' @param res.in data.table of results from parameter sweep
#'
#' @return
#' @export
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_line geom_point facet_wrap ylab xlab scale_x_continuous scale_y_continuous coord_cartesian
#' @importFrom cowplot panel_border
#'
#' @examples
#'
sub_plot <- function(theta.in = "15%",
                     delay.in = "SARS",
                     prop.asym.in = 0,
                     num.initial.cases.in = 20,
                     index_R0.in = 2.5,
                     res.in = NULL,
                     facet.by = NULL,
                     col.by = NULL) {

  col.by <- ggplot2::ensym(col.by)

  res.in %>%
    dplyr::filter(theta %in% theta.in,
                  delay %in% delay.in,
                  prop.asym %in% prop.asym.in,
                  num.initial.cases %in% num.initial.cases.in,
                  index_R0 %in% index_R0.in) %>%
    # Ugly factor re-naming
    dplyr::mutate(num.initial.cases = factor(num.initial.cases,
                                             levels = c(5, 20, 40),
                                             labels = c("5 cases",
                                                        "20 cases",
                                                        "40 cases"))) %>%
    dplyr::mutate(delay = factor(delay,
                                 levels = c("SARS", "Wuhan"),
                                 labels = c("Short isolation delay",
                                            "Long isolation delay"))) %>%
    dplyr::mutate(prop.asym = factor(prop.asym,
                                     levels = c(0, 0.1),
                                     labels = c("No asymptomatic cases ",
                                                "10% cases asmyptomatic"))) %>%
    dplyr::mutate(theta = factor(theta,
                                 levels = c("<1%", "15%", "30%"),
                                 labels = c("<1% trans. pre-onset",
                                            "15% trans. pre-onset",
                                            "30% trans. pre-onset"))) %>%
    # Put plot together
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = pext,
                                 color = as.factor(!!col.by))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        ggplot2::aes(fill = as.factor(!!col.by)), size = 3) +
    ggplot2::facet_wrap(as.formula(paste(". ~", facet.by))) +
    ggplot2::ylab("Simulated outbreaks controlled (%)") +
    ggplot2::xlab("Contacts traced (%)") +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2),
                                labels = seq(0, 100, 20)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2),
                                labels = seq(0, 100, 20)) +
    cowplot::panel_border() +
    ggplot2::coord_cartesian(ylim = c(0, 1))

}
