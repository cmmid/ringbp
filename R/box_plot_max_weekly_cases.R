#' Create box plots of maximum weekly cases by scenario
#' @author Amy Gimma and Sam Abbott
#'
#' @param results results of the branching model in a data.frame or tibble
#' @param cap_cases the maximimum number of cases per outbreak scenario; default is 5000
#' @param extinct_threshold filters the minimum proportion of simulations that become extinct per scenario; default 0.8
#' @param theta_value A Character string defaulting to "15%". Determines the proportion of infections that occur prior to
#' sypmtom onset.
#' @param prop_asym A numeric string defaulting to 0. Filters the proportion of infectiouns are asymptomatic
#' @param facet_scales passed to facet_grid’s scales parameter; default is "fixed"
#' @param filt_control_effectiveness filters by the minimum control effectiveness proportion; default is 0.4
#' @param flip_coords flip coordinates of the box plot; default is FALSE
#' @param num_initial_cases filters by the number of initial cases in the scenario; default is 40
#' @param record_params option to display the params as a caption (used for testing); default FALSE
#' @param y_lim Numeric the limit of the y axis to show.
#' @importFrom ggrepel geom_label_repel
#' @importFrom dplyr group_by mutate ungroup filter mutate left_join summarise select
#' @importFrom tidyr unnest
#' @importFrom cowplot theme_minimal_hgrid panel_border
#' @importFrom ggplot2 ggplot aes stat_summary facet_grid vars scale_fill_gradient scale_y_continuous scale_x_discrete theme_bw theme labs ggtitle coord_flip
#' @export
#' @examples
#'
#'
box_plot_max_weekly_cases <- function(results = NULL,
                                      cap_cases = 5000,
                                      extinct_thresold = 0.1,
                                      theta_value = "15%",
                                      prop_asym = 0,
                                      facet_scales = "fixed",
                                      filt_control_effectiveness = 0.4,
                                      flip_coords = FALSE,
                                      num_initial_cases = 20,
                                      record_params = FALSE,
                                      y_lim = NULL) {
  filt_results <- results %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(prob_extinct = extinct_prob(sims[[1]], cap_cases = cap_cases)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(prob_extinct >= extinct_thresold) %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(
      extinct = list(sims[[1]] %>%
                       detect_extinct(cap_cases = cap_cases) %>%
                       dplyr::filter(extinct == 1))) %>%
    dplyr::mutate(
      sum_meas = list(dplyr::left_join(extinct[[1]],
                                       sims[[1]],
                                       by = ("sim")) %>%
                        dplyr::group_by(sim) %>%
                        dplyr::summarise(max_weekly_cases = max(weekly_cases),
                                         total_cases = max(cumulative)))
    ) %>%
    dplyr::select(-extinct) %>%
    dplyr::ungroup() %>%
    tidyr::unnest(c("sum_meas")) %>%
    dplyr::select(-sims)



  ## Clean data
  df <- filt_results %>%
    dplyr::filter(control_effectiveness >= filt_control_effectiveness) %>%
    dplyr::filter(theta == theta_value) %>%
    dplyr::filter(num.initial.cases %in% num_initial_cases) %>%
    dplyr::filter(prop.asym == prop_asym) %>%
    dplyr::group_by(scenario) %>%
    dplyr::ungroup() %>%
    rename_variables_for_plotting()


  df_summary <- df %>%
    dplyr::group_by(index_R0, control_effectiveness, prob_extinct, delay) %>%
    dplyr::summarise(
      y0 = quantile(max_weekly_cases, 0.025),
      y25 = quantile(max_weekly_cases, 0.25),
      y50 = median(max_weekly_cases),
      y75 = quantile(max_weekly_cases, 0.75),
      y100 = quantile(max_weekly_cases, 0.975)
    ) %>%
    dplyr::mutate(y100 = ifelse(y100 > y_lim, y_lim, y100))


  plot <- df_summary %>%
    ggplot2::ggplot(ggplot2::aes(y = y50,
                                 x = factor(control_effectiveness),
                                 fill = prob_extinct), alpha = 0.9) +
    ggplot2::geom_boxplot(
      ggplot2::aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100),
      stat = "identity"
    ) +
    ggplot2::facet_grid(rows = ggplot2::vars(factor(delay)),
                        cols = ggplot2::vars(index_R0),
                        scales = facet_scales) +
    ggplot2::scale_fill_gradient(low = "white", high = "deepskyblue3", guide = "none") +
    ggplot2::scale_y_continuous(breaks = seq(0, 1000, 50)) +
    ggplot2::scale_x_discrete(breaks = seq(0, 1, 0.2),
                              labels = paste0(seq(0, 100, 20), "")) +
    ggplot2::theme(legend.position = "none",
                   plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(fill = "Percentage of contacts traced") +
    ggplot2::labs(y = "Maximum weekly cases in controlled outbreaks",
                  x = "Contacts traced (%)")
  if (flip_coords) {
    plot <- plot + ggplot2::coord_flip()
  }
  if (record_params) {
    plot <- plot + ggplot2::labs(caption = paste("params: ",
                                                 "extinct_thresold:", extinct_thresold,
                                                 "; filt_control_effectiveness:", filt_control_effectiveness,
                                                 "; cap_cases: ", cap_cases,
                                                 "; num_initial_cases: ", num_initial_cases,
                                                 "; facet_scales: ", facet_scales,
                                                 "; 95% conf interval",
                                                 sep = " "))
  }
  plot <- plot + cowplot::theme_minimal_hgrid() + cowplot::panel_border()

  ##Text positions
  text_positions <- df %>%
    dplyr::select(index_R0, control_effectiveness, prob_extinct, delay, max_weekly_cases) %>%
    dplyr::group_by(index_R0, control_effectiveness, prob_extinct, delay) %>%
    dplyr::summarise(y50 = quantile(max_weekly_cases, 0.5, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(label_extinct = round(prob_extinct * 100, 0) %>%
                    paste0("%"))



  plot <- plot +
    ggrepel::geom_label_repel(data = text_positions,
                              ggplot2::aes(label = label_extinct),
                              col = "black", fill = "white",
                              size = 4,
                              point.padding = NA) +
    ggplot2::coord_cartesian(ylim=c(0,y_lim))


  return(plot)
}

#' Rename variables for plotting
#'
#' @param df Dataframe of results
#'
#' @return
#' @export
#' @importFrom dplyr mutate
#' @author Sam Abbott
#' @examples
#'
rename_variables_for_plotting <- function(df = NULL) {

  df %>%
    dplyr::mutate(delay = factor(delay,
                                 levels = c("SARS", "Wuhan"),
                                 labels = c("Short isolation delay",
                                           "Long isolation delay"))) %>%
    dplyr::mutate(index_R0 = index_R0 %>%
                    paste0("R0 = ", .))
}
