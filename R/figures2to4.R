#' Makes Figure 2a from manuscript
#' @author Joel Hellewell
#'
#' @return
#' @export
#' @importFrom data.table data.table
#' @importFrom tidyr gather
#' @importFrom magrittr %<>%
#' @importFrom dplyr mutate
#' @importFrom data.table data.table
#' @importFrom ggplot2 ggplot geom_ribbon theme_bw theme xlab ylab geom_line geom_vline scale_colour_brewer scale_fill_brewer element_text aes
#' @examples
#'
make_figure_2a <- function() {

  # Weibull estimation of dgamma(x,shape=2.448898,rate = 0.639399)
  delay_sars <- function(x) {
    dweibull(x, shape = 1.651524, scale = 4.287786)
  }

  delay_wuhan <- function(x) {
    dweibull(x, shape = 2.305172, scale = 9.483875)
  }

  out <- data.table::data.table(x = seq(0, 15, 0.01))
  out[, `:=`(delay_wuhan = delay_wuhan(x),
             delay_sars = delay_sars(x)), ]
  out %<>% tidyr::gather("dist", "value", -x)


  medians <- data.frame(x = c(3.43, 8.09),
                      dist = c("delay_sars", "delay_wuhan")) %>%
    dplyr::mutate(dist = factor(dist,
                                levels = c("delay_sars",
                                           "delay_wuhan"),
                                labels = c("Short delay",
                                           "Long delay")))


  out %>%
    ggplot2::ggplot(ggplot2::aes(x = x,
                                 y = value,
                                 ymin = 0,
                                 ymax = value,
                                 fill = as.factor(dist))) +
    ggplot2::geom_ribbon(alpha = 0.4) +
    cowplot::theme_cowplot() +
    ggplot2::geom_line(aes(x, value)) +
    ggplot2::theme(legend.position = "bottom",
                   axis.text = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_text(size = 12),
                   legend.text = ggplot2::element_text(size = 10)) +
    ggplot2::scale_fill_brewer(name = "",
                               labels = c("Short delay",
                                          "Long delay"),
                               palette = "Set1") +
    ggplot2::scale_colour_brewer(guide = "none",
                                 palette = "Set1") +
    ggplot2::geom_vline(data = medians,
                        ggplot2::aes(xintercept = x,
                                     col = as.factor(dist)),
                        lty = 2,
                        size = 0.8) +
    ggplot2::labs(tag = "A",
                  x = "time since infection (days)",
                  y = "probability density")

}


#' Constructs figure 2 from the manuscript
#' @author Joel Hellewell
#' @return
#' @export
#' @importFrom sn dsn
#' @importFrom ggplot2 ggplot aes geom_line geom_vline coord_cartesian labs geom_ribbon scale_fill_manual theme element_text
#' @importFrom cowplot theme_cowplot
#' @examples
#'\dontrun{
#'make_figure_2()
#'}
make_figure_2 <- function() {

  p2 <- data.frame(x = seq(0, 15, 0.1),
                   y = dweibull(x = seq(0, 15, 0.1),
                                shape = 2.322737,
                                scale = 6.492272)) %>%
    ggplot2::ggplot(aes(x = x, y = y)) +
    ggplot2::geom_line() +
    cowplot::theme_cowplot() +
    ggplot2::geom_vline(xintercept = 5.8, lty = 2) +
    ggplot2::coord_cartesian(xlim = c(0, 13)) +
    ggplot2::labs(tag = "B", x = "time since infection (days)", y = "probability density") +
    ggplot2::geom_ribbon(aes(ymax = y, ymin = 0),
                         fill = "chartreuse2",
                         alpha = 0.4)


  p3 <- data.frame(y = c(sn::dsn(x = seq(0, 13, 0.1), xi = 5, omega = 2, alpha = 0.7),
                       sn::dsn(x = seq(0, 13, 0.1), xi = 5, omega = 2, alpha = 1.95),
                       sn::dsn(x = seq(0, 13, 0.1), xi = 5, omega = 2, alpha = 30)),
                   x = rep(rep(seq(0, 13, 0.1), 3)),
                   theta = rep(c("30%", "15%", "<1%"), rep(131, 3))) %>%
    ggplot2::ggplot(aes(x, y, fill = theta)) +
    ggplot2::geom_ribbon(aes(ymin = 0, ymax = y), alpha = 0.4) +
    ggplot2::geom_line(aes(x, y)) +
    cowplot::theme_cowplot() +
    ggplot2::coord_cartesian(xlim = c(0, 13)) +
    ggplot2::geom_vline(xintercept = 5) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_fill_manual(values = c("grey65",
                                          "goldenrod3",
                                          "orchid"),
                               name = "Proportion of\ntransmission\nbefore symptoms") +
    ggplot2::labs(tag = "C",
                  x = "time since infection (days)",
                  y = "probability density")


  (make_figure_2a() | (p2 / p3)) & theme(axis.text = element_text(size = 10),
                                         legend.title = element_text(size = 11),
                                         axis.title = element_text(size = 11))

}


#' Construct Figure 3a from manuscript
#'
#' @param df
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot geom_line geom_point scale_fill_manual scale_color_manual scale_x_continuous scale_y_continuous theme labs
#' @importFrom cowplot theme_cowplot
#' @importFrom dplyr filter select
#' @examples
#'\dontrun{
#'make_figure_3a()
#'}
make_figure_3a <- function(df = NULL) {
  pl <- df %>%
    dplyr::filter(num.initial.cases == 20,
                  theta == "15%",
                  delay == "SARS",
                  prop.asym == 0) %>%
    dplyr::select(control_effectiveness, index_R0, pext) %>%
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = pext,
                                 color = as.factor(index_R0))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        ggplot2::aes(fill = as.factor(index_R0)),
                        size = 3) +
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("red", "black", "firebrick4")) +
    ggplot2::scale_color_manual(values = c("red", "black", "firebrick4"),
                                name = "Reproduction\nnumber")  +
    cowplot::theme_cowplot() +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2),
                                labels = seq(0, 100, 20)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2),
                                labels = seq(0, 100, 20)) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(tag = "A",
                  x = "Contacts traced (%)",
                  y = "Simulated outbreaks controlled (%)")
  return(pl)
}


#' Generate a figure comparing the effective reproduction no with contacts traced.
#' @author Sam Abbott
#' @param df A dataframe of results as produced by `parameter_sweep`
#'
#' @return A ggplot2 plot of the effective reproduction no vs contacts traced.
#' @export
#'
#' @examples
#'\dontrun{
#'make_figure_3b()
#'}
make_figure3b <- function(df = NULL) {
  df_extracted <-  df %>%
    dplyr::mutate(effective_r0 = purrr::map(
      sims,
      ~ dplyr::group_by(., sim) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::summarise(median_eff_r0 = median(effective_r0,
                                                na.rm = TRUE),
                         lower = quantile(effective_r0, 0.025,
                                          na.rm = TRUE),
                         iqr_lower = quantile(effective_r0,
                                              0.25,
                                              na.rm = TRUE),
                         iqr_upper = quantile(effective_r0,
                                              0.75,
                                              na.rm = TRUE),
                         upper = quantile(effective_r0,
                                          0.975,
                                          na.rm = TRUE))
    )) %>%
    tidyr::unnest("effective_r0")

  df_extracted %>%
    dplyr::filter(prop.asym == 0,
                  theta == "15%",
                  num.initial.cases == 20,
                  delay == "SARS") %>%
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = median_eff_r0,
                                 col = as.factor(index_R0),
                                 fill = as.factor(index_R0))) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lower,
                                      ymax = upper,
                                      col = NULL),
                         alpha = 0.1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = iqr_lower,
                                      ymax = iqr_upper,
                                      col = NULL),
                         alpha = 0.3) +
    ggplot2::geom_line() +
    ggplot2::xlab("Contacts traced (%)") +
    ggplot2::ylab("Effective reproduction number") +
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2),
                                labels = seq(0, 100, 20)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 3.5, 0.5)) +
    ggplot2::geom_hline(yintercept = 1, lty = 2, size = 0.75) +
    ggplot2::geom_point(shape = 21, col = "black",
                        ggplot2::aes(fill = as.factor(index_R0)),
                        size = 3) +
    ggplot2::scale_fill_manual(values = c("red", "black", "firebrick4")) +
    ggplot2::scale_color_manual(values = c("red", "black", "firebrick4"),
                                name = "Reproduction\nnumber") +
    cowplot::theme_cowplot() +
    ggplot2::theme(legend.position = "none")
}



#' Construct Figure 4 from the manuscript
#'
#' @param res
#'
#' @return
#' @export
#' @importFrom ggplot2 ggplot geom_line geom_point scale_fill_manual scale_color_manual aes theme xlab ylab element_text
#' @importFrom dplyr filter mutate
#' @importFrom cowplot theme_cowplot
#' @importFrom patchwork plot_annotation
#'
#' @examples
#'\dontrun{
#'make_figure_4()
#'}
make_figure_4 <- function(res = NULL) {

  f4p1 <- res %>%
    dplyr::filter(delay == "SARS",
                  theta == "15%",
                  index_R0 == 2.5,
                  prop.asym == 0) %>%
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = pext,
                                 color = as.factor(num.initial.cases))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        aes(fill = as.factor(num.initial.cases)),
                        size = 3) +
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("dodgerblue", "black", "dodgerblue3")) +
    ggplot2::scale_color_manual(values = c("dodgerblue", "black", "dodgerblue3"),
                                name = "Number of\ninitial cases")  +
    cowplot::theme_cowplot()

  f4p2 <- res %>%
    dplyr::filter(num.initial.cases == 20,
                  theta == "15%",
                  index_R0 == 2.5,
                  prop.asym == 0) %>%
    dplyr::mutate(delay = factor(delay,
                                 levels = c("SARS", "Wuhan"),
                                 labels = c("Short", "Long"))) %>%
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = pext,
                                 color = as.factor(delay))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        aes(fill = as.factor(delay)),
                        size = 3) +
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("black", "forestgreen")) +
    ggplot2::scale_color_manual(values = c("black", "forestgreen"),
                                name = "Onset to\nisolation\ndelay") +
    cowplot::theme_cowplot()

  f4p3 <- res %>%
    dplyr::filter(num.initial.cases == 20,
                  delay == "SARS",
                  index_R0 == 2.5,
                  prop.asym == 0) %>%
    ggplot2::ggplot(ggplot2::aes(x = control_effectiveness,
                                 y = pext,
                                 color = as.factor(theta))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        aes(fill = as.factor(theta)),
                        size = 3) +
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("mediumpurple2", "black", "mediumpurple4")) +
    ggplot2::scale_color_manual(values = c("mediumpurple2", "black", "mediumpurple4"),
                                name = "Pre-symptom\ntransmission") +
    cowplot::theme_cowplot()

  f4p4 <- res %>%
    dplyr::filter(delay == "SARS",
                  theta == "15%",
                  index_R0 == 2.5,
                  num.initial.cases == 20) %>%
    dplyr::mutate(prop.asym = factor(prop.asym,
                                     levels = c(0, 0.1),
                                     labels = c("0%", "10%"))) %>%
    ggplot2::ggplot(aes(x = control_effectiveness,
                        y = pext,
                        color = as.factor(prop.asym))) +
    ggplot2::geom_line(size = 0.75) +
    ggplot2::geom_point(shape = 21,
                        col = "black",
                        aes(fill = as.factor(prop.asym)),
                        size = 3) +
    ggplot2::scale_fill_manual(guide = "none",
                               values = c("black", "chocolate4")) +
    ggplot2::scale_color_manual(values = c("black", "chocolate4"),
                                name = "Proportion\nof cases\nwithout\nsymptoms")  +
    cowplot::theme_cowplot()

  fig4 <- (f4p1 + f4p2) / (f4p3 + f4p4) + patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(legend.position = "left",
          plot.tag.position = "topleft",
          plot.tag = element_text(size = 20, face = "bold"),
          axis.text = element_text(size = 11),
          axis.title = element_text(size = 11),
          legend.title = element_text(size = 11),
          legend.text = element_text(size = 11)) &
    ggplot2::ylab("Simulated outbreaks controlled (%)") &
    ggplot2::xlab("Contacts traced (%)") &
    ggplot2::scale_x_continuous(breaks = seq(0, 1, 0.2),
                                labels = seq(0, 100, 20)) &
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.2),
                                labels = seq(0, 100, 20))

  return(fig4)
}

