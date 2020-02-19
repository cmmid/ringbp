#' Constructs figure S6 from supplementary material
#' @author Joel Hellewell
#' @return
#' @export
#' @importFrom data.table data.table rbindlist
#' @importFrom sn rsn
#' @importFrom dplyr mutate group_by summarise
#' @importFrom ggplot2 ggplot aes geom_density theme_bw geom_vline scale_x_continuous coord_cartesian xlab ylab scale_fill_discrete scale_color_discrete theme
#' @examples
#'
make_figure_S6 <- function() {

  inf_fn <- function(inc_samp, k) {
    out <- sn::rsn(n = length(inc_samp),
                   xi = inc_samp,
                   omega = 2,
                   alpha = k)
    out <- ifelse(out < 1, 1, out)
    return(out)
  }

  incfn <- dist_setup(dist_shape = 2.322737,
                      dist_scale = 6.492272)

  inc_df <- data.table::data.table(x = seq(0, 20, 0.01),
                                   y = dweibull(seq(0, 20, 0.01),
                                                shape = 2.322737,
                                                scale = 6.492272))

  inc_samp <- incfn(100000)
  # 30, 1.95, 0.7

  tab <- data.table::rbindlist(list(data.table(samp = inf_fn(inc_samp, 30), k = 30),
                        data.table(samp = inf_fn(inc_samp, 1.95), k = 1.95),
                        data.table(samp = inf_fn(inc_samp, 0.7), k = 0.7)))

  tab <- tab %>% dplyr::mutate(theta = factor(k,
                                              levels = c(30, 1.95, 0.7),
                                     labels = c("<1%", "15%", "30%")))

  tab_sm <- tab %>%
    dplyr::group_by(theta) %>%
    dplyr::summarise(mean = mean(samp))

  tab %>%
    ggplot2::ggplot(aes(x = samp,
                        fill = as.factor(theta),
                        col = as.factor(theta))) +
    ggplot2::geom_density(alpha = 0.2) +
    ggplot2::theme_bw() +
    ggplot2::geom_vline(data = tab_sm,
                        aes(xintercept = mean,
                            col = as.factor(theta)),
                        lty = 2) +
    ggplot2::scale_x_continuous(breaks = seq(0, 20, 2)) +
    ggplot2::coord_cartesian(xlim = c(0, 20)) +
    ggplot2::xlab("Days since exposure") +
    ggplot2::ylab("Probability density") +
    ggplot2::scale_fill_discrete(name = "Proportion of transmission that occurs before symptom onset") +
    ggplot2::scale_color_discrete(guide = "none") +
    ggplot2::theme(legend.position = "bottom")

}

#' Plots the impact of dispersion on the distribution of new cases
#' @author Sam Abbott
#'
#' @return
#' @export
#' @importFrom tidyr gather unnest
#' @importFrom dplyr mutate filter
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot geom_ribbon theme_bw theme xlab ylab geom_line geom_vline scale_colour_brewer scale_fill_brewer
#' @examples
#'
make_figure_S7 <- function() {

  disp_sars <- function(x, r0) {
    dnbinom(x, size = 0.16, mu = r0)
  }
  disp_diff <- function(x, r0) {
    dnbinom(x, size = 2, mu = r0)
  }

  out <- tibble::tibble(r0 = c(1.5, 2.5, 3.5),
                        x = list(seq(0, 60, 1))) %>%
    dplyr::mutate(sars_samples = purrr::map2(r0, x, ~ disp_sars(x[[1]], .)),
                  diff_samples = purrr::map2(r0, x, ~ disp_diff(x[[1]], .))) %>%
    tidyr::gather(key = "disp", value = "samples", -r0, -x) %>%
    tidyr::unnest(c("samples", "x")) %>%
    dplyr::mutate(disp = disp %>%
                    factor(levels = c("sars_samples", "diff_samples"),
                           labels = c("SARS-like (Dispersion = 0.16)", "Flu-like (Dispersion = 2)")))



  out %>%
    dplyr::filter(x < 30) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = samples, ymin = 0,
                                 ymax = samples, fill = disp)) +
    ggplot2::facet_wrap(~ r0) +
    ggplot2::geom_ribbon(alpha = 0.4) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   axis.text = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_text(size = 12),
                   legend.text = ggplot2::element_text(size = 10)) +
    ggplot2::xlab("Secondary cases per infectious case") +
    ggplot2::scale_fill_brewer(name = "", palette = "Set1") +
    ggplot2::ylab("Probability density") +
    ggplot2::scale_colour_brewer(guide = "none", palette = "Set1")

}
