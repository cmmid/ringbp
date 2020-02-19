
# Packages ----------------------------------------------------------------

library(ringbp)
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)

# Figure 1 ----------------------------------------------------------------

## Generated in inkscape

# Figure 2 ----------------------------------------------------------------

ringbp::make_figure_2()

ggplot2::ggsave("inst/plots/fig_2.pdf", height = 5.5, width = 10,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/fig_2.png", height = 5.5, width = 10)

# Load in results  -------------------------------------------------------

sweep_results <- readRDS("data-raw/res.rds")

res <- sweep_results %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = 5000)) %>%
  dplyr::ungroup(scenario)


# Figure 3 ----------------------------------------------------------------

fig_3_a <- make_figure_3a(df = res)
fig_3_b <- make_figure3b(df = res) + ggplot2::labs(tag = "B")

fig_3_a + fig_3_b + patchwork::plot_layout(guides = "keep")

ggplot2::ggsave("inst/plots/fig_3.pdf", height = 5, width = 8,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/fig_3.png", height = 5, width = 8)

# Figure 4 ----------------------------------------------------------------


make_figure_4(res = res)

ggplot2::ggsave("inst/plots/fig_4.pdf", height = 5, width = 9,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/fig_4.png", height = 5, width = 9)


# Figure 5 ----------------------------------------------------------------

ringbp::box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000,
                                  extinct_thresold = 0.1,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 20, flip_coords = FALSE,
                                  facet_scales = "fixed", record_params = F,
                                  prop_asym = 0, y_lim = 125)

ggplot2::ggsave("inst/plots/fig_5.pdf", height = 7, width = 12,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/fig_5.png", height = 7, width = 12)

# Supplementary figures ---------------------------------------------------

## S1

make_figure_S1(res)

ggplot2::ggsave("inst/plots/S_fig_1.pdf", height = 7.5, width = 9,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_1.png", height = 7.5, width = 9)
## S2

make_figure_S2(res)

ggplot2::ggsave("inst/plots/S_fig_2.pdf", height = 7, width = 9,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_2.png", height = 7, width = 9)
## S3

make_figure_S3(res)


ggplot2::ggsave("inst/plots/S_fig_3.pdf", height = 5.5, width = 9,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_3.png", height = 5.5, width = 9)

## S4

make_figure_S4(res)

ggplot2::ggsave("inst/plots/S_fig_4.pdf", height = 3, width = 6.5,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_4.png", height = 3, width = 6.5)

## S5

ringbp::box_plot_max_weekly_cases(results = sweep_results,
                                  cap_cases = 5000,
                                  extinct_thresold = 0.05,
                                  prop_asym = 0,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 5,
                                  facet_scales = "fixed",
                                  record_params = F,
                                  y_lim = 100)

ggplot2::ggsave("inst/plots/S_fig_5_A.pdf", height = 5, width = 10,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_5_A.png", height = 5, width = 10)

ringbp::box_plot_max_weekly_cases(results = sweep_results,
                                  cap_cases = 5000,
                                  extinct_thresold = 0.05,
                                  prop_asym = 0,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 40,
                                  facet_scales = "fixed",
                                  record_params = F,
                                  y_lim = 450)

ggplot2::ggsave("inst/plots/S_fig_5_B.pdf", height = 5, width = 10,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_5_B.png", height = 5, width = 10)

ringbp::box_plot_max_weekly_cases(results = sweep_results,
                                  cap_cases = 5000,
                                  extinct_thresold = 0.05,
                                  prop_asym = 0.1,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 20,
                                  facet_scales = "fixed",
                                  record_params = F,
                                  y_lim = 1000)

ggplot2::ggsave("inst/plots/S_fig_5_C.pdf", height = 5, width = 10,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_5_C.png", height = 5, width = 10)

## S6

ringbp::make_figure_S6()

ggplot2::ggsave("inst/plots/S_fig_6.pdf", height = 8, width = 12,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_6.png", height = 8, width = 12)

## Get data for supplement looking at flu like dispersion

results_dispersion_flu <- readRDS("data-raw/res_dispersion_flu.rds")

res_flu <- results_dispersion_flu  %>%
  dplyr::group_by(scenario) %>%
  dplyr::mutate(pext = extinct_prob(sims[[1]], cap_cases = 5000)) %>%
  dplyr::ungroup(scenario)

## S7

ringbp::make_figure_S7()

# remaking fig 3 with flu dispersion

make_figure_3a(df = res_flu)
ggplot2::ggsave("inst/plots/S_fig_7b.pdf", height = 4, width = 6,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_7b.png", height = 4, width = 6)



# remaking fig 4 with flu dispersion
make_figure_4(res = res_flu)
ggplot2::ggsave("inst/plots/S_fig_7c.pdf", height = 5, width = 7,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_7c.png", height = 5, width = 7)

## S6

ringbp::box_plot_max_weekly_cases(results = results_dispersion_flu,
                                  cap_cases = 5000,
                                  extinct_thresold = 0.05,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 40,
                                  flip_coords = F,
                                  prop_asym = 0,
                                  facet_scales = "fixed",
                                  record_params = F,
                                  y_lim = 400)

ggplot2::ggsave("inst/plots/S_fig_6.pdf", height = 8, width = 12,
                useDingbats = FALSE)
ggplot2::ggsave("inst/plots/S_fig_6.png", height = 8, width = 12)
