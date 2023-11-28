
# Packages ----------------------------------------------------------------

library(ringbp.ncov)
library(ringbp)
library(ggplot2)
library(dplyr)
library(patchwork)
library(cowplot)

dir.create("inst/plots", showWarnings = FALSE)

# Figure 1 ----------------------------------------------------------------

## Generated in inkscape

# Figure 2 ----------------------------------------------------------------

fig_2 <- make_figure_2()

ggsave("inst/plots/fig_2.pdf", fig_2, height = 5.5, width = 10,
                useDingbats = FALSE)
ggsave("inst/plots/fig_2.png", fig_2, height = 5.5, width = 10)

# Load in results  -------------------------------------------------------

sweep_results <- readRDS("data-raw/res.rds")

res <- sweep_results %>%
  group_by(scenario) %>%
  mutate(pext = extinct_prob(sims[[1]], cap_cases = 5000)) %>%
  ungroup(scenario)


# Figure 3 ----------------------------------------------------------------

fig_3_a <- make_figure_3a(df = res)
fig_3_b <- make_figure_3b(df = res) + labs(tag = "B")

fig_3 <- fig_3_a + fig_3_b + patchwork::plot_layout(guides = "keep")

ggsave("inst/plots/fig_3.pdf", fig_3, height = 5, width = 8,
                useDingbats = FALSE)
ggsave("inst/plots/fig_3.png", fig_3, height = 5, width = 8)

# Figure 4 ----------------------------------------------------------------


fig_4 <- make_figure_4(res = res)

ggsave("inst/plots/fig_4.pdf", fig_4, height = 5, width = 9,
                useDingbats = FALSE)
ggsave("inst/plots/fig_4.png", fig_4, height = 5, width = 9)


# Figure 5 ----------------------------------------------------------------

fig_5 <- box_plot_max_weekly_cases(results = sweep_results, cap_cases = 5000,
                                  extinct_thresold = 0.1,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 20, flip_coords = FALSE,
                                  facet_scales = "fixed", record_params = F,
                                  prop_asym = 0, y_lim = 125)

ggsave("inst/plots/fig_5.pdf", fig_5, height = 7, width = 12,
                useDingbats = FALSE)
ggsave("inst/plots/fig_5.png", fig_5, height = 7, width = 12)

# Supplementary figures ---------------------------------------------------

## S1

fig_s1 <- make_figure_S1(res)

ggsave("inst/plots/S_fig_1.pdf", fig_s1, height = 7.5, width = 9,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_1.png", fig_s1, height = 7.5, width = 9)
## S2

fig_s2 <- make_figure_S2(res)

ggsave("inst/plots/S_fig_2.pdf", fig_s2, height = 7, width = 9,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_2.png", fig_s2, height = 7, width = 9)
## S3

fig_s3 <- make_figure_S3(res)


ggsave("inst/plots/S_fig_3.pdf", fig_s3, height = 5.5, width = 9,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_3.png", fig_s3, height = 5.5, width = 9)

## S4

fig_s4 <- make_figure_S4(res)

ggsave("inst/plots/S_fig_4.pdf", fig_s4, height = 3, width = 6.5,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_4.png", height = 3, width = 6.5)

## S5

fig_s5 <- box_plot_max_weekly_cases(results = sweep_results,
                                  cap_cases = 5000,
                                  extinct_thresold = 0.05,
                                  prop_asym = 0,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 5,
                                  facet_scales = "fixed",
                                  record_params = F,
                                  y_lim = 100)

ggsave("inst/plots/S_fig_5_A.pdf", fig_s5, height = 5, width = 10,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_5_A.png", fig_s5, height = 5, width = 10)

fig_s5b <- box_plot_max_weekly_cases(results = sweep_results,
                                  cap_cases = 5000,
                                  extinct_thresold = 0.05,
                                  prop_asym = 0,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 40,
                                  facet_scales = "fixed",
                                  record_params = F,
                                  y_lim = 200)

ggsave("inst/plots/S_fig_5_B.pdf", fig_s5b, height = 5, width = 10,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_5_B.png", fig_s5b, height = 5, width = 10)

fig_s5c <- box_plot_max_weekly_cases(results = sweep_results,
                                  cap_cases = 5000,
                                  extinct_thresold = 0.05,
                                  prop_asym = 0.1,
                                  filt_control_effectiveness = 0.4,
                                  num_initial_cases = 20,
                                  facet_scales = "fixed",
                                  record_params = F,
                                  y_lim = 100)

ggsave("inst/plots/S_fig_5_C.pdf", fig_s5c, height = 5, width = 10,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_5_C.png", fig_s5c, height = 5, width = 10)

## S6

fig_s6 <- make_figure_S6()

ggsave("inst/plots/S_fig_6.pdf", fig_s6, height = 8, width = 12,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_6.png", fig_s6, height = 8, width = 12)

## Get data for supplement looking at flu like dispersion

results_dispersion_flu <- readRDS("data-raw/res_dispersion_flu.rds")

res_flu <- results_dispersion_flu  %>%
  group_by(scenario) %>%
  mutate(pext = extinct_prob(sims[[1]], cap_cases = 5000)) %>%
  ungroup(scenario)

## S7

fig_s7a <- make_figure_S7()
ggsave("inst/plots/S_fig_7b.pdf", fig_s7a, height = 4, width = 6,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_7b.png", fig_s7a, height = 4, width = 6)


# remaking fig 3 with flu dispersion

fig_s7b <- make_figure_3a(df = res_flu)
ggsave("inst/plots/S_fig_7b.pdf", fig_s7b, height = 4, width = 6,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_7b.png", fig_s7b, height = 4, width = 6)



# remaking fig 4 with flu dispersion
fig_s7c <- make_figure_4(res = res_flu)
ggsave("inst/plots/S_fig_7c.pdf", fig_s7c, height = 5, width = 7,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_7c.png", fig_s7c, height = 5, width = 7)

fig_s9 <- make_figure_S9(sweep_results = res)
ggsave("inst/plots/S_fig_9.pdf", fig_s9, height = 4.5, width = 11,
                useDingbats = FALSE)
ggsave("inst/plots/S_fig_9.png", fig_s9, height = 4.5, width = 11)

