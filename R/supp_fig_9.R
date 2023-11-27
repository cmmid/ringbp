#' @export
make_figure_S9 <- function(sweep_results = NULL){
  temp <- sweep_results %>% filter(index_R0 == 1.5,
                                   theta=="15%",
                                   delay == "SARS",
                                   prop.asym == 0,
                                   num.initial.cases==20,
                                   control_effectiveness == 0.8)


  ext_df <- temp$sims[[1]] %>% group_by(sim) %>%
    dplyr::filter(week %in% 12:16) %>%
    dplyr::summarise(extinct =
                       ifelse(all(weekly_cases == 0 &
                                    cumulative < 5000),
                              1, 0)) %>%
    dplyr::ungroup()

  p1 <- full_join(temp$sims[[1]],ext_df) %>%
    filter(sim < 50) %>%
    ggplot(aes(x=week,y=cumulative,col=as.factor(extinct),group=sim)) + geom_line(alpha=0.6) +
    cowplot::theme_cowplot() + coord_cartesian(ylim=c(0,1000),xlim=c(0,16)) +
    scale_color_manual(name="Simulation controlled",values=c("dodgerblue1"),guide="none") +
    ylab("Cumulative cases") +
    ggtitle("R0 = 1.5") +
    xlab("Outbreak week")


  temp <- sweep_results %>% filter(index_R0 == 2.5,
                                   theta=="15%",
                                   delay == "SARS",
                                   prop.asym == 0,
                                   num.initial.cases==20,
                                   control_effectiveness == 0.8)

  ext_df <- temp$sims[[1]] %>% group_by(sim) %>%
    dplyr::filter(week %in% 12:16) %>%
    dplyr::summarise(extinct =
                       ifelse(all(weekly_cases == 0 &
                                    cumulative < 5000),
                              1, 0)) %>%
    dplyr::ungroup()

  p2 <- full_join(temp$sims[[1]],ext_df) %>%
    filter(sim < 50) %>%
    ggplot(aes(x=week,y=cumulative,col=as.factor(extinct),group=sim)) + geom_line(alpha=0.6) +
    cowplot::theme_cowplot() + coord_cartesian(ylim=c(0,1000),xlim=c(0,16)) +
    scale_color_manual(name="Simulation controlled",values=c("firebrick1","dodgerblue1"),labels=c("Uncontrolled","Controlled")) +
    ggtitle("R0 = 2.5") +
    ylab("Cumulative cases") +
    xlab("Outbreak week")

  temp <- sweep_results %>% filter(index_R0 == 3.5,
                                   theta=="15%",
                                   delay == "SARS",
                                   prop.asym == 0,
                                   num.initial.cases==20,
                                   control_effectiveness == 0.8)

  ext_df <- temp$sims[[1]] %>% group_by(sim) %>%
    dplyr::filter(week %in% 12:16) %>%
    dplyr::summarise(extinct =
                       ifelse(all(weekly_cases == 0 &
                                    cumulative < 5000),
                              1, 0)) %>%
    dplyr::ungroup()

  p3 <- full_join(temp$sims[[1]],ext_df) %>%
    filter(sim < 50) %>%
    ggplot(aes(x=week,y=cumulative,col=as.factor(extinct),group=sim)) + geom_line(alpha=0.6) +
    cowplot::theme_cowplot() + coord_cartesian(ylim=c(0,1000),xlim=c(0,16)) +
    scale_color_manual(name="Simulation controlled",values=c("firebrick1","dodgerblue1"),labels=c("Uncontrolled","Controlled")) +
    ylab("Cumulative cases") +
    ggtitle("R0 = 3.5") +
    xlab("Outbreak week")
  p3
  p1 + p2 + p3 + patchwork::plot_layout(guides="collect")
}

