library(ggplot2)
library(ggtext)


######################################################
######################################################
#  NEED TO FIX / STREAMLINE TEXT FONT SIZES IN PLOTS #
######################################################
######################################################

# plot simulation batch of any number as the change in Dissimilarity segregation score as a function of sim iterations
plot_batch_dissimilarity <- function(batch, agent_ratio_str) {
  
  combined_df <- map2_df(batch, seq_along(batch), 
                         ~ .x %>%
                           mutate(sim_id = .y, iteration = row_number()))  %>% 
    group_by(iteration) %>% 
    mutate(dissimilarity_total = mean(dis_overall),
           dissimilarity_tier2 = mean(dis_tier2),
           dissimilarity_tier3 = mean(dis_tier3),
           theil_total         = mean(theil_overall)) %>%
    ungroup() %>%
    group_by(sim_id) %>%
    mutate(dissimilarity_increased = as.factor(if_else(first(dis_overall) < last(dis_overall), "d_increased", "d_decreased")))
  
  diss_t0 <- mean(filter(combined_df, iteration == 1)$dissimilarity_total)
  diss_t1 <- mean(filter(combined_df, iteration == max(iteration))$dissimilarity_total)
  diss_diff <- str_replace(str_c(round(100*((diss_t1-diss_t0)/diss_t0), 2), "%"), "^(?=[0-9])", "+")
  
  p <-  ggplot(combined_df, aes(x = iteration, y = dissimilarity_total)) + 
            geom_line(aes(y = dis_overall, group = sim_id, color = dissimilarity_increased), alpha = 0.45) +
            scale_color_manual(values = c("d_increased" = "steelblue4", "d_decreased" = "tomato")) + 
            #geom_line(linewidth = 2, alpha = 0.75) +
            geom_textpath(linewidth = 1.5,
                           label = str_c("mean change: ", diss_diff), 
                           stat = "smooth",
                           color = "black",
                           family = "Roboto",
                           size = 4,
                           hjust = 0.25) +
            labs(x = "Iteration", y = "Dissimilarity", 
                 title = "Dissimilarity over time",
                 subtitle = str_c("Change in segregation between 100 organizations time with an agent (A/B) ratio of ", agent_ratio_str,
                                  "\nThese are the results of ", length(batch), " simulations.")) +
            theme_minimal() + 
            theme(legend.position = "none",
                  text = element_text(family = "Roboto"),
                  plot.title = element_text(face = "bold",
                                            size = rel(1.5)),
                  axis.text = element_text(family = "Roboto")) + 
                    ylim(0.15, 0.35)
  
  return(p)
}

# plot simulation batch of any number as the change in Theil segregation score as a function of sim iterations 
plot_batch_theil <- function(batch, agent_ratio_str, preferences = TRUE, plot_all_sims = TRUE) {
  
  combined_df <- map2_df(batch, seq_along(batch), 
                         ~ .x %>%
                           mutate(sim_id = .y, iteration = row_number()))  %>% 
    group_by(iteration) %>% 
    mutate(theil_total = mean(theil_overall)) %>%
    ungroup() %>%
    filter(iteration > 1) %>%
    group_by(sim_id) %>%
    mutate(theil_increased = as.factor(if_else(first(theil_overall) < 
                                                last(theil_overall), "h_increased", "h_decreased")))
  
  #calculate mean initial (t0) and final (t1) theil
  theil_t0 <- mean(filter(combined_df, iteration == 2)$theil_total)
  theil_t1 <- mean(filter(combined_df, iteration == max(iteration))$theil_total)
  #calculate the % difference, round to 2 decimals, and add a '+' sign if the number is positive
  theil_diff <- str_replace(str_c(round(100*((theil_t1-theil_t0)/theil_t0), 2), "%"), "^(?=[0-9])", "+")
  
  pct_up <- mean(combined_df$theil_increased == "h_increased") * 100
  pct_down <- 100 - pct_up
  
  if(preferences) {
    pref_txt <- "based on racial composition preferences"  
  } else {
    pref_txt <- "NULL models (no compositional preferences)"
  }
  
  
  subtitle_str <- str_c(
    "Based on ", length(batch), " simulations, each of ", nrow(batch[[1]]), 
    " organizations with a ratio of ", agent_ratio_str, " White/Black agents.<br>",
    "<span style='color:steelblue;'>**", pct_up, "% of simulations**", 
    "</span> increased in segregation, while <span style='color:tomato;'>**",  
    pct_down, "% of simulations</span> decreased**.<br>",
    "**Overall, the mean change in segregation is ", theil_diff, "**.</span>")
  
  #DO THE PLOTTING! :)
  p <- ggplot(combined_df, aes(x = iteration, y = theil_overall))
  
  if(plot_all_sims) {
    p <- p +
      geom_line(aes(y = theil_overall, group = sim_id, color = theil_increased), alpha = 0.35) +
      scale_color_manual(values = c("h_increased" = "steelblue4", "h_decreased" = "tomato"))
  } 
  p <- p +  #stat_summary(geom = "smooth", 
            #             fun.data = mean_cl_normal,
            #             fun.args = list(conf.int = 0.95),
            #             group = 1,
            #             alpha = .5,
            #             color = "black",
            #             fill = "darkgrey",
            #             se = TRUE) +
            geom_smooth() + 
            labs(x = "Iteration", y = "Theil index", 
                 title = str_c("Between-organization segregation (Theil) change over time, ", pref_txt),
                 subtitle = subtitle_str) + 
            theme_minimal() 
  
  return(p)
}


# Saving for print and presentations - NEED WORK ------------------------------------------------------------------

save_plot_print <- function(plot, name) {
  plot <- plot +     
theme(legend.position = "none",
      text = element_text(family = "Roboto"),
      plot.title = element_text(face = "bold",
                                size = 20,
                                family = "Roboto"),
      plot.subtitle = element_markdown(size = 16, family = "Roboto"),
      axis.text = element_text(family = "Roboto", size = 14),
      axis.title = element_text(size = 16))
  
  ggsave(plot,
         filename = str_c("plots/", name, ".png"),
         device = "png", dpi = 300, units = "in",
         height = 8.5,
         width = 14)
}

save_plot_ppt <- function(plot) {
  plot <- plot +     
    theme(legend.position = "none",
          text = element_text(family = "Roboto"),
          plot.title = element_text(face = "bold",
                                    size = rel(1.5)),
          axis.text = element_text(family = "Roboto"))
  
  ggsave()
}