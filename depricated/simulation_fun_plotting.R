library(ggplot2)
library(geomtextpath)


plot_sim_batch <- function(batch, agent_ratio_str) {
  
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
  
  ggplot(combined_df, aes(x = iteration, y = dissimilarity_total)) + 
    geom_line(aes(y = dis_overall, group = sim_id, color = dissimilarity_increased), alpha = 0.45) +
    scale_color_manual(values = c("d_increased" = "steelblue4", "d_decreased" = "tomato")) + 
    geom_line(linewidth = 2, alpha = 0.75) +
    geom_labelpath(linewidth = 1.5,
                  label = str_c("MEAN CHANGE: ", diss_diff), 
                  stat = "smooth",
                  color = "violetred4",
                  family = "Roboto",
                  size = 3.5,
                  hjust = 0.8) +
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
}


plot_sim_batch_theil <- function(batch, agent_ratio_str) {
  
  combined_df <- map2_df(batch, seq_along(batch), 
                         ~ .x %>%
                           mutate(sim_id = .y, iteration = row_number()))  %>% 
    group_by(iteration) %>% 
    mutate(theil_total = mean(theil_overall)) %>%
    ungroup() %>%
    filter(iteration > 1) %>%
    group_by(sim_id) %>%
    mutate(theil_increased = as.factor(if_else(first(theil_overall) < last(theil_overall), "h_increased", "h_decreased")))
  
  #calculate mean initial (t0) and final (t1) theil
  theil_t0 <- mean(filter(combined_df, iteration == 2)$theil_total)
  theil_t1 <- mean(filter(combined_df, iteration == max(iteration))$theil_total)
  #calculate the % difference, round to 2 decimals, and add a '+' sign if the number is positive
  theil_diff <- str_replace(str_c(round(100*((theil_t1-theil_t0)/theil_t0), 2), "%"), "^(?=[0-9])", "+")
  
  #DO THE PLOTTING! :)
  ggplot(combined_df, aes(x = iteration, y = theil_total)) + 
    geom_line(aes(y = theil_overall, group = sim_id, color = theil_increased), alpha = 0.45) +
    scale_color_manual(values = c("h_increased" = "steelblue4", "h_decreased" = "tomato")) + 
    geom_line(linewidth = 2, alpha = 0.75) +
    geom_labelpath(linewidth = 1.5,
                   label = str_c("MEAN CHANGE: ", theil_diff), 
                   stat = "smooth",
                   color = "violetred4",
                   family = "Roboto",
                   size = 3.5,
                   hjust = 0.8) +
    labs(x = "Iteration", y = "Theil index", 
         title = "Theil over time",
         subtitle = str_c("Change in segregation between 100 organizations time with an agent (A/B) ratio of ", agent_ratio_str,
                          "\nThese are the results of ", length(batch), " simulations.")) +
    theme_minimal() + 
    theme(legend.position = "none",
          text = element_text(family = "Roboto"),
          plot.title = element_text(face = "bold",
                                    size = rel(1.5)),
          axis.text = element_text(family = "Roboto")) + 
    ylim(0.025, 0.125)
}
