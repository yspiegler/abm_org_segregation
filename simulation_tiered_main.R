#effects <- read_csv(file = "conjoint_effects.csv")
effects <- data.frame(composition = c("just_b", "just_a", "just_b", 
                                      "just_a", "mix", "just_b", "just_a", "mix", "just_b", "just_a", 
                                      "just_b", "just_a", "mix", "just_b", "just_a", "mix"), 
                      estimate = c(0.5842148, 0.4126582, 0.5803922, 0.3997613, 0.5277435, 0.542955, 0.3633218, 
                                   0.54293535, 0.5431255, 0.4587755, 0.4496815, 0.4845869, 0.545, 
                                   0.3510638, 0.5211506, 0.55849065), 
                      tier = c(1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 2, 2, 2, 3, 3, 3), 
                      race = c("black", "black", "black", "black", "black", "black", "black", "black", "white", "white", 
                               "white", "white", "white", "white", "white", "white"), 
                      type = c("B", "B", "B", "B", "B", "B", "B", "B", "A", "A", "A", "A", "A", "A", "A", "A"))

effects <- data.frame(composition = c("just_b", "just_a", "just_b", 
                                      "just_a", "mix", "just_b", "just_a", "mix", "just_b", "just_a", 
                                      "just_b", "just_a", "mix", "just_b", "just_a", "mix"), 
                      estimate = c(0.5842148, 0.4126582, 0.5803922, 0.3997613, 0.5277435, 0.542955, 0.3633218, 
                                   0.54293535, 0.5431255, 0.4587755, 0.4496815, 0.4845869, 0.545, 
                                   0.3510638, 0.5211506, 0.55849065), 
                      tier = c(1, 1, 2, 2, 2, 3, 3, 3, 1, 1, 2, 2, 2, 3, 3, 3), 
                      race = c("black", "black", "black", "black", "black", "black", "black", "black", "white", "white", 
                               "white", "white", "white", "white", "white", "white"), 
                      type = c("B", "B", "B", "B", "B", "B", "B", "B", "A", "A", "A", "A", "A", "A", "A", "A"))

set.seed(1000)

n_iterations <- 150

agents <- initialize_agents_tiered(c(1,3,9), 10, 0.6, 0.6, 0.6, 0.4, 0.4, 0.4, 0.1, TRUE) %>%
  select(-agent)

dissimilarity_vector <- numeric(n_iterations )
dissimilarity_vector[1] <- calc_dissimilarity(agents)
print(str_c("Iteration ", 1, " | Dissimilaity: ", dissimilarity_vector[1]))

for (i in 2:n_iterations ) {
  agents <- move_agents(agents, 3)
  dissimilarity_vector[i] <- calc_dissimilarity(agents)
  print(str_c("Iteration ", i, " | Dissimilaity: ", dissimilarity_vector[i]))
}
