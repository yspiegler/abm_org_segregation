# load relevant libraries
library(tidyverse)
library(microbenchmark)

# make sure all relevant functions are loaded
source("simulation_fun_tiered_orgs.R")

#effects <- read_csv(file = "conjoint_effects.csv")
effects <- data.frame(composition = c("just_b", "just_a", "just_b", "just_a", "mix", "just_b", "just_a", "mix", 
                                      "just_b", "just_a", "just_b", "just_a", "mix", "just_b", "just_a", "mix"), 
                      estimate = c(0.5842148, 0.4126582, 0.5803922, 0.3997613, 0.5277435, 0.542955, 0.3633218, 0.54293535, 
                                   0.5431255, 0.4587755, 0.4496815, 0.4845869, 0.545, 0.3510638, 0.5211506, 0.55849065), 
                      tier = c(1, 1, 2, 2, 2, 3, 3, 3, 
                               1, 1, 2, 2, 2, 3, 3, 3), 
                      race = c("black", "black", "black", "black", "black", "black", "black", "black", 
                               "white", "white", "white", "white", "white", "white", "white", "white"), 
                      type = c("B", "B", "B", "B", "B", "B", "B", "B", 
                               "A", "A", "A", "A", "A", "A", "A", "A"))


# meta data for the models runs
n_iterations <- 200
n_models     <- 50


# Run simulation functions ----------------------------------------------------------------------------------------

# Run a single simulation
# @exact_prop = that the stated proportions are exact
# @agent_preferences = TRUE = use the 'effects' data above for the agent preferences. If FALSE, choice is random.
run_sim <- function(org_structure = c(1,3,9), n_orgs = 50, prop_a_t1, prop_a_t2, prop_a_t3, 
                    prop_b_t1, prop_b_t2, prop_b_t3, prop_empty = 0.1, exact_prop = TRUE, agent_preferences = TRUE) {
  
  # initialize agents
  agents <- initialize_agents_tiered(org_structure, n_orgs, prop_a_t1, prop_a_t2, prop_a_t3, 
                                     prop_b_t1, prop_b_t2, prop_b_t3, prop_empty, exact_prop) %>%
    select(-agent)
  
  # initialize outcome (dissimilarity) table
  dissimilarity_df <- data.frame(dis_overall = numeric(n_iterations),
                                 dis_tier2   = numeric(n_iterations),
                                 dis_tier3   = numeric(n_iterations))
  
  # fill dissimilarity table with initial values
  dissimilarity_df[1,]$dis_overall <- calc_dissimilarity(agents)
  dissimilarity_df[1,]$dis_tier2 <- calc_dissimilarity(agents, calc_tier = 2)
  dissimilarity_df[1,]$dis_tier3 <- calc_dissimilarity(agents, calc_tier = 3)
  
  #print(str_c("Iteration ", 1, " | Dissimilaity: ", dissimilarity_df[1,]$dis_overall))
  
  # run the model iterations
  for (i in 2:n_iterations ) {
    agents <- move_agents(agents, 3, have_prerefernce = agent_preferences) ### HERE YOU MAKE SURE PREF = TRUE. FALSE WILL BE RANDOM CHOICE
    
    dissimilarity_df[i,]$dis_overall <- calc_dissimilarity(agents)
    dissimilarity_df[i,]$dis_tier2 <- calc_dissimilarity(agents, calc_tier = 2)
    dissimilarity_df[i,]$dis_tier3 <- calc_dissimilarity(agents, calc_tier = 3)
    
   # print(str_c("Iteration ", i, " | Dissimilaity: ", dissimilarity_df[1,]$dis_overall))
  }
  
  # return the outcome - the dissimilarity data
  return(dissimilarity_df)
}


run_sim_batch <- function(n_sims, prop_a, prop_b) {
  # prepare outcome df
  sim_outcomes <- list()
  
  # run batch
  for (i in 1:n_sims) {
    print(str_c("======================\n started sim ", i, timestamp()))
    out <- run_sim(org_structure = c(1,3,9), n_orgs =  50, prop_a_t1 = prop_a, prop_a_t2 = prop_a, prop_a_t3 = prop_a, 
                   prop_b_t1 = prop_b, prop_b_t2 = prop_b, prop_b_t3 = prop_b, 
                   prop_empty = 0.1, exact_prop = TRUE, agent_preferences = TRUE)
    print(str_c("======================\n finishied with sim ", i, "\n======================"))
    
    #sim_outcomes[i]$model_num <- i
    #sim_outcomes[i]$prop_a <- prop_a
    #sim_outcomes[i]$prop_b <- prop_b
    sim_outcomes[[i]] <- out
  }

  return(sim_outcomes)  
}


# Actually run the simulations ------------------------------------------------------------------------------------


output_61_39 <- run_sim_batch(20, 0.61, 0.39)
output_50_50 <- run_sim_batch(20, 0.5, 0.5)
output_40_60 <- run_sim_batch(20, 0.4, 0.6)
output_nopref <- run_sim_batch(20, 0.61, 0.39)

output_61_39_big <- run_sim_batch(n_sims = 50, 0.61, 0.39)

