# load relevant libraries
library(tidyverse)
library(data.table)
library(tictoc)
library(foreach)
library(doParallel)

# make sure all relevant functions are loaded
source("simulation_fun_tiered_orgs_datatable.R")
source("simulation_fun_plotting.R")

# set number of cores for multithreading
n_cores <- detectCores() - 1 #leaves one for other system processes


# get effects - the agent preferences data ------------------------------------------------------------------------

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

#set effects as a data table
setDT(effects)


# simulation functions ----------------------------------------------------------------------------------------

# meta data for the models runs
#n_iterations <- 100
n_models     <- 100

# Run a single simulation
# @exact_prop = that the stated proportions are exact
# @agent_preferences = TRUE = use the 'effects' data above for the agent preferences. If FALSE, choice is random.
run_sim <- function(org_structure = c(1,3,9), n_orgs = 50, prop_a_t1, prop_a_t2, prop_a_t3, 
                    prop_b_t1, prop_b_t2, prop_b_t3, prop_empty = 0.1, 
                    exact_prop = TRUE, agent_preferences = TRUE, n_iterations = 100) {
  
  # initialize agents
  agents <- initialize_agents_tiered(org_structure, n_orgs, prop_a_t1, prop_a_t2, prop_a_t3, 
                                     prop_b_t1, prop_b_t2, prop_b_t3, prop_empty, exact_prop) %>%
    select(-agent)
  
  #set agents as a data table
  setDT(agents)
  
  # initialize outcome (dissimilarity) table
  outcome_df <- data.frame(dis_overall   = numeric(n_iterations),
                           dis_tier2     = numeric(n_iterations),
                           dis_tier3     = numeric(n_iterations),
                           theil_overall = numeric(n_iterations))
  
  # fill dissimilarity table with initial values
  outcome_df[1,]$dis_overall <- calc_dissimilarity(agents)
  outcome_df[1,]$dis_tier2 <- calc_dissimilarity(agents, calc_tier = 2)
  outcome_df[1,]$dis_tier3 <- calc_dissimilarity(agents, calc_tier = 3)
  
  #print(str_c("Iteration ", 1, " | Dissimilaity: ", outcome_df[1,]$dis_overall))
  
  # run the model iterations
  for (i in 2:n_iterations ) {
    # calculate agent movement for a single itteration
    tic(str_c("=============== ITERATION ", i))
    agents <- move_agents(agents, 3, have_prerefernce = agent_preferences) ### HERE YOU MAKE SURE PREF = TRUE. FALSE WILL BE RANDOM CHOICE
    
    # calculate outcomes
    outcome_df[i,]$dis_overall <- calc_dissimilarity(agents)
    outcome_df[i,]$dis_tier2 <- calc_dissimilarity(agents, calc_tier = 2)
    outcome_df[i,]$dis_tier3 <- calc_dissimilarity(agents, calc_tier = 3)
  
    outcome_df[i,]$theil_overall <- calc_theil(agents)
  
    toc()
  }
  
  # return the outcome - the dissimilarity data
  return(outcome_df)
}



# Run multiple models (n_sims) with the same agent A and B proportions.
# This function is parallelized
run_sim_batch <- function(n_sims, prop_a, prop_b, use_preferences = TRUE) {
 
  print(str_c("====================== started sim batch with ", n_sims, " simulations"))
  print(timestamp())

  # Register parallel backend
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  
  # run batch
  simulations <- foreach(i = 1:n_sims, 
          .packages = c("stringr", "tidyverse", "data.table", "tictoc"), 
          .export = c("run_sim", "initialize_agents_tiered", "calc_dissimilarity", "calc_theil", "calc_entropy", 
                      "choose_position", "get_rel_hierarchy", "move_agent", "move_agents", "effects")) %dopar% 
    {

      out <- run_sim(org_structure = c(1,3,9), n_orgs =  100, prop_a_t1 = prop_a, prop_a_t2 = prop_a, prop_a_t3 = prop_a, 
                     prop_b_t1 = prop_b, prop_b_t2 = prop_b, prop_b_t3 = prop_b, 
                     prop_empty = 0.05, exact_prop = TRUE, agent_preferences = use_preferences)
      
      out
  }

  #stop multi threading
  stopCluster(cl)
  
  print("finished batch")
  print(timestamp())
  
  return(simulations)  
}


# Actually run the simulations ------------------------------------------------------------------------------------

# 50 orgs, 100 iterations, 100 simulations
output_empty <- run_sim_batch(100, 0.61, 0.39, FALSE)
output_61 <- run_sim_batch(100, 0.61, 0.39, TRUE)
output_40 <- run_sim_batch(100, 0.4, 0.6, TRUE)
output_50 <- run_sim_batch(100, 0.5, 0.5, TRUE)

write_rds(output_empty, "out/output61_nopref.rds")
write_rds(output_61, "out/output61.rds")
write_rds(output_40, "out/output40.rds")
write_rds(output_50, "out/output50.rds")

# 100 orgs, 100 iterations, 100 simulations
output100_empty <- run_sim_batch(100, 0.61, 0.39, FALSE)
output100_61 <- run_sim_batch(100, 0.61, 0.39, TRUE)
output100_40 <- run_sim_batch(100, 0.4, 0.6, TRUE)
output100_50 <- run_sim_batch(100, 0.5, 0.5, TRUE)

write_rds(output100_empty, "out/100org_output61_nopref.rds")
write_rds(output100_61, "out/100org_output61.rds")
write_rds(output100_40, "out/100org_output40.rds")
write_rds(output100_50, "out/100org_output50.rds")

# 
plot_sim_batch(output_empty, "The ratio between agents A/B is 61/39. Base model (no pref.)")
plot_sim_batch(output_61, "The ratio between agents A/B is 61/39.")
plot_sim_batch(output_40, "The ratio between agents A/B is 40/60.")
plot_sim_batch(output_50, "The ratio between agents A/B is 50/50.")

plot_sim_batch(output100_empty, "The ratio between agents A/B is 61/39. Base model (no pref.)")
plot_sim_batch(output100_61, "The ratio between agents A/B is 61/39.")
plot_sim_batch(output100_40, "The ratio between agents A/B is 40/60.")
plot_sim_batch(output100_50, "The ratio between agents A/B is 50/50.")

plot_sim_batch_theil(output100_empty, "The ratio between agents A/B is 61/39. Base model (no pref.)")
plot_sim_batch_theil(output100_61, "The ratio between agents A/B is 61/39.")
plot_sim_batch_theil(output100_40, "The ratio between agents A/B is 40/60.")
plot_sim_batch_theil(output100_50, "The ratio between agents A/B is 50/50.")

