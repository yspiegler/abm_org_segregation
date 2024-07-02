library(readr)


# Source all relevant R files -------------------------------------------------------------------------------------

source("src/initialize_agents.R")
source("src/run_simulations.R")
source("src/agent_movement.R")
source("src/plotting_functions.R")
source("src/calculate_segregation.R")



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


# Plot simulation outcomes ----------------------------------------------------------------------------------------

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
