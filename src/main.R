library(readr)


# Source all relevant R files -------------------------------------------------------------------------------------

source("src/initialize_agents.R")
source("src/run_simulations.R")
source("src/agent_movement.R")
source("src/plotting_functions.R")
source("src/calculate_segregation.R")

#  Run the simulations ------------------------------------------------------------------------------------

# 50 orgs, 100 iterations, 100 simulations
#output_empty <- run_sim_batch(100, 0.61, 0.39, FALSE)
#output_61 <- run_sim_batch(100, 0.61, 0.39, TRUE)
#output_40 <- run_sim_batch(100, 0.4, 0.6, TRUE)
#output_50 <- run_sim_batch(100, 0.5, 0.5, TRUE)

#write_rds(output_empty, "out/output61_nopref.rds")
#write_rds(output_61, "out/output61.rds")
#write_rds(output_40, "out/output40.rds")
#write_rds(output_50, "out/output50.rds")

# 100 orgs, 100 iterations, 100 simulations
out_nopref <- run_sim_batch(100, 0.61, 0.39, FALSE)
out_61 <- run_sim_batch(100, 0.61, 0.39, TRUE)
out_40 <- run_sim_batch(100, 0.4, 0.6, TRUE)
out_50 <- run_sim_batch(100, 0.5, 0.5, TRUE)

write_rds(out_nopref, "out/100org_output61_nopref.rds")
write_rds(out_61, "out/100org_output61.rds")
write_rds(out_40, "out/100org_output40.rds")
write_rds(out_50, "out/100org_output50.rds")


# Plot simulation outcomes ----------------------------------------------------------------------------------------

plot_batch_dissimilarity(out_nopref, "61/39. [no pref. models]")
plot_batch_dissimilarity(out_61, "61/39.")
plot_batch_dissimilarity(out_40, "40/60.")
plot_batch_dissimilarity(out_50, "50/50.")

plot_batch_theil(out_nopref, "61/39. [no pref. models]")
plot_batch_theil(out_61, "61/39.")
plot_batch_theil(out_40, "40/60.")
plot_batch_theil(out_50, "50/50.")
