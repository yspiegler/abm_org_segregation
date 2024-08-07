library(readr)


# Source all relevant R files -------------------------------------------------------------------------------------

source("src/initialize_agents.R")
source("src/run_simulations.R")
source("src/agent_movement.R")
source("src/plotting_functions.R")
source("src/calculate_segregation.R")

#  Run the simulations ------------------------------------------------------------------------------------

# 100 orgs, 100 iterations, 100 simulations max
out_nopref <- run_sim_batch(100, 0.61, 0.39, FALSE) #minimum effect - NULL models
out_61 <- run_sim_batch(100, 0.61, 0.39, TRUE)
out_40 <- run_sim_batch(100, 0.4, 0.6, TRUE)
out_50 <- run_sim_batch(100, 0.5, 0.5, TRUE)
out_75 <- run_sim_batch(100, 0.75, 0.25, TRUE)
out_deterministic <- run_sim_batch(100, 0.61, 0.39, TRUE, TRUE) #maximum effect

# save results
write_rds(out_nopref, "out/100org_output61_nopref.rds")
write_rds(out_61, "out/100org_output61.rds")
write_rds(out_40, "out/100org_output40.rds")
write_rds(out_50, "out/100org_output50.rds")
write_rds(out_75, "out/100org_output75.rds")


# Plot simulation outcomes ----------------------------------------------------------------------------------------

# plot Theil simulations with mean effect line + individual sim lines [+ Sims = s]
## this is more impressive and gives a view of how the individual simulations behave underneath the mean effect
p0_s <- plot_batch_theil(out_nopref, "61/39", FALSE, TRUE)
p61_s <- plot_batch_theil(out_61, "61/39", TRUE, TRUE)
p75_s <- plot_batch_theil(out_75, "75/25", TRUE, TRUE)
p50_s <- plot_batch_theil(out_50, "50/50", TRUE, TRUE)
p40_s <- plot_batch_theil(out_40, "40/60", TRUE, TRUE)

## save the plots
save_plot_print(p0_s, "plot_theil_61_sims_nopref")
save_plot_print(p61_s, "plot_theil_61_sims")
save_plot_print(p75_s, "plot_theil_75_sims")
save_plot_print(p50_s, "plot_theil_50_sims")
save_plot_print(p40_s, "plot_theil_40_sims")

#plot Theil simulations with mean effect line without individual sim lines [No Sim = ns]
#this highlights the shape of the mean effect of segregation because removing individual simulation lines narrows the Y range
p0_ns <- plot_batch_theil(out_nopref, "61/39", FALSE, FALSE)
p61_ns <- plot_batch_theil(out_61, "61/39", TRUE, FALSE)
p75_ns <- plot_batch_theil(out_75, "75/25", TRUE, FALSE)
p50_ns <- plot_batch_theil(out_50, "50/50", TRUE, FALSE)
p40_ns <- plot_batch_theil(out_40, "40/60", TRUE, FALSE)

## save the plots
save_plot_print(p0_ns, "plot_theil_61_main_nopref")
save_plot_print(p61_ns, "plot_theil_61_main")
save_plot_print(p75_ns, "plot_theil_75_main")
save_plot_print(p50_ns, "plot_theil_50_main")
save_plot_print(p40_ns, "plot_theil_40_main")
