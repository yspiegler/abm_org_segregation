library(tidyverse)
library(magick)

# Get the necessary functions
source("simulation_functions.R")


# Running simulation 1------------------------------------------------------------------------------------------

# Define thresholds (for sim 1)
run_sim1 <- function(grid_size, prop_a, prop_b, threashold_a, threashold_b, iterations, make_gif = FALSE, neighborhood_size = 3) {
  
  #check that neighborhood/org size fits in the grid size
  if (grid_size %% neighborhood_size != 0) {
    stop("simulation aborted: the neighborhood/org size must divide the grid_size without a reminder.")
  }
  
  #initialize agents
  agents <- initialize_agents(grid_size, prop_a, prop_b)
  agents$neighborhood <- mapply(get_neighborhood_name, agents$x, agents$y, neighborhood_size)
  #agent_identities <- agents %>% select(id, type) #immutable agent list - just type and id for tracking
  agents <- calc_happiness(agents, threashold_a, threashold_b)
  
  print(plot_grid(agents, time = 1, neighborhood_size = neighborhood_size))
  prev_dissimilarity <- 0
  
  
  for (j in 2:iterations) {
    agents <- move_agents(agents)
    agents <- calc_happiness(agents, threashold_a, threashold_b)
    dsm <- calc_dissimilarity(get_neighborhood_compositions(agents))
    
    plt <- plot_grid(agents, time = j, neighborhood_size = neighborhood_size)
    print(plt)
    
    #save the plot as an *.png file if requested
    if (make_gif) { 
      ggsave(filename = paste(directory, "animations/", "plot", j, ".png", sep = ""), plot = plt, width = 800, height = 800, units = "px", create.dir = TRUE) 
    }
    print(str_c("iteration " , j, ": dissimilarity: ", dsm))
  }
  
  if (make_gif) {
    # Create an image list from the saved PNG files
    image_list <- image_read(path = paste(directory, "animations/", sep = ""))
    #image_list <- image_read(path = paste("C:/Users/Yuval/Videos/simulations", sep = ""))
    
    # Animate the image list
    gif <- image_animate(image_list, fps = 2)  # fps controls the speed, adjust as necessary
    
    # Save the animated GIF
    image_write(gif, path = paste(directory, "animations/", "animated_sim1_plot.gif"))
    
  }
}

#us labor market: white = 61.5, non-white = 38.5, here with 10% empty
# t0: diss = 0.248 | t25: diss = 0.374
run_sim1(grid_size = 40, prop_a = 0.58425, prop_b = 0.36575, threashold_a = 0.33, threashold_b = 0.33, 
         iterations = 25, make_gif = FALSE, neighborhood_size = 4)
#us labor market: white = 61.5, non-white = 38.5, here with 10% empty
# t0: diss = 0.229 | t25: diss = 0.419
run_sim1(grid_size = 40, prop_a = 0.55, prop_b = 0.35, threashold_a = 0.33, threashold_b = 0.33, 
         iterations = 25, make_gif = FALSE, neighborhood_size = 4)

#us labor market: white = 61.5, non-white = 38.5, here with 20% empty
# t0: diss = 0.244 | t25: diss = 0.472
run_sim1(grid_size = 40, prop_a = 0.492, prop_b = 0.308, threashold_a = 0.33, threashold_b = 0.33, 
         iterations = 25, make_gif = FALSE, neighborhood_size = 4)

#us labor market: white = 61.5, non-white = 38.5, here with 20% empty, threshold 0.25 only
# t0: diss = 0.241 | t25: diss = 0.31
run_sim1(grid_size = 40, prop_a = 0.492, prop_b = 0.308, threashold_a = 0.25, threashold_b = 0.25, 
         iterations = 25, make_gif = FALSE, neighborhood_size = 4)

#us labor market: white = 61.5, non-white = 38.5, here with 10% empty, threshold their prop
# t0: diss = 0.222 | t25: diss = 0.735
run_sim1(grid_size = 40, prop_a = 0.55, prop_b = 0.35, threashold_a = 0.55, threashold_b = 0.35, 
         iterations = 25, make_gif = FALSE, neighborhood_size = 4)

# Running simulation 2 ------------------------------------------------------------------------

# Define relative preferences. Format: three preference modifiers based on relative proportion of 'my' agent type - 
# c(mod.more.mytype, mod.equal.types, mod.less.mytype) - this is theory / empiric based
# these are calculated by combining each of the three attribute marginal means
rel_pref_A <- c(1.49, 1.62, 1.44)   #the overall profile marginal mean sum for mostly whites, mix, and mostly blacks [for whites]
rel_pref_B <- c(1.26, 1.574, 1.705) #the overall profile marginal mean sum for mostly whites, mix, and mostly blacks [for blacks]

#' Title Run Simulation #2: agents move based on a probability function based on relative preferenes
#'
#' This function creates an initial agent grid based with organizational boundaries based on user input, calculates 
#' organizational/neighborhood compositions, and then moves the agents for multiple iterations until either the iteration max
#' is met, or all agents are happy. At each iteration the agent grid is plotted.
#' 
#' 
#' 
#' @param agents an agent grid with neighborhood compositions
#' @param prfs_a the preferences of agent type A. This is a vector with three values with the calculated preference for more A, equal, more B
#' @param prfs_b the preferences of agent type B. This is a vector with three values with the calculated preference for more A, equal, more B
#' @param happy_if_stayed_n the number of times an agent needs to stay put in a row in order to leave the pool. This directly increases iterations to completion.
#'
#' @return an updated agent_grid with updated compositions for one iteration of movement.
#' @export
run_sim2 <- function(grid_size, prop_a, prop_b, prop_empty = 0.05, rel_pref_A, rel_pref_B, iterations = 50, make_gif = FALSE, neighborhood_size = 3) {
  
  #check that neighborhood/org size fits in the grid size
  if (grid_size %% neighborhood_size != 0) {
    stop("simulation aborted: the neighborhood/org size must divide the grid_size without a reminder.")
  }
  
  # initialize agent grid based on A and B proportions
  agents <- initialize_agents(grid_size, prop_a, prop_b, prop_empty, exact = TRUE)
  # add neighborhood names based on neighborhood size (make sure it perfectly divides by the grid size)
  agents$neighborhood <- mapply(get_neighborhood_name, agents$x, agents$y, neighborhood_size)
  # add column that counts how many times an agent refused to change jobs / neighborhoods
  agents$times_stayed <- 0
  
  # Calculate neighborhood compositions
  compositions <- get_neighborhood_compositions(agents)
  #add neighborhood data
  agents <- left_join(agents, compositions, by = "neighborhood")
  
  print(plot_grid(agents, neighborhood_size = neighborhood_size, time = 1, simulation = 2))
  
  for (j in 2:iterations) {
    print(str_c("this is the start of itteration: ", j))
    agents <- move_agents_2(agents, rel_pref_A, rel_pref_B)
    
    #check if no more movement is left (I know this is ugly. I should not know the internals of 'move_agents_2'... fix when have time)
    if (is_empty(agents)) {
      print("------- no more agents to move ------")
      break
    }
    
    # dissimilarity
    dsm <- calc_dissimilarity(get_neighborhood_compositions(agents))
    # plot grid
    plt <- plot_grid(agents, time = j, neighborhood_size = neighborhood_size, simulation = 2)
    print(plt)
    
    #save the plot as an *.png file if requested
    if (make_gif) { 
      ggsave(filename = paste(directory, "animations/", "plot", j, ".png", sep = ""), plot = plt, width = 800, height = 800, units = "px", create.dir = TRUE) 
    }
    print(str_c("iteration " , j, ": dissimilarity: ", dsm))
  }
}


# Run simulations arena -------------------------------------------------------------------------------------------


# for reproducibility, Remove when running batch.
set.seed(1000)

# labor market proportions like overall US (61 White/39 Minority) [ex. S.Carolina, Washington, Connecticut] :dissimilarity 0.159 -> 0.339
run_sim2(grid_size = 20, prop_a = 0.61, prop_b = 0.39, prop_empty = 0.15, rel_pref_A, rel_pref_B, iterations = 150, make_gif = FALSE, neighborhood_size = 4)

# labor market proportions like overall US (61 White/39 Minority) [ex. S.Carolina, Washington, Connecticut] :dissimilarity 0.286 -> 0.406
run_sim2(grid_size = 30, prop_a = 0.61, prop_b = 0.39, prop_empty = 0.05, rel_pref_A, rel_pref_B, iterations = 150, make_gif = FALSE, neighborhood_size = 3)

# labor market proportions are equal (50 White/50 Minority) [ex. Florida, NJ, Georgia] :dissimilarity 0.179 -> 0.258
run_sim2(grid_size = 20, prop_a = 0.5, prop_b = 0.5, prop_empty = 0.05, rel_pref_A, rel_pref_B, iterations = 150, make_gif = FALSE, neighborhood_size = 4)

# labor market proportions are reversed (40 White/60 Minority) [ex. Texas, DC, Nevada] :dissimilarity 0.173 -> 0.300
run_sim2(grid_size = 20, prop_a = 0.40, prop_b = 0.60, prop_empty = 0.05, rel_pref_A, rel_pref_B, iterations = 250, make_gif = FALSE, neighborhood_size = 4)

