
library(tidyverse)
library(magick)

#notice that on some systems my User is yspiegler, not Yuval
directory <- "C:/Users/Yuval/OneDrive - Harvard University/projects/Labor market simulation/simulations/"

# Internal functions ----------------------------------------------------------------------------------------------

# Generate initial agents data.frame [v]
# exact - number of agents of each type are exactly (rounded) chosen based on proportions [v]
#' Create an agent grid.
#' This is the first step of the agent-based simulation.
#'
#' @param grid_size the length (= width) of the grid. The total number of cells is grid_size^2
#' @param prop_a the proportion of A-type agents. 
#' @param prop_b the proportion of B-type agents. 
#' @param prop_empty the proportion of empty cells. No empty cells = no agent movement. 
#' @param exact should the agent proportions be exact? 
#'
#' @return a data.frame with the agents with a row for each agent, and columns for id, type, and x and y location
#' @examples initialize_agents(30, 0.61, 0.39, 0.05, TRUE)
initialize_agents <- function(grid_size, prop_a, prop_b, prop_empty = 0.05, exact = TRUE) {
  #number of cells
  n <- grid_size^2
  #number of agents and empty cells
  n_a <- round(n * prop_a * (1 - prop_empty))
  n_b <- round(n * prop_b * (1 - prop_empty))
  n_empty <- n - n_a - n_b
  
  if(exact) { 
    agent_types <- sample(c(rep("A", n_a), rep("B", n_b), rep("empty", n_empty)))
  } else {
    agent_types <- sample(c("A", "B", "empty"), n, replace = TRUE, prob = c(prop_a, prop_b, 1-prop_a-prop_b))
  }
  
  agents <- data.frame(
    id = 1:n,
    type = agent_types,
    x = rep(1:grid_size, grid_size),
    y = rep(seq(1, grid_size), each = grid_size)
  ) %>% mutate(id = if_else(type == "empty", NA, id))
  
  return(agents)
}



#' Title Get neighborhood (organization) name
#'# Function to identify the specific neighborhood based on agent coordinates [v]
# [names each neighborhood by its position in the neigh grid. e.g., 1-2 = the second neigh on the first column]
#' @param x x coordinate on the grid
#' @param y y coordinate on the grid
#' @param neighborhood_size neighborhood (organization) size (e.g., 3 if the n is 9)
#'
#' @return a string with the name in the format of '#-#' where the first number is the row, and the last is the column
get_neighborhood_name <- function(x, y, neighborhood_size = 3) {
  nx <- ((x - 1) %/% neighborhood_size) + 1
  ny <- ((y - 1) %/% neighborhood_size) + 1
  return(paste(nx, ny, sep = "-"))
}

#' Title Get neighborhood/organizational compositions of an agent grid.
#'
#' @param agents an agent_grid data.frame.
#'
#' @return data.frame of neighborhoods with compositions based on an agents df. 
#' the returned df has one row per neighborhood, with counts for A, B, empty, and total agents, as well as A and B proportions
#' @export
get_neighborhood_compositions <- function(agents) {
  compositions <- agents %>%
    group_by(neighborhood, type) %>%
    summarise(count = n(), .groups = "drop") %>%
    ungroup() %>%
    complete(neighborhood, type = c("A", "B"), fill = list(count = 0)) %>%
    pivot_wider(
      names_from = type, 
      values_from = count,
      values_fill = list(count = 0)  # Ensures filling for unexpected missing cases
    ) %>%
    mutate(
      total = A + B,                   # Calculate total
      prop_A = if_else(total > 0, A / total, 0),  # Calculate proportion of A
      prop_B = if_else(total > 0, B / total, 0)   # Calculate proportion of B
    )
  return(compositions)
}

#calculate happiness for agents [v]
calc_happiness <- function(agents, threshold_a = 0.5, threshold_b = 0.5) {
  # Calculate neighborhood compositions
  compositions <- get_neighborhood_compositions(agents)
  #Determine unhappy agents
  agents <- left_join(agents, compositions, by = "neighborhood")
  agents <- agents %>% mutate(happy = case_when((type == "A" & prop_A >= threshold_a) | 
                                                  (type == "B" & prop_B >= threshold_b) ~ TRUE,
                                                type == "empty"                      ~ NA,
                                                TRUE                                 ~ FALSE)) %>%
    select(id, type, x, y, neighborhood, happy)
  
  return(agents)
}

#get composition of a single neighborhood [v]
#' Title Get the demographic composition of one neighborhood only. Wrapper to get_neighborhood_compositions.
#'
#' @param agents agent grid with neighborhood names
#' @param neighborhood_name of the neighborhood/org of interest
#'
#' @return a data.frame with a single row
#' @export
#'
#' @examples
#' get_one_neighborhood_composition(agent_grid, "2-5")
get_one_neighborhood_composition <- function(agents, neighborhood_name) {
  neighborhood_agents <- agents %>% filter(neighborhood == neighborhood_name)
  return(get_neighborhood_compositions(neighborhood_agents) %>% slice_head())
}

#move agents [v]
move_agents <- function(agents, threshold_a = 0.5, threshold_b = 0.5) {
  #get unhappy agents [v]
  unhappy_agents <- agents %>% filter(!happy) %>% select(id, type, x, y, neighborhood)
  
  if (nrow(unhappy_agents) > 0) {
    #get empty cells [v]
    free_cells <- agents %>% filter(type == "empty") %>% select(id, type, x, y, neighborhood) %>% mutate(id = NA)
    
    # df with all cells that will experience a state change this round (can be moved or moved to) + randomize order [v]
    mutable_cells <- rbind(unhappy_agents, free_cells) %>% slice(sample(1:n()))
    
    #vector of all agent ids we want to move around [v]
    ids <- mutable_cells %>% filter(!is.na(id)) %>% pull(id) 
    
    # loop over actual ids of agents to move and move them
    for (i in ids) { #[v]
      agent <- filter(mutable_cells, id == i) #[v]
      
      potential_moves <- mutable_cells %>% filter(is.na(id) & neighborhood != agent$neighborhood) #[v]
      
      if(nrow(potential_moves) > 0) {
        #select a free cell at random to move to [v]
        move_to <- slice_sample(potential_moves, n = 1)
        
        # actual move ### THERE BE PROBLEMS HERE ###
        ## take agent from original position
        mutable_cells$type[mutable_cells$id==i] <- "empty"
        mutable_cells$id[mutable_cells$id==i] <- NA
        ## take destination empty cell and move agent
        mutable_cells$type[mutable_cells$x == move_to$x & mutable_cells$y == move_to$y] <- agent$type
        mutable_cells$id[mutable_cells$x == move_to$x & mutable_cells$y == move_to$y] <- i
      }
    }
  }
  #now we combine 'mutable_cells' with the immutable cells in 'agents', and return
  agents <- agents %>% select(id, type, x, y, neighborhood) %>% anti_join(mutable_cells, by = c("x", "y"))
  agents <- rbind(agents, mutable_cells)
  
  return(agents)
}

#' Title Main movement function for the second sim. 
#'
#' Moves all agents once (one iteration) based on the following rules:
#' 1. Each agent who is still in the pool (his times_stayed counter is smaller or equal to @happy_if_stayed_n) is offered one random open cell in a different neighborhood/org.
#' 2. The relative preferences for the current and potential neighborhoods/orgs are calculated based on composition and preferences
#' which are dependent on agent type and given as a parameter prfs_a or prfs_b.
#' 3. an augmented choice is made to stay or move. If an agent moves, the agent grid changes to reflec this, and neighborhood 
#' compositions are recalculated. [this needs optimization as it is computationally heavy]
#' 4. If an agent stays, his times_stayed counter rises by one. If an agent moves, reset the counter.
#'
#' @param agents an agent grid with neighborhood compositions
#' @param prfs_a the preferences of agent type A. This is a vector with three values with the calculated preference for more A, equal, more B
#' @param prfs_b the preferences of agent type B. This is a vector with three values with the calculated preference for more A, equal, more B
#' @param happy_if_stayed_n the number of times an agent needs to stay put in a row in order to leave the pool. This directly increases iterations to completion.
#'
#' @return an updated agent_grid with updated compositions for one iteration of movement.
#' @export
#'
#' @examples
move_agents_2 <- function(agents, prfs_a, prfs_b, happy_if_stayed_n = 2) {
  # all agents have a chance to move - randomize order
  mutable_cells <- agents %>% slice(sample(1:n()))
  
  #vector of all agent ids we want to move around (no empty spaces or agents that decided to stop moving) [v]
  ids <- mutable_cells %>% filter(!is.na(id) & times_stayed <= happy_if_stayed_n) %>% pull(id) 
  
  #check if no more agents can move, return an empty data.frame
  if (is_empty(ids)) { 
    return(data.frame()) 
  }
  
  # loop over actual ids of agents to move and move them
  for (i in ids) { #[v]
    agent <- filter(mutable_cells, id == i) #[v]
    
    #potential moves are unoccupied cells in other neighborhoods
    potential_moves <- mutable_cells %>% filter(is.na(id) & neighborhood != agent$neighborhood) #[v]
    
    if(nrow(potential_moves) > 0) {
      #select a free cell at random as a potential move [v]
      move_to <- slice_sample(potential_moves, n = 1)
      
      #get current and potential move neighborhood compositions, and correct preference by agent type
      n0 <- agent$prop_A
      n1 <- move_to$prop_A
      if (agent$type == "A") {
        pref <- prfs_a
      } else {
        pref <- prfs_b
      }
      
      #calculate relative probabilities for current and potential cells based on preferences
      modifier_n0 <- case_when(n0 > 0.5  ~ pref[1],
                               n0 == 0.5 ~ pref[2],
                               n0 < 0.5  ~ pref[3])
      modifier_n1 <- case_when(n1 > 0.5  ~ pref[1],
                               n1 == 0.5 ~ pref[2],
                               n1 < 0.5  ~ pref[3])
      
      # calculate the probability of moving
      prob_n1 <- 1 / (1 + exp(modifier_n0 - modifier_n1))
      
      #make a movement decision
      choice <- sample(c("n0", "n1"), size = 1, prob = c((1-prob_n1), prob_n1))
      
      ###############################
      #if decided to move - move 
      ###############################
      if (choice == "n1") {
        #take the agent out of the origin neighgborhood/organization
        mutable_cells$type[mutable_cells$id==i] <- "empty"
        mutable_cells$id[mutable_cells$id==i] <- NA
        mutable_cells$times_stayed[mutable_cells$id==i] <- 0 #reset the times_stayed counter to 0
        ## take destination empty cell and move agent
        mutable_cells$type[mutable_cells$x == move_to$x & mutable_cells$y == move_to$y] <- agent$type
        mutable_cells$id[mutable_cells$x == move_to$x & mutable_cells$y == move_to$y] <- i 
        mutable_cells$times_stayed[mutable_cells$x == move_to$x & mutable_cells$y == move_to$y] <- 0 # just moved
        
        #recalculate old neighborhood compositions
        n0_neighborhood <- agent$neighborhood
        n0_new_comp <- get_one_neighborhood_composition(mutable_cells, n0_neighborhood)
        mutable_cells$prop_A[mutable_cells$neighborhood==n0_neighborhood] <- n0_new_comp$prop_A
        
        #recalculate new neighborhood compositions
        n1_neighborhood <- move_to$neighborhood
        n1_new_comp <- get_one_neighborhood_composition(mutable_cells, n1_neighborhood)
        mutable_cells$prop_A[mutable_cells$neighborhood==n1_neighborhood] <- n1_new_comp$prop_A
        
        # if stayed, nothing changes except increasing the counter
      } else {
        mutable_cells <- mutate(mutable_cells, times_stayed = if_else(id==i, times_stayed + 1, times_stayed))
      }
    }
  }
  #now we combine 'mutable_cells' with the immutable cells in 'agents', and return [v]
  agents <- agents %>% select(id, type, x, y, neighborhood, times_stayed) %>% anti_join(mutable_cells, by = c("x", "y"))
  agents <- rbind(agents, mutable_cells)
  
  return(agents)
}


#' Title Calculate the dissimilarity index
#' Calculate a dissimilarity index = what proportion of group B needs to move spatial units (e.g., orgs, neighborhoods)
#' to create no segregation (i.e., that the prop of the groups in each unit is equal to their overall proportion) 
#' --- works with the output from the function @get_neighborhood_compositions()
#' 
#' @param neighborhoods data.frame containing organizations/neighborhoods with compositions.
#'
#' @return the numeric value of the dissimilarity index.
#' @export
calc_dissimilarity <- function(neighborhoods) {
  Tot <- sum(neighborhoods$total) #the total number of agents of both groups
  P <- sum(neighborhoods$B) / Tot #the overall proportion of B group in the entire grid
  
  numerator <- sum(neighborhoods$total * abs(neighborhoods$prop_B - P))
  denominator <- 2 * Tot * P * (1 - P)
  
  return(numerator / denominator)
}

# Visualization functions -----------------------------------------------------------------------------------------

# Function to print the grid as a matrix (text viz) [v]
print_grid_matrix <- function(agents, grid_size, neighborhood_size) {
  grid <- matrix("", nrow = grid_size, ncol = grid_size)
  for (i in 1:nrow(agents)) {
    grid[agents$x[i], agents$y[i]] <- agents$type[i]
  }
  print(grid)
}

# WORK ON TEXT LOCATION!
#' Title prints out a nice ggplot of an agent_grid. Adds dissimilarity outcome text.
#'
#' @param grid an agent_grid data.frame - does not need neighborhood/org compositions or names.
#' @param neighborhood_size the size of the neighborhood (organization). If 3x3 put in 3.
#' @param time for annotating the iteration number
#' @param simulation sim type. 1 = original, corresponds to move_agents(), 2 = corresponds to move_agents_2()
#'
#' @return a plot
#' @export
#'
plot_grid <- function(grid, neighborhood_size, time = NA, simulation = 1) {
  #calculate segregation
  dsmlrty <- calc_dissimilarity(get_neighborhood_compositions(grid))
  siz <- sqrt(nrow(grid))
  
  if (simulation == 1) {
    annotation <- str_c("Dissimilarity = ", round(dsmlrty, 3), " | Unhappy = ", 
                        round(100*prop.table(table(grid$happy))[[1]],3), "%", 
                        " | Iteration: ", time)
  } else if (simulation == 2) {
    annotation <- str_c("Itteration: ", time, " | Dissimilarity: ", round(dsmlrty, 3))
  }
  
  a_plot <- ggplot(grid, aes(x = x, y = y, fill = type)) +
    geom_tile(color = "black", size = 0.3) +
    scale_fill_manual(values = c("A" = "#444ccc", "B" = "orange", "empty" = "white")) +
    theme_void() +
    geom_hline(yintercept = seq(0.5,(siz+neighborhood_size), by = neighborhood_size), color = "white", size = 1.2) +
    geom_vline(xintercept = seq(0.5,(siz+neighborhood_size), by = neighborhood_size), color = "white", size = 1.2) +
    theme(legend.position = "none") + 
    ggtitle(annotation)
  
  return(a_plot)
}

