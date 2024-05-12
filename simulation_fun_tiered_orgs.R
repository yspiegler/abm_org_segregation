### in this version of the simulation, organizations are much more flexible. They can be of any size,
### or be hierarchical (like in the conjoint experiment). So the actual size of the grid should be 
### indicated by the organizational size and number or orgs, ands isn't a square.
initialize_agents_tiered <- function(org_srtucture = c(1,3,3), n_orgs, prop_a_t1, prop_a_t2, prop_a_t3, 
                                     prop_b_t1, prop_b_t2, prop_b_t3, prop_empty = 0.05, exact_prop = TRUE) {
  
  # number of people in the organization
  org_size  <- sum(org_srtucture)
  # total number of cells in the 'labor market'
  n <- org_size * n_orgs
  # proportion of each tier in the organizations
  prop_t1   <- org_srtucture[1] / org_size
  prop_t2   <- org_srtucture[2] / org_size
  prop_t3   <- org_srtucture[3] / org_size
  
  # number of each agent type + empty cells. Calculated by multiplying the proportion of the tier out of total, 
  # by the proportion of the agent type out of the tier, multiplied by 1 - the proportion of empty cells. 
  # Finally, the result is rounded to the closest agent (no half agents ;))
  n_a1 <- round(n * prop_a_t1 * prop_t1 * (1 - prop_empty))
  n_a2 <- round(n * prop_a_t2 * prop_t2 * (1 - prop_empty))
  n_a3 <- round(n * prop_a_t3 * prop_t3 * (1 - prop_empty))
  n_b1 <- round(n * prop_b_t1 * prop_t1 * (1 - prop_empty))
  n_b2 <- round(n * prop_b_t2 * prop_t2 * (1 - prop_empty))
  n_b3 <- round(n * prop_b_t3 * prop_t3 * (1 - prop_empty))
  n_empty <- n - (n_a1 + n_a2 + n_a3 + n_b1 + n_b2 + n_b3)
  
  # create randomized vectors of agents based on the proportion of each type
  agents_t1 <- 
  agents_t2
  agents_t3
  
  _types <- c(rep("A1", n_a1), rep("A2", n_a2), rep("A3", n_a3),
                   rep("B1", n_b1), rep("B2", n_b2), rep("B3", n_b3),
                   rep("empty", n_empty))
  # randomize it
  agent_types <- sample(agent_types)
  
  # set organizational ids
  org_names <- rep(1:n_orgs, each = org_size)
  
  # create and return an agent data frame
  agents <- data.frame(
    id   = 1:n,
    type = agent_types,
    org  = org_names 
    
  )
}


function(grid_size, prop_a, prop_b, prop_empty = 0.05, exact = TRUE) {
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