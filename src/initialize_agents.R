library(stringr)


### in this version of the simulation, organizations are much more flexible. They can be of any size,
### or be hierarchical (like in the conjoint experiment). So the actual size of the grid should be 
### indicated by the organizational size and number or orgs, ands isn't a square. [V]
initialize_agents_tiered <- function(org_srtucture = c(1,3,9), n_orgs, prop_a_t1, prop_a_t2, prop_a_t3, 
                                     prop_b_t1, prop_b_t2, prop_b_t3, prop_empty = 0.1, exact_prop = TRUE) {
  # the size of each tier (one manager per org => size_t1 == 1)
  size_t1 <- org_srtucture[1]
  size_t2 <- org_srtucture[2]
  size_t3 <- org_srtucture[3]
  
  # number of people in the organization
  org_size  <- sum(org_srtucture)
  # total number of cells in the 'labor market'
  n <- org_size * n_orgs
  # total number of each tier in the organizations (ex. org=13, n_org = 10 -> n_t3 = 130*9/13 = 90)
  n_t1 <- (n * size_t1) / org_size
  n_t2 <- (n * size_t2) / org_size
  n_t3 <- (n * size_t3) / org_size
  
  # number of each agent type + empty cells. Calculated by multiplying the number of agents in a tier 
  # by the proportion of the agent type out of the tier, multiplied by 1 - the proportion of empty cells. 
  # Finally, the result is rounded to the closest agent (no half agents ;))
  n_a1 <- round(prop_a_t1 * n_t1 * (1 - prop_empty))
  n_a2 <- round(prop_a_t2 * n_t2 * (1 - prop_empty))
  n_a3 <- round(prop_a_t3 * n_t3 * (1 - prop_empty))
  n_b1 <- round(prop_b_t1 * n_t1 * (1 - prop_empty))
  n_b2 <- round(prop_b_t2 * n_t2 * (1 - prop_empty))
  n_b3 <- round(prop_b_t3 * n_t3 * (1 - prop_empty))
  n_empty1 <- n_t1 - (n_a1 + n_b1)
  n_empty2 <- n_t2 - (n_a2 + n_b2)
  n_empty3 <- n_t3 - (n_a3 + n_b3)
  
  # create randomized vectors of agents based on the proportion of each type
  agents_t1 <- sample(c(rep("A1", n_a1), rep("B1", n_b1), rep("empty", n_empty1)))
  agents_t2 <- sample(c(rep("A2", n_a2), rep("B2", n_b2), rep("empty", n_empty2)))
  agents_t3 <- sample(c(rep("A3", n_a3), rep("B3", n_b3), rep("empty", n_empty3)))
  
  # set organizational ids (if org size is 13, than repeat each id 13 times)
  org_names <- rep(1:n_orgs, each = org_size)
  
  # the entire next part populates an agent vector with the randomized tiered agent vectors (e.g., agents_t1, 2, 3)
  # from above, such that the order is one from t1, three from t2, and nine from t3 [or any other prespecified number]
  # and this is done as many times as there are organizations. 
  agent_types <- character(length = n)
  # this runs n_orgs iterations, each adding the correct agents (randomized prior) to each tier in sequence
  for(i in 1:n_orgs) {
    # when tier1 is index (iteration / org number) i, what is the beginning index of tier 2 and 3?
    indx    <- ((i-1) * org_size)+ 1
    indx_t2 <- ((i-1) * size_t2) + 1
    indx_t3 <- ((i-1) * size_t3) + 1
    
    # add the entire org to the correct place in sequence
    agent_types[indx:(indx+org_size-1)] <- c(agents_t1[i],
                                             agents_t2[indx_t2:(indx_t2 + size_t2 - 1)],
                                             agents_t3[indx_t3:(indx_t3 + size_t3 - 1)])
  }
  
  # create and return an agent data frame
  agents <- data.frame(
    position_id   = 1:n,
    agent = agent_types,  #this is mostly left for debugging (making sure df is created as expected)
    type = str_sub(agent_types, 1, 1),
    tier = rep(c(rep(1, size_t1), rep(2, size_t2), rep(3, size_t3)), n_orgs),
    vert_group = rep(c(rep(0, size_t1), 1:size_t2, rep(1:size_t2, size_t3/size_t2)), n_orgs),
    org  = org_names, 
    n_stay = 0 #number of times in a row the agent chose not to move
  ) %>% mutate(type = if_else(type == "e", "empty", type))
  
  return(agents)
}