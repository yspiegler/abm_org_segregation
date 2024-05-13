### in this version of the simulation, organizations are much more flexible. They can be of any size,
### or be hierarchical (like in the conjoint experiment). So the actual size of the grid should be 
### indicated by the organizational size and number or orgs, ands isn't a square.
initialize_agents_tiered <- function(org_srtucture = c(1,3,3), n_orgs, prop_a_t1, prop_a_t2, prop_a_t3, 
                                     prop_b_t1, prop_b_t2, prop_b_t3, prop_empty = 0.05, exact_prop = TRUE) {
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
  agents_t2 <- sample(c(rep("A2", n_a2), rep("B2", n_b2), rep("empty", n_empty1)))
  agents_t3 <- sample(c(rep("A3", n_a3), rep("B3", n_b3), rep("empty", n_empty3)))
  
  # set organizational ids (if org size is 13, than repeat each id 13 times)
  org_names <- rep(1:n_orgs, each = org_size)
  
  # take 1 of agents_t1, 3 of agents_t2 and 9 (or 3) of agents_t3, and repeat that
  agent_types <- character()
  
  for(i in 1:n_orgs) {
    # when tier1 is index (iteration / org number) i, what is the beginning index of tier 2 and 3?
    indx_t2 <- ((i-1) * size_t2) + 1
    indx_t3 <- ((i-1) * size_t3) + 1
    
    agent_types <- c(agent_types, 
                     agents_t1[i],
                     agents_t2[indx_t2:(indx_t2 + size_t2 - 1)],
                     agents_t3[indx_t3:(indx_t3 + size_t3 - 1)])
  }
  
  # create and return an agent data frame
  agents <- data.frame(
    id   = 1:n,
    type = agent_types,
    org  = org_names 
    
  ) %>% mutate(id = if_else(type == "empty", NA, id))
  
  return(agent_types)
}
