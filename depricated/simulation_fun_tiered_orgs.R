library(tidyverse)
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

# get the correct agent group that  is hierarchically relevant to a specific agent. 
# Example - a tier 3 agent of vertical group 1 should get their peers (all t3 agents) + 
# their direct manager (the t2 group 1 agent) [V]
get_rel_hierarchy <- function(agents, agent, summarize_data = TRUE) {
  agent_id    <- agent$position_id
  agent_tier  <- agent$tier
  agent_group <- agent$vert_group
  agent_org   <- agent$org
  
  # GET THE CORRECT AGENT HIERARCHY GIVEN THE FOCAL AGENT TIER AND VERTICAL GROUP
  ### if agent is t1, just get all t2 (their subordinates).
  if (agent_tier == 1) {
    rh <- agents %>% filter(
      org == agent_org,
      tier == 2
    )
  }
  ### different rule for tier 2 agents, because they need all t1 and t2 agents, but only t3 agents with the same vert_group
  if (agent_tier == 2) {
    rh <- agents %>% filter(
      org == agent_org,
      (tier != 3 | (tier == 3 & vert_group == agent_tier))
    )  
  }
  ### if t3, we need the t3 agents who share the same vert_group, and the one t2 that also shares it (their manager)
  if (agent_tier == 3) {
    rh <- agents %>% filter(
      org == agent_org,
      vert_group == agent_group
    )
  }
  
  # Return a summarized version of the agent hierarchy - one row per tier, summarized to all white, all black, or mixed.
  # Calculate without "empty" spots. If an entire tier is empty (will be most common for t1, since there is only one agent
  # spot), remove the tier entirely.
  # Return full agent data if summarize_data is FALSE
  if (summarize_data) {
    rh <- rh %>% filter(position_id != agent_id) %>%
      arrange(tier, type) %>%
      group_by(tier) %>%
      summarize(composition = case_when(all(type %in% c("A", "empty")) & any(type == "A") ~ "just_a",
                                        all(type %in% c("B", "empty")) & any(type == "B") ~ "just_b",
                                        all(type == "empty")                              ~ "empty",
                                        TRUE                                              ~ "mix")
                )
  } 

  return(rh)  
}


# Calculate the dissimilarity index for all data, or by tier. Note that type 1 tier will always be the same as the 
# overall proportions and there is no reasons  [V]
calc_dissimilarity <- function(agents, calc_tier = NA) {
  #make sure the values are correct
  if(!is.na(calc_tier) & calc_tier != 2 & calc_tier != 3) {
    print("ERROR: Dissimilarity needs to be calculated on all agents, 
          or agents of type 2 or 3. Please use one of these values.")
    stop()
  }
  
  #NA (default) means the function will calculate overall dissimilarity between orgs.
  #if calc_tier equals 2, or 3, this will calculate just the dissimilarity between orgs in that tier only.
  if(calc_tier %in% c(2,3)) {
    agents <- agents %>% filter(tier == calc_tier)    
  }
  
  #create a small data for the organizations 
  orgs <- agents %>% 
    filter(type != "empty") %>%
    group_by(org) %>%
    summarise(total  = n(),
              n_b    = sum(type=="B")) %>%
    mutate(prop_b    = n_b / total)

  #calculate dissimilarity
  Tot <- sum(orgs$total) #the total number of agents of both groups
  P <- sum(orgs$n_b) / Tot #the overall proportion of B group in the entire grid
  
  numerator <- sum(orgs$total * abs(orgs$prop_b - P))
  denominator <- 2 * Tot * P * (1 - P)
  
  return(numerator / denominator)
}

#Calculate H Theil index of segregation (based on Entropy)
calc_theil <- function(agents) {
  #create a small data for the organizations 
  orgs <- agents %>% 
    filter(type != "empty") %>%
    group_by(org) %>%
    summarise(total  = n(),
              n_b    = sum(type=="B")) %>%
    mutate(prop_b    = n_b / total)
  
  # step 1: calculate entropy for each organization
  orgs <- mutate(orgs, entropy = map_dbl(prop_b, calc_entropy))
  # step 2: calculate overall entropy score
  n_workers <- sum(orgs$total)
  E <- calc_entropy(sum(orgs$n_b) / n_workers)
  
  # step 3: calculate partial organizational Theil
  orgs <- orgs %>%
    mutate(partial_h = (total * (E - entropy)))
  
  # step 4: calc the Market (overall) Theil index
  H <- (1 / (n_workers * E)) * sum(orgs$partial_h)
  
  return(H)
}

# calculate entropy index for a single unit with two groups (e.g., Black and White people) [v]
calc_entropy <- function(prop) {
  
  if(prop == 1 | prop == 0) {
    return(0)
  } else {
    return((prop * log(1 / prop)) + 
          ((1 - prop) * log(1 / (1 - prop))))
  }
}

move_agent <- function(agents, agent, position, have_preference = TRUE) {
  
  #get surrounding workers in org hierarchy [v]
  neighbors_0 <- get_rel_hierarchy(agents, agent) # MB micro = 5,493
  neighbors_1 <- get_rel_hierarchy(agents, position)
  
  #make a choice - stay, or move
  
  if (have_preference) {
    choice <- choose_position(agent$type, agent$tier, neighbors_0, neighbors_1)   # MB micro = 2,604
  } else {
    choice <- sample(c("stay", "move"), 1)
  }
  
  
  agt_id <- agent$position_id
  pos_id <- position$position_id
  
  # if agent chose to stay, just increase the n_stay counter by 1
  if (choice == "stay") {
    agents <- agents %>% mutate(n_stay = if_else(position_id == agt_id, n_stay + 1, n_stay))
  }
  # I know 'else' if a bit move efficient here in R, but this is move readable
  if (choice == "move") {
    agt_type <- agent$type
    
    agents <- agents %>% mutate(n_stay = if_else(position_id == agt_id, 0, n_stay), #reset stay number to 0
                                type   = case_when(position_id == agt_id ~ "empty",    #turn original position to empty
                                                   position_id == pos_id  ~ agt_type, #move agent type to new spot
                                                   TRUE ~ type))                         
  }
  
  return(agents)
}

# give the agent type and tier, calculate the relative utilities of the current (h0) and offered (h1) positions
# and make a choice = "stay" or "move" [v]
choose_position <- function(agent_type, agent_tier, h0, h1) {
  #get relevant effects based on agent/position tier + agent type (A or B)
  # !!!!! NOTE::: this means that for a tier 3 agent, the relevant effects belong to t1 (manager) and t2 (peers)
  #!!!!!!         the manager effect will be mapped on tier two (the manager for a t3 agent), and the peer effects
  #!!!!!!         will be mapped on tier three (the peers of a t3 agent). Be wary of confusion!! [v]
  
  ### for managers, tier 2 are their subordinates (change tier 2 which is normally peers, to 3, to reflect that it should
  ### use the effect for subordinates)
  if (agent_tier == 1) {
    eff <- effects %>% filter(tier == 3 & 
                              type == agent_type)
    
    h0$tier <- h0$tier + 1
    h1$tier <- h1$tier + 1
  } 
  
  ### for peers, t1 are managers, t2 are peers, and t3 are subordinates (no change)
  else if (agent_tier == 2) {
    eff <- effects %>% filter(type == agent_type)
  } 
  
  ### for subordinates, t3 are peers, and t2 is the manager.
  ### so, the relevant effects are for manager and peers [v]
  else if (agent_tier == 3) {
    eff <- effects %>% filter(tier %in% c(1, 2) & 
                                type == agent_type)
    
    h0$tier <- h0$tier - 1
    h1$tier <- h1$tier - 1
  } 
  else {
    print("Error: agent_tier argument is not 1, 2, or 3")
    stop()
  }

  #utility for the start (current) position on the focal agent
  h0 <- left_join(h0, eff, by = c("tier", "composition")) #add the correct estimates (effects) to tiers based on compositions
  h0[h0$composition=="empty","estimate"] <- 0.5  #change empty tiers to 'no effect' = 0.5 (if all agents in a tier in an org are empty, no preference)
  h0_util <- sum(h0$estimate) #add partial utilities
  h0_util_exp <- exp(h0_util) #exponentiate the sum
  
  #utility for the offered position to the focal agent
  h1 <- left_join(h1, eff, by = c("tier", "composition"))
  h1[h1$composition=="empty","estimate"] <- 0.5
  h1_util <- sum(h1$estimate)
  h1_util_exp <- exp(h1_util)
  
  #prop. choosing to stay in current position
  (prob_stay <- h0_util_exp / (h0_util_exp + h1_util_exp))
  
  # choose 
  choose <- sample(c("stay", "move"), size = 1, prob = c(prob_stay, (1-prob_stay)))
  return(choose)
}

move_agents <- function(agents, happy_if_stayed = 3, have_prerefernce = TRUE) {
  
  # get the ids of all non empty agents that are still looking to move [BM mean = 982]
  agent_ids <- agents %>% filter(type != "empty" & n_stay <= happy_if_stayed) %>% pull(position_id)
  
  # randomize their movement order [MB mean = 30]
  agent_ids <- sample(agent_ids)

  # if no more agents are left to move, return an empty data.frame
  if (is_empty(agent_ids)) { 
    return(data.frame()) 
  }
  
  # loop over actual ids of agents to move and move them
  for (i in agent_ids) {
    agent <- agents[agents$position_id == i,] # MB mean = 46
    agent_org <- agent$org  # MB mean = .660
    agent_tier <- agent$tier # MB mean = .690
    
    # get random open position in another organization but same tier [MB mean = 4,728]
    position <- sample_n(agents %>% filter(type == "empty" & 
                                           org != agent_org &
                                           tier == agent_tier),
                         1)

    if (nrow(position) == 1){
      agents <- move_agent(agents, agent, position, have_prerefernce)  #[MB mean = 14,237 (micro.s)]
    } else {
      print(agent)
      print(position)
      stop()
    }
  }
  
  return(agents)
}


# testing ---------------------------------------------------------------------------------------------------------

