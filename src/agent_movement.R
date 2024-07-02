library(data.table)



# Move all relevant agents
move_agents <- function(agents, happy_if_stayed = 3, have_prerefernce = TRUE) {
  
  # Get the IDs of all non-empty agents still looking to move
  agent_ids <- agents[type != "empty" & n_stay <= happy_if_stayed, position_id]
  
  # Randomize their movement order
  agent_ids <- sample(agent_ids)
  
  # If no more agents are left to move, return an empty data.table
  if (length(agent_ids) == 0) {
    return(data.table()) 
  }
  
  # Loop over actual IDs of agents to move and move them
  for (i in agent_ids) {
    agent <- agents[position_id == i]
    agent_org <- agent$org
    agent_tier <- agent$tier
    
    # Get a random open position in another organization but same tier
    position <- agents[type == "empty" & org != agent_org & tier == agent_tier, .SD[sample(.N, 1)]]
    
    if (nrow(position) == 1) {
      agents <- move_agent(agents, agent, position, have_prerefernce)
    } else {
      print(agent)
      print(position)
      stop("No valid position found, stopping execution.")
    }
  }
  
  return(agents)
}


# Move a single agent
move_agent <- function(agents, agent, position, have_preference = TRUE) {
  # Get surrounding workers in org hierarchy
  neighbors_0 <- get_rel_hierarchy(agents, agent)  
  neighbors_1 <- get_rel_hierarchy(agents, position)
  
  # Make a choice - stay, or move
  if (have_preference) {
    choice <- choose_position(agent$type, agent$tier, neighbors_0, neighbors_1)
  } else {
    choice <- sample(c("stay", "move"), 1)
  }
  
  agt_id <- agent$position_id
  pos_id <- position$position_id
  
  # If agent chose to stay, just increase the n_stay counter by 1
  if (choice == "stay") {
    agents[position_id == agt_id, n_stay := n_stay + 1]
  }
  
  # If the agent chose to move
  if (choice == "move") {
    agt_type <- agent$type
    
    # Reset stay number to 0 for the original position and turn it to 'empty'
    agents[position_id == agt_id, `:=`(n_stay = 0, type = "empty")]
    
    # Move agent type to new spot
    agents[position_id == pos_id, type := agt_type]
  }
  
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
  
  # Initialize rh as a data table
  rh <- data.table()
  
  # Filter based on the agent tier
  if (agent_tier == 1) {
    rh <- agents[org == agent_org & tier == 2]
  }
  if (agent_tier == 2) {
    rh <- agents[org == agent_org & (tier != 3 | (tier == 3 & vert_group == agent_tier))]
  }
  if (agent_tier == 3) {
    rh <- agents[org == agent_org & vert_group == agent_group]
  }
  
  if (summarize_data) {
    rh <- rh[position_id != agent_id, .(composition = fifelse(all(type %in% c("A", "empty")) & any(type == "A"), "just_a",
                                                              fifelse(all(type %in% c("B", "empty")) & any(type == "B"), "just_b",
                                                                      fifelse(all(type == "empty"), "empty", "mix")))),
             by = tier]
    # Arrange is not typically necessary in data.table since it optimizes grouping internally
  }
  
  return(rh)
}

# give the agent type and tier, calculate the relative utilities of the current (h0) and offered (h1) positions
# and make a choice = "stay" or "move" [v]
choose_position <- function(agent_type, agent_tier, h0, h1) {
  
  # Adjust tiers based on agent tier
  if (agent_tier == 1) {
    eff <- effects[tier == 3 & type == agent_type]
    h0[, tier := tier + 1]
    h1[, tier := tier + 1]
    
  } else if (agent_tier == 2) {
    eff <- effects[type == agent_type]
    
  } else if (agent_tier == 3) {
    eff <- effects[tier %in% c(1, 2) & type == agent_type]
    h0[, tier := tier - 1]
    h1[, tier := tier - 1]
    
  } else {
    stop("Error: agent_tier argument is not 1, 2, or 3")
  }
  
  # Utility for the start (current) position on the focal agent
  h0 <- h0[eff, on = .(tier, composition), nomatch = 0]
  h0[composition == "empty", estimate := 0.5]  # Change empty tiers to 'no effect'
  h0_util <- sum(h0$estimate)  # Add partial utilities
  h0_util_exp <- exp(h0_util)  # Exponentiate the sum
  
  # Utility for the offered position to the focal agent
  h1 <- h1[eff, on = .(tier, composition), nomatch = 0]
  h1[composition == "empty", estimate := 0.5]
  h1_util <- sum(h1$estimate)
  h1_util_exp <- exp(h1_util)
  
  # Proportional choosing to stay in current position
  prob_stay <- h0_util_exp / (h0_util_exp + h1_util_exp)
  
  # Choose
  choose <- sample(c("stay", "move"), size = 1, prob = c(prob_stay, 1 - prob_stay))
  return(choose)
}