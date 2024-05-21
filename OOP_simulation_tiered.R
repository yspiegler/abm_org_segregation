library(R6)
library(tidyverse)

# Define the Agent class
Agent <- R6Class(
  "Agent",
  #PRIVATE ATTRIBUTES + FUNCTIONS (no access from outside of the object)
  private = list(
      id = NULL,
      type = NULL,          # agent type - nominally A or B
      tier = NULL,          # agent tier - t1 (manager), t2 (peer-level), t3 (subordinate-level)
      vert_group = NULL,    # vertical group - only for t2 & t3. vg1 t1 will have (say) 3 vg1 t3 subordinates etc.
      organization = NULL,  # name of the organization / neighborhood
      preference = NULL,    # preferences - this predetermined by tier and type
      move_count = 0,       # number of times the agent moved in a row
      has_stopped_looking = FALSE, # had this agent stopped looking for work
      
      # MAKE AN INTERNAL MOVE 
      move_to = function(new_organization, new_vert_group) {
        private$vert_group <- new_vert_group
        private$organization <- new_organization
      }
  ),
  #PUBLIC ATTRIBUTES + FUNCTIONS
  public = list(
      # INSTANTIATING A NEW AGENT
      initialize = function(id, type, tier, organization, preferences = list()) {
        private$id <- id
        private$type <- type
        private$tier <- tier
        private$organization <- organization
        #I am *hard coding* preferences here. Bad? maybe. But lets leave this for now.
        if(is_empty(preferences)) {
          private$preference <- case_when(type == "A" & tier == 1 ~ 999,
                                          type == "A" & tier == 2 ~ 999,
                                          type == "A" & tier == 3 ~ 999,
                                          type == "B" & tier == 1 ~ 999,
                                          type == "B" & tier == 2 ~ 999,
                                          type == "B" & tier == 3 ~ 999)  
        } else {
          private$preferences <- preferences
        }
      },
      
      # DECIDE IF AGENT MOVES TO NEW LOCATION
      evaluate_move = function(open_position) {
        # return FALSE is agent stopped looking for work
        if (private$has_stopped_looking) {
          return(FALSE)
        }
        # Evaluate the preference to move to the new position
        
        
        
        if (new_preference > self$preference) {
          self$move_count <- 0
          return(TRUE)
        } else {
          self$move_count <- self$move_count + 1
          if (self$move_count >= 3) {
            self$has_stopped_looking <- TRUE
          }
          return(FALSE)
        }
      },
      
      # BROADCAST IF AGENT STOPPED MOVING
      is_stopped_looking <- function() {
        return(private$has_stopped_looking)
      }
  )
)

# Define the Organization class
Organization <- R6Class(
  "Organization",
  public = list(
    id = NULL,
    agents = list(),
    
    initialize = function(id) {
      self$id <- id
      self$agents <- list(t1 = NULL, t2 = vector("list", 3), t3 = vector("list", 9))
    },
    
    add_agent = function(agent, tier) {
      if (tier == 1) {
        self$agents$t1 <- agent
      } else if (tier == 2) {
        for (i in 1:3) {
          if (is.null(self$agents$t2[[i]])) {
            self$agents$t2[[i]] <- agent
            break
          }
        }
      } else if (tier == 3) {
        for (i in 1:9) {
          if (is.null(self$agents$t3[[i]])) {
            self$agents$t3[[i]] <- agent
            break
          }
        }
      }
    },
    
    get_open_positions = function(tier) {
      open_positions <- list()
      if (tier == 1 && is.null(self$agents$t1)) {
        open_positions <- c(open_positions, list(self$id))
      } else if (tier == 2) {
        for (i in 1:3) {
          if (is.null(self$agents$t2[[i]])) {
            open_positions <- c(open_positions, list(self$id))
          }
        }
      } else if (tier == 3) {
        for (i in 1:9) {
          if (is.null(self$agents$t3[[i]])) {
            open_positions <- c(open_positions, list(self$id))
          }
        }
      }
      return(open_positions)
    }
  )
)

LaborMarket <- R6Class(
  "LaborMarket",
  private = list(n_orgs = NULL,
                 prop_agent_a = 0.6,
                 prop_agent_b = 0.4,
                 prop_empty_jobs = 0.05,
                 organizations = list(),
                 segregation_total = NULL,
                 segregation_t1 = NULL,
                 segregation_t2 = NULL,
                 segregation_t3 = NULL),
  public = list(calc_segregation = function(tierma))
)


# Initialize organizations
organizations <- lapply(1:10, function(id) Organization$new(id))

# Initialize agents and place them in organizations
agents <- list()
for (i in 1:10) {
  agent <- Agent$new(id = i, type = sample(c("A", "B"), 1), tier = sample(1:3, 1), organization = organizations[[i]])
  agents <- c(agents, list(agent))
  organizations[[i]]$add_agent(agent, agent$tier)
}

# Example iteration of the model
for (agent in agents) {
  if (!agent$has_stopped_looking) {
    current_tier <- agent$tier
    open_positions <- list()
    for (org in organizations) {
      open_positions <- c(open_positions, org$get_open_positions(current_tier))
    }
    if (length(open_positions) > 0) {
      new_org_id <- sample(open_positions, 1)
      new_org <- organizations[[new_org_id]]
      if (agent$evaluate_move(new_org, current_tier)) {
        agent$organization <- new_org
      }
    }
  }
}
