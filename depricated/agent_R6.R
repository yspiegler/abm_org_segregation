library(R6)
library(devtools)

agent_factory <- R6Class(
  "Agent",
  private = list(
    type = "",          # agent type - nominally A or B
    tier = "",          # agent tier - t1 (manager), t2 (peer-level), t3 (subordinate-level)
    vert_group = NaN,   # vertical group - only for t2 & t3. vg1 t1 will have (say) 3 vg1 t3 subordinates etc.
    preferences = c(),  # preferences - this predetermined by tier and type
    n_stayed = 0,       # number of times the agents 'chose' not to move in a row
    org_name = NULL       # name of the organization / neighborhood
  ),
  
  public = list(
    # object initializing method
    initialize = function(type, tier, preferences = NA) {
      private$type <- type
      private$tier <- tier
      if(!is.na(preferences)) {
        private$preferences <- preferences
      }
    },
    # set attributes
    set_vertgroup <- function(x) {
      private$vert_group <- x
    },
    
    set_orgname <- function(x) {
      private$org_name <- x
    },
    
    #get attributes
    get_vertgroup = function() {
      return(private$vert_group)
    },
    
    get_orgname = function() {
      return(private$org_name)
    },
    
    move = function(x) {
      
    },
    
    get_choice = function(x) {
      
    }
  )
  
)
