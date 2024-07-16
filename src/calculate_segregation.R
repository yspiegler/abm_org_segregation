library(tidyverse)



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

# Calculate H Theil index of segregation (based on Entropy) [v]
### NOTE: the values seem low, but Theil isn't an easily interpretable scale. But for context, 
###       in Ferguson & Koenig (2018), the overall Theil score of between-firm segregation is around 0.17
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
