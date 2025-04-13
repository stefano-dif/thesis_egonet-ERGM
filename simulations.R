list_to_igraph <- function(l){
  res <- list()
  for (i in 1:length(l)){
    res[[i]] <- asIgraph(l[[i]])
  } 
  return(res)
}

list_to_network <- function(l){
  res <- list()
  for (i in 1:length(l)){
    res[[i]] <- asNetwork(l[[i]])
  } 
  return(res)
}

list_extract_largestcomponent <- function(l){
  #list of igraph objects
  res <- list()
  for (i in 1:length(l)){
    res[[i]] <- largest_component(l[[i]])
  } 
  return(res)
}

n_nodes <- 10000
num_networks <- 10
max_nodes <- 1777*(1777-1)/2

# COWORKERS SIMULATIONS

meandeg_cow <- as.numeric(as.numeric(names(degreedist(egor_IT_cow))) %*% degreedist(egor_IT_cow))

er_list_cow <- list()

for (i in 1:num_networks) {
  net <- sample_gnp(n = n_nodes, p = (meandeg_cow/(10000)))
  net <- asNetwork(net)
  
  er_list_cow[[i]] <- net
}

ba_list_cow <- list()

for (i in 1:num_networks) {
  net <- sample_pa(n = n_nodes, m = meandeg_cow/2, directed = FALSE)
  net <- asNetwork(net)
  
  ba_list_cow[[i]] <- net
}

ergm2_list_cow <- list()

for (i in 1:num_networks) {
  net <- simulate(m2_88_IT_cow,
                  nsim = 1, popsize = n_nodes,
                  seed = 1234, verbose = TRUE)
  
  ergm2_list_cow[[i]] <- net
}

er_list_cow_ig <- list_to_igraph(er_list_cow)
ba_list_cow_ig <- list_to_igraph(ba_list_cow)
ergm2_list_cow_ig <- list_to_igraph(ergm2_list_cow)

er_list_cow_ig_lc <- list_extract_largestcomponent(er_list_cow_ig)
ba_list_cow_ig_lc <- list_extract_largestcomponent(ba_list_cow_ig)
ergm2_list_cow_ig_lc <- list_extract_largestcomponent(ergm2_list_cow_ig)

er_list_cow_lc <- list_to_network(er_list_cow_ig_lc)
ba_list_cow_lc <- list_to_network(ba_list_cow_ig_lc)
ergm2_list_cow_lc <- list_to_network(ergm2_list_cow_ig_lc)

# FRIENDS SIMULATIONS

meandeg_fr <- as.numeric(as.numeric(names(degreedist(egor_IT_fr))) %*% degreedist(egor_IT_fr))

er_list_fr <- list()

for (i in 1:num_networks) {
  net <- sample_gnp(n = n_nodes, p = (meandeg_fr/(10000)))
  net <- asNetwork(net)
  
  er_list_fr[[i]] <- net
}

ba_list_fr <- list()

for (i in 1:num_networks) {
  net <- sample_pa(n = n_nodes, m = meandeg_fr/2, directed = FALSE)
  net <- asNetwork(net)
  
  ba_list_fr[[i]] <- net
}

ergm2_list_fr <- list()

for (i in 1:num_networks) {
  net <- simulate(m2_108_IT_fr,
                  nsim = 1, popsize = n_nodes,
                  seed = 1234, verbose = TRUE)
  
  ergm2_list_fr[[i]] <- net
}

er_list_fr_ig <- list_to_igraph(er_list_fr)
ba_list_fr_ig <- list_to_igraph(ba_list_fr)
ergm2_list_fr_ig <- list_to_igraph(ergm2_list_fr)

er_list_fr_ig_lc <- list_extract_largestcomponent(er_list_fr_ig)
ba_list_fr_ig_lc <- list_extract_largestcomponent(ba_list_fr_ig)
ergm2_list_fr_ig_lc <- list_extract_largestcomponent(ergm2_list_fr_ig)

er_list_fr_lc <- list_to_network(er_list_fr_ig_lc)
ba_list_fr_lc <- list_to_network(ba_list_fr_ig_lc)
ergm2_list_fr_lc <- list_to_network(ergm2_list_fr_ig_lc)