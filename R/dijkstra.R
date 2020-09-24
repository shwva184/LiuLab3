#' Implementation of dijkstra algorithm to find the shortest distance.
#' @param graph A dataframe with 3 variables v1, v2 and w ie edges of the graph from v1 to v2 with weight w.
#' @param init_node A number which is considered to be the initial point.
#' @description This algorithm calculates the shortest distance between initial node and all the other nodes and saves the result into a vector.
#' @return The shortest distance from \code{init_node} to all other nodes in the graph.
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @references https://en.wikipedia.org/wiki/Graph#Mathematics
#' @export

dijkstra=function(graph, init_node)
{
  stopifnot(is.data.frame(graph),length(graph)==3,names(graph[1])=="v1",
            names(graph[2])=="v2",names(graph[3])=="w", length(init_node)==1,
            is.numeric(init_node),
            init_node%in%c(graph$v1,graph$v2))
  queue=vector()
  distance=vector()
  v1=graph$v1
  v2=graph$v2
  N=unique(c(v1, v2))
  distance[N]=Inf
  queue[N]=N
  distance[init_node]=0
  while (!all(is.na(queue))) {
    current_distance=queue[which.min(distance[queue])]
    queue=queue[!(queue %in% current_distance)]
    neighbour=v2[v1 == current_distance]
    for (v in neighbour) {
      w=graph[v1 == current_distance & v2 == v,"w"]
      new_distance=distance[current_distance] + w
      
      if (new_distance < distance[v]) {
        distance[v]=new_distance
      }
    }
  } 
  return(distance)
}
