#' Implementation of dijkstra algorithm to find the shortest distance.
#' @param graph A dataframe with 3 variables v1, v2 and w ie edges of the graph from v1 to v2 with weight w.
#' @param init_node A number which is considered to be the initial point.
#' @description This algorithm caculates the shortest distance between initial node and all the other nodes and saves the result into a vector.
#' @return The shortest distance from \code{init_node} to all other nodes in the graph.
#' @references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @references https://en.wikipedia.org/wiki/Graph#Mathematics
#' @export

dijkstra <- function(graph,init_node){
  stopifnot(is.data.frame(graph),length(graph)==3,names(graph[1])=="v1",names(graph[2])=="v2",names(graph[3])=="w", length(init_node)==1,is.numeric(init_node))
  newgraph=cppRouting::makegraph(graph)
  visited=vector()
  j=1
  res=vector()
  to=unlist(graph[1])
  for (i in 1:length(to)){
    if((to[i]%in%visited)==FALSE){
      res[j]=suppressMessages(cppRouting::get_distance_pair(newgraph,init_node,to[i],algorithm = "bi" ))
      j=j+1
      visited=append(visited,to[i])
    }
  }
  return(res)
}
