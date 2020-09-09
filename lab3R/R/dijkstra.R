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
