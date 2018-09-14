#'Function to run dijkstra algorithm
#'@param graph as data.frame
#'@param init_node as numeric
#'@return vector
#'@export
#'@examples dijkstra(graph = data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), init_node = 1)
#'@references <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm>
dijkstra <- function(graph, init_node){

  unvisitedset<-data.frame(node=c(0),dist=c(0),prev=c(0))

  newdist=c(length(unique(graph$v1)))

  for(i in 1:length(unique(graph$v1)))
  {
    unvisitedset[i,]<-c(i,Inf,NA)
  }

  unvisitedset[unvisitedset$node==init_node,] <- c(init_node,0,init_node) #distance of initial node from initial node

 while(nrow(unvisitedset)>0){

    minNode <- which(unvisitedset$dist==min(unvisitedset$dist))[1]
    minNodePrev <-unvisitedset[minNode,]$prev #get row index for minimum distance
    minNodeDist <-unvisitedset[minNode,]$dist

    #get neighbour nodes
    neighbournodes <- graph[which(graph$v1==unvisitedset[minNode,]$node),]
    newdist[unvisitedset[minNode,]$node]<-minNodeDist
    unvisitedset<-unvisitedset[-c(minNode),] #remove

    for(j in 1:nrow(neighbournodes)){
      if(is.infinite(minNodeDist) || nrow(unvisitedset[unvisitedset$node==neighbournodes$v2[j],])>0)
      {
         alt<- minNodeDist + neighbournodes$w[j]
        if(alt<unvisitedset[unvisitedset$node==neighbournodes$v2[j],]$dist){
          unvisitedset[unvisitedset$node==neighbournodes$v2[j],]<-c(neighbournodes$v2[j],alt,minNodePrev)

        }
      }

    }
 }
  return(newdist)
}
