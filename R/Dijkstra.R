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