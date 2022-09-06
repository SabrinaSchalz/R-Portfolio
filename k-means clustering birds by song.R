#Networking Mapping

library(tidyverse)
library(igraph)
library(ggraph)
library(RColorBrewer)

#define data lists

setwd("C:/Users/slyth/Documents/Uni/R")
Edge<-read.table("Edge Bird Song Mapping.csv",header=TRUE,sep=";")
Node<-read.table("Node Bird Song Mapping Megacluster.csv",header=TRUE,sep=";")
edge_list <- tibble(Edge)
nodes <- tibble(Node)

edges <- edge_list %>% 
  left_join(nodes, by = c("ï..From" = "Label")) %>% 
  rename(from = ï..ID)

edges <- edges %>% 
  left_join(nodes, by = c("To" = "Label")) %>% 
  rename(to = ï..ID)

edges <- select(edges, from, to)

edges<-na.omit(edges)

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T)
#all clusters
pal<- brewer.pal(8, "Spectral")
plot(net, mark.groups=list(c(48,49,78,79), c(4,20,21,23,30,34,41,47,61,69,81,85,87,88,90), c(2,42,43,68,93), c(22,66,83,86), c(1,12,13,14,31,32,36,37,38,39,40,57,62,67,72),
c(3,5:11,15:19,24:29,33,35,44,45,46,50:56,58,59,60,63,64,65,70,71,73:77,80,82,84,89,91,92,94)),
mark.col=pal, mark.border=NA, vertex.size = 5, vertex.label="", vertex.color="gray", edge.color="black", edge.arrow.size=0.3)

legend(x=-1.7, y=1.3, c("Peru","Yucatan", "North America", "Kalahari", "Australia", "?"), pch=21,
       col=pal, pt.cex=0, cex=.8, bty="y", ncol=1, fill=pal)

#single cluster
plot(net, layout=layout_with_kk, layout=a*1, vertex.shape="none", vertex.label=nodes$Label, edge.color="black", edge.arrow.size=0.3)


ggraph(net, layout = 'kk') +
  geom_edge_link(aes(start_cap = label_rect(node1.name),
                       end_cap = label_rect(node2.name)),arrow = arrow(length = unit(2, 'mm')),end_cap = circle(3, 'mm'),color="orange") + 
  geom_node_point(size=1, color="orange") +
geom_node_text(aes(label = nodes$Label), size=2.3, color="black") +
 theme_void() 
