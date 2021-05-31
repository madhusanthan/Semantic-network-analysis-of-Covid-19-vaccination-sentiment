library(dplyr)
library(igraph)
library(tibble)

#topic 1

nodes =  read.csv(file = 'topic 1 nodes.csv', na.strings=c("","NA"))
per_stem = read.csv(file = 'edges word stem topic1 1.csv', na.strings=c("","NA"))


colnames(per_stem) = c("stem1", "stem2")

per_stem <- per_stem %>%  
  group_by(stem1, stem2) %>%
  summarise(weight = n()) %>% 
  ungroup()
View(per_stem)

nodes <- nodes %>% rowid_to_column("id")
View(nodes)

edges <- per_stem %>% 
  left_join(nodes, by = c("stem1" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("stem2" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)

network <- graph_from_data_frame(d = edges, vertices = nodes, directed = F)
network <- simplify(network, remove.multiple=F, remove.loops=T)

V(network)$size <- edges$weight*0.7
E(network)$color <- as.factor(edges$to)

# Make the plot
plot(network, 
     vertex.label= V(network)$label, 
     vertex.color = '#a2d67e',
     vertex.label.color = "black",
     vertex.frame.color= "black",
     vertex.label.dist=0,
     vertex.label.cex = 2.5,
     vertex.label.font = 2,
     edge.width = 0.5,
     edge.color = "grey",
     layout = layout_nicely)

#degree centrality
degree.cent <- degree(network, mode = "all", loops=FALSE, v=V(network))
degree.cent

#betweenness
betweenness.cent <- betweenness(network, v=V(network), directed=FALSE)
betweenness.cent

#closeness
closeness.cent <- closeness(network, mode="all", vids = V(network))
closeness = data.frame(closeness.cent)
closeness.cent

#eigenvector centrality
eigen.cent <- eigen_centrality(network, directed=FALSE)
eigen = data.frame(eigen.cent$vector)
eigen.cent
#size
gsize(network)

#density
edge_density(network, loops = FALSE)

#diameter
diameter(network, directed = FALSE)

