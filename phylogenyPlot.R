# Phylogeny with tipcolours plus heatmap of year of isolation

library(ggtree)
library(ggplot2)
library(viridis)

tree=read.tree("RAxML_Tri_all.bipartitions")
metadata=read.csv("metadata.csv",header=T)
df2=as.data.frame(metadata[,8]) # selects 8th column which contains year of isolation
rownames(df2)=metadata[,1]

p=ggtree(tree)
p=p %<+% metadata
p1=p+geom_tippoint(mapping=aes(colour=Identification),size=3,alpha=.75)
gheatmap(p1,df2,offset=.005,width=.1,colnames=FALSE)
scale_fill_viridis()
