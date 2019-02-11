votes <- read.csv("~/Desktop/GRADUATE/5703/100980213-YC-ASSIGNMENT2/Data/votes.csv")
install.packages("factoextra")
install.packages("cluster")
library(factoextra)
library(cluster)
#check missing value
any(is.na(votes)) #no missing value
#delete fips1:3,13,17,18,23 and state name 11,12,24,25
votes.use<-votes[,-c(1:3,11:13,17,18,23:25)]

#to account for different scales of measurement, standardize
f.data.std<-function(data) {
  data<-as.matrix(data)
  bar<-apply(data,2,mean)
  s<-apply(data,2,sd)
  t((t(data)-bar)/s)
}
votes.use.s<-f.data.std(votes.use)

#hierarchical clustering
sink("output A2Q2.txt")
hc1<-agnes(votes.use.s,method="complete")
(hc1$ac)
hc2<-agnes(votes.use.s,method="average")
(hc2$ac)
hc3<-agnes(votes.use.s,method="single")
(hc3$ac)
hc4<-agnes(votes.use.s,method="ward")
(hc4$ac)
#agglomerative coefficient, measures the amount of clustering structure found
#values closer to 1 suggest strong clustering structure)
#ward strongest clutsering structures
jpeg('dendrogram of agnes.jpeg')
pltree(hc4,cex=0.6,hang=-1,main="Dendrogram of agnes")
dev.off()

#how many counties are in each of the groups
#2 clusters
groups.2<-cutree(hc4,2)
(table(groups.2))
#3 clusters
groups.3<-cutree(hc4,3)
(table(groups.3))
#4 clusters
groups.4<-cutree(hc4,4)
(table(groups.4))
#we'd like a solution where there aren't too many clusters with just a few
#observations
counts<-sapply(2:20,function(ncl)table(cutree(hc4,ncl)))
names(counts)<-2:20
(counts)
#2&3 cluster better

jpeg('votes cluster2.jpeg')
fviz_cluster(list(data=votes.use.s,cluster=groups.2))
dev.off()
jpeg('votes cluster3.jpeg')
fviz_cluster(list(data=votes.use.s,cluster=groups.3))
dev.off()




#determine optimal clusters
jpeg('silhouette.jpg')
fviz_nbclust(votes.use.s,FUN=hcut,method="silhouette")
dev.off()
#2 cluster better

#see which couties are in which clusters
(sapply(unique(groups.2),function(g)votes$county_name[groups.2==g]))
#see cluster and state
(table(groups.2,votes$state_abbr))
sink()
sink("median cluster2.output.txt")
print(aggregate(votes.use.s,list(groups.2),median))
sink()

