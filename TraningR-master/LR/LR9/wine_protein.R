# library(rattle)
library(cluster)

# Kmeans -----------------------------------------------------------------------

# data(wine, package='rattle')
# wine
# saveRDS(wine, "Data/wine.RData")

wine <- readRDS("Data/wine.RData")

wine.stand <- scale(wine[-1])  # To standardize the variables

# K-Means
k.means.fit <- kmeans(wine.stand, 3) # k = 3
# attributes(k.means.fit)

# Centroids:
k.means.fit$centers

# Clusters:
k.means.fit$cluster

# Cluster size:
k.means.fit$size


wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- (nrow(data) - 1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)
    }
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters",
       ylab = "Within groups sum of squares")
  }

wssplot(wine.stand, nc = 10) 

########
# Task #
########

# Исходя из графика, какое оптимальное количество кластеров и почему?


clusplot(wine.stand, k.means.fit$cluster, 
         main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE, labels=2, lines=0)

table(wine[, 1], k.means.fit$cluster)

########
# Task #
########

# О чём говорят данные таблицы?


# Hierarchical clustering ------------------------------------------------------

d <- dist(wine.stand, method = "euclidean") # Euclidean distance matrix.

H.fit <- hclust(d, method = "ward.D2")

plot(H.fit) # display dendogram

groups <- cutree(H.fit, k = 3) # cut tree into 3 clusters

# draw dendogram with red borders around the 3 clusters
rect.hclust(H.fit, k = 3, border = "red") 

table(wine[, 1], groups)



# Case 1------------------------------------------------------------------------

food <- read.csv("Data/protein.csv")

set.seed(123456789) # to fix
grpMeat <- kmeans(food[ , c("WhiteMeat", "RedMeat")], centers = 3, nstart = 10)
grpMeat

# list of cluster assignments
o <- order(grpMeat$cluster)
data_frame(food$Country[o], grpMeat$cluster[o])

# Makes plot
plot(food$RedMeat, food$WhiteMeat, type = "n", xlim = c(3, 19), 
     xlab = "Red Meat", ylab = "White Meat")
text(x = food$RedMeat, y = food$WhiteMeat, 
     labels = food$Country, col = grpMeat$cluster + 1)

# Same analysis, but now with clustering on all protein groups 
# change the number of clusters to 7
set.seed(123456789) 
grpProtein <- kmeans(food[ , -1], centers = 7, nstart = 10)
o <- order(grpProtein$cluster)
data_frame(food$Country[o], grpProtein$cluster[o])

clusplot(food[ , -1], grpProtein$cluster, 
         main = '2D representation of the Cluster solution',
         color = TRUE, shade = TRUE,
         labels = 2, lines = 0)

# foodagg <- agnes(food, diss = FALSE, metric = "euclidian")
# plot(foodagg, main = "Dendrogram")


groups <- cutree(foodagg, k = 4) # cut tree into 3 clusters
rect.hclust(foodagg, k = 4, border = "red")







# 
# 
# 
# 
# # Case 2--------------------------------------------------------
# 
# offers <- read.xlsx("data/clustering-vanilla.xls", sheetIndex = 1, header = TRUE)
# transactions <- read.xlsx("data/clustering-vanilla.xls", sheetIndex = 2, header = TRUE)
# 
# # Create transaction matrix
# pivot <- melt(transactions[1:2])
# pivot <- dcast(pivot, value ~ Customer.Last.Name, value.var = "Customer.Last.Name", fun.aggregate = length) %>% tbl_df()
# pivot <- cbind(offers, pivot[-1])
# 
# write.csv(pivot, "data/pivot.csv") # to save your data
# 
# cluster.data <- pivot[ , -(1:7)] %>% t()
# head(cluster.data)
# 
# d <- daisy(cluster.data, metric = "gower")
# H.fit <- hclust(d, method = "ward")
# plot(H.fit) # displays dendrogram
# 
# groups <- cutree(H.fit, k = 4) # cut tree into 4 clusters
# rect.hclust(H.fit, k = 4, border = "red")
# 
# # 2D representation of the Segmentation
# clusplot(cluster.data, groups, 
#          main = 'Customer segments',
#          color = TRUE, shade = TRUE,
#          labels = 2, lines = 0)
# 
# # Merge Data
# cluster.deals <- merge(transactions[1:2], groups, by.x = "Customer.Last.Name", by.y = "row.names")
# colnames(cluster.deals) <- c("Name", "Offer", "Cluster")
# head(cluster.deals)
# 
# # Get top deals by clusters
# cluster.pivot <- melt(cluster.deals, id = c("Offer", "Cluster"))
# cluster.pivot <- dcast(cluster.pivot, Offer ~ Cluster, value.var = "Cluster", fun.aggregate = length) %>% tbl_df()
# cluster.topDeals <- cbind(offers, cluster.pivot[-1])
# head(cluster.topDeals)
# 
# # Export the data
# write.csv(cluster.topDeals, "data/topdeals.csv", row.names = FALSE)
# 
# # Case 3--------------------------------------------------------
# 
# teens <- read.csv("data/snsdata.csv")
# 
# head(teens)
# dim(teens)
# str(teens)
# summary(teens$age)
# 
# teens %<>% na.omit()
# dim(teens)
# 
# interests <- teens[-(1:4)]
# interests_z <- as.data.frame(lapply(interests, scale))
# 
# teen_clusters <- kmeans(interests_z, 5)
# teen_clusters$size
# teen_clusters$centers
# 
# # Make pie charts
# par(mfrow = c(2, 2))
# pie(colSums(interests[teen_clusters$cluster == 1, ]), cex = 0.5)
# pie(colSums(interests[teen_clusters$cluster == 2, ]), cex = 0.5)
# pie(colSums(interests[teen_clusters$cluster == 3, ]), cex = 0.5)
# pie(colSums(interests[teen_clusters$cluster == 4, ]), cex = 0.5)
# 
# # It useful to offer customers products on their interests
